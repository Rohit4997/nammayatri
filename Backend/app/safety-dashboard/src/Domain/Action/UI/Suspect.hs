{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Suspect where

import API.Types.UI.Suspect
import qualified API.Types.UI.Suspect
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import Data.OpenApi (ToSchema)
import Data.Text as T hiding (concat, elem, filter, length, map)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Notification as Domain.Types.Notification
import Domain.Types.Suspect as Domain.Types.Suspect
import Domain.Types.SuspectFlagRequest as Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectFlagRequest as Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusChangeRequest as Domain.Types.SuspectStatusChangeRequest
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (concatMap, elem, filter, id, length, map, mapM_, readMaybe, whenJust)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified Storage.Queries.Notification as SQN
import "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.PortalConfigs as PC
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import qualified Storage.Queries.Suspect as SQ
import Storage.Queries.SuspectExtra
import qualified Storage.Queries.SuspectFlagRequest as SQF
import Storage.Queries.SuspectFlagRequestExtra
import qualified Storage.Queries.SuspectStatusChangeRequest as SQSS
import "lib-dashboard" Tools.Auth
import Tools.Error
import "lib-dashboard" Tools.Error

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo request =
  T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo) request

data ChangeFlagNotificationMetadata = ChangeFlagNotificationMetadata
  { suspect :: Domain.Types.Suspect.Suspect,
    reasonToChange :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

postUploadSuspectBulk :: TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.Flow API.Types.UI.Suspect.SuspectBulkUploadResp
postUploadSuspectBulk tokenInfo req = do
  transaction <- buildTransaction Safety.UploadBulkSuspectEndpoint tokenInfo (encodeToText req)
  portalConfig <- PC.findByConfigName "BULK_UPLOAD_COUNT"
  let bulkUploadCount = case portalConfig of
        Just config -> read (T.unpack config.value) :: Int
        Nothing -> 100
  T.withTransactionStoring transaction $ do
    when (length req.suspects > bulkUploadCount) $ throwError (BulkUploadLimitExceeded bulkUploadCount)
    let validSuspects = filter (\suspect -> isJust (suspect.dl) || isJust (suspect.voterId)) req.suspects
    let dlList = mapMaybe (\suspect -> suspect.dl) $ validSuspects
    let voterIdList = mapMaybe (\suspect -> suspect.voterId) $ validSuspects
    suspectAlreadyFlagged <- findAllByDlAndVoterIdAndMerchantId dlList voterIdList (Just tokenInfo.merchantId)
    let dlListForAlreadyFlagged = mapMaybe (\suspect -> suspect.dl) suspectAlreadyFlagged
    let voterIdListForAlreadyFlagged = mapMaybe (\suspect -> suspect.voterId) suspectAlreadyFlagged
    suspectCleanedByAdmin <- findAllByDlOrVoterIdAndFlaggedStatus dlListForAlreadyFlagged voterIdListForAlreadyFlagged Domain.Types.Suspect.Clean
    let suspectNeedToFlagAgain = filter (\suspect -> (suspect.dl, suspect.voterId) `elem` (map (\suspect' -> (suspect'.dl, suspect'.voterId)) suspectCleanedByAdmin)) validSuspects
    let suspectNotFlagged = filter (\suspect -> not ((suspect.dl, suspect.voterId) `elem` (map (\suspect' -> (suspect'.dl, suspect'.voterId)) suspectAlreadyFlagged))) validSuspects
    let suspectsNeedToFlag = suspectNotFlagged <> suspectNeedToFlagAgain
    when (length suspectsNeedToFlag > 0) $ do
      person <- findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
      merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
      let flaggedBy = person.firstName <> " " <> person.lastName
      suspectFlagRequest <- mapM (\suspect -> buildSuspectFlagRequest suspect tokenInfo flaggedBy Domain.Types.SuspectFlagRequest.Pending) suspectsNeedToFlag
      SQF.createMany suspectFlagRequest
      let notificationMetadata = encodeToText $ suspectFlagRequest
      adminRole <- QRole.findByDashboardAccessType MERCHANT_ADMIN >>= fromMaybeM (RoleDoesNotExist "MERCHANT_ADMIN")
      allMerchantUsers <- QMerchantAccess.findAllUserAccountForMerchant merchant.id
      let personIds = map (\merchantUser -> merchantUser.personId) allMerchantUsers
      merchantAdminList <- QP.findAllByIdAndRoleId personIds adminRole.id
      mapM_
        ( \receiver -> do
            notification <- buildNotification tokenInfo.merchantId merchant.shortId.getShortId (length suspectsNeedToFlag) Domain.Types.Notification.FLAG_REQUEST_UPLOAD notificationMetadata receiver.id.getId tokenInfo.personId.getId
            SQN.create notification
        )
        merchantAdminList
    let message = case length suspectsNeedToFlag of
          0 -> "All DLs or VoterIds are already flagged before"
          _ -> "Dl and VoterId List provided who were already Flagged before."
    return $ SuspectBulkUploadResp {dlList = map (\suspect -> suspect.dl) suspectAlreadyFlagged, voterIdList = map (\suspect -> suspect.voterId) suspectAlreadyFlagged, message = message}

postChangeFlag :: TokenInfo -> API.Types.UI.Suspect.SuspectFlagStatusChangeReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postChangeFlag tokenInfo req = do
  transaction <- buildTransaction Safety.ChangeFlagRequestEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    suspect <- case (req.dl, req.voterId) of
      (Nothing, Nothing) -> throwError SuspectDlOrVoterIdRequired
      (Just _, Nothing) -> SQ.findByDl req.dl >>= fromMaybeM SuspectNotFound
      _ -> SQ.findByVoterId req.voterId >>= fromMaybeM SuspectNotFound
    alreadyRequested <- SQSS.findBySuspectId suspect.id.getId
    whenJust alreadyRequested $ \request -> do
      if request.reqStatus == Domain.Types.SuspectFlagRequest.Pending
        then throwError RequestAlreadyExists
        else throwError FlagRequestAlreadyProcessed
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    suspectChangeRequest <- buildSuspectStatusChangeRequest suspect merchant req.reasonToChange
    SQSS.create suspectChangeRequest
    let notificationMetadata = encodeToText $ [ChangeFlagNotificationMetadata {suspect = suspect, reasonToChange = req.reasonToChange}]
    adminRole <- QRole.findByDashboardAccessType DASHBOARD_ADMIN >>= fromMaybeM (RoleDoesNotExist "DASHBOARD_ADMIN")
    receiversList <- QP.findAllByRole adminRole.id
    mapM_
      ( \receiver -> do
          notification <- buildNotification tokenInfo.merchantId merchant.shortId.getShortId 1 Domain.Types.Notification.CHANGE_REQUEST_PARTNER_ADMIN notificationMetadata receiver.id.getId tokenInfo.personId.getId
          SQN.create notification
      )
      receiversList
    return Kernel.Types.APISuccess.Success

buildSuspectFlagRequest :: API.Types.UI.Suspect.SuspectUploadReq -> TokenInfo -> Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> Environment.Flow Domain.Types.SuspectFlagRequest.SuspectFlagRequest
buildSuspectFlagRequest req tokenInfo name approval = do
  uid <- generateGUID
  now <- getCurrentTime
  return $
    Domain.Types.SuspectFlagRequest.SuspectFlagRequest
      { id = Id uid,
        dl = req.dl,
        voterId = req.voterId,
        adminApproval = approval,
        flaggedStatus = Domain.Types.Suspect.Flagged,
        flaggedReason = req.flaggedReason,
        flaggedBy = name,
        merchantId = Just tokenInfo.merchantId,
        createdAt = now,
        firstName = req.firstName,
        lastName = req.lastName,
        approvedBy = Nothing,
        updatedAt = now,
        flaggedCategory = req.flaggedCategory
      }

buildNotification :: Id Domain.Types.Merchant.Merchant -> Text -> Int -> Domain.Types.Notification.NotificationCategory -> Text -> Text -> Text -> Environment.Flow Domain.Types.Notification.Notification
buildNotification merchantId merchantShortId notificationCount notificationCategory metadata receiverId senderId = do
  uid <- generateGUID
  now <- getCurrentTime
  return $
    Domain.Types.Notification.Notification
      { id = Id uid,
        notificationCategory = notificationCategory,
        readStatus = False,
        notificationCount = notificationCount,
        merchantId = Just merchantId,
        merchantShortId = merchantShortId,
        metadata = metadata,
        receiverId = receiverId,
        senderId = senderId,
        createdAt = now,
        updatedAt = now
      }

buildSuspectStatusChangeRequest :: Domain.Types.Suspect.Suspect -> Domain.Types.Merchant.Merchant -> Text -> Environment.Flow Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest
buildSuspectStatusChangeRequest suspect merchant reasonTochange = do
  uid <- generateGUID
  now <- getCurrentTime

  return $
    Domain.Types.SuspectStatusChangeRequest.SuspectStatusChangeRequest
      { id = Id uid,
        suspectId = suspect.id.getId,
        reasonToChange = reasonTochange,
        merchantId = Just merchant.id,
        reqStatus = Domain.Types.SuspectFlagRequest.Pending,
        merchantShortId = merchant.shortId.getShortId,
        createdAt = now,
        updatedAt = now
      }
