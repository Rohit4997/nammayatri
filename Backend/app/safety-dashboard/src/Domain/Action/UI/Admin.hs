{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Admin where

import API.Types.UI.Admin
import qualified API.Types.UI.Admin
import qualified API.Types.UI.Suspect
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.OpenApi (ToSchema)
import Data.Text as T hiding (concat, elem, filter, length, map)
import qualified Domain.Action.UI.Suspect as DS
import qualified Domain.Action.UI.SuspectFlagRequest as SAF
import Domain.Action.UI.Webhook as Webhook
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Notification as Domain.Types.Notification
import qualified "lib-dashboard" Domain.Types.Person
import qualified Domain.Types.Suspect as Domain.Types.Suspect
import Domain.Types.SuspectFlagRequest as Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusHistory as Domain.Types.SuspectStatusHistory
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (concatMap, elem, filter, id, length, map, mapM_, readMaybe, whenJust)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.MerchantConfigs as SQMC
import qualified Storage.Queries.Notification as SQN
import "lib-dashboard" Storage.Queries.Person as QP
import qualified Storage.Queries.PortalConfigs as PC
import qualified "lib-dashboard" Storage.Queries.RegistrationToken as QReg
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import qualified Storage.Queries.Suspect as SQ
import Storage.Queries.SuspectExtra
import qualified Storage.Queries.SuspectExtra as SE
import qualified Storage.Queries.SuspectFlagRequest as SQF
import Storage.Queries.SuspectFlagRequestExtra
import qualified Storage.Queries.SuspectStatusHistory as SQSH
import "lib-dashboard" Tools.Auth
import qualified "lib-dashboard" Tools.Auth.Common as Auth
import Tools.Error
import "lib-dashboard" Tools.Error

data AdminCleanSuspect = AdminCleanSuspect
  { id :: Text,
    dl :: Maybe Text,
    voterId :: Maybe Text,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    flagUpdatedAt :: UTCTime,
    statusChangedReason :: Maybe Text,
    flaggedCounter :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo request =
  T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo) request

postChangeSuspectFlag :: TokenInfo -> API.Types.UI.Admin.SuspectFlagChangeRequestList -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postChangeSuspectFlag tokenInfo req = do
  transaction <- buildTransaction Safety.ChangeSuspectFlagEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    let idList = map (\id -> Kernel.Types.Id.Id $ id) req.ids
    suspectList <- SE.findAllByIds idList
    updatedSuspectList <- mapM (\suspect -> buildUpdateSuspectRequest req suspect) suspectList
    mapM_ (\updatedSuspect -> SQ.updateByPrimaryKey updatedSuspect) updatedSuspectList
    let dlList = mapMaybe (\suspect -> suspect.dl) $ updatedSuspectList
    let voterIdList = mapMaybe (\suspect -> suspect.voterId) $ updatedSuspectList
    mapM_ (\dl -> updateAllWIthDlAndFlaggedStatus dl req.flaggedStatus) dlList
    mapM_ (\voterId -> updateAllWithVoteIdAndFlaggedStatus voterId req.flaggedStatus) voterIdList
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    let notificationMetadata = encodeToText updatedSuspectList
    adminRole <- QRole.findByDashboardAccessType MERCHANT_ADMIN >>= fromMaybeM (RoleDoesNotExist "MERCHANT_ADMIN")
    receiversList <- QP.findAllByRole adminRole.id
    mapM_
      ( \receiver -> do
          notification <- DS.buildNotification tokenInfo.merchantId merchant.shortId.getShortId (length updatedSuspectList) Domain.Types.Notification.ADMIN_CHANGE_SUSPECT_STATUS notificationMetadata receiver.id.getId tokenInfo.personId.getId
          SQN.create notification
      )
      receiversList
    mapM_
      ( \suspect -> do
          suspectStatusHistory <- buildSuspectStatus tokenInfo merchant.shortId.getShortId suspect
          SQSH.create suspectStatusHistory
      )
      updatedSuspectList
    webhookBody <- buildAdminCleanSuspectWebhookBody updatedSuspectList
    merchantConfigs <- SQMC.findByRequestWebHook True
    fork "Sending webhook to partners" $ do
      Webhook.sendWebHook merchantConfigs webhookBody
    return Kernel.Types.APISuccess.Success

postAdminUploadSuspectBulk :: TokenInfo -> API.Types.UI.Suspect.SuspectBulkUploadReq -> Environment.Flow API.Types.UI.Suspect.SuspectBulkUploadResp
postAdminUploadSuspectBulk tokenInfo req = do
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
      suspectFlagRequest <- mapM (\suspect -> DS.buildSuspectFlagRequest suspect tokenInfo flaggedBy Domain.Types.SuspectFlagRequest.Approved) suspectsNeedToFlag
      SQF.createMany suspectFlagRequest
      suspectList <- mapM (\suspect -> SAF.addOrUpdateSuspect suspect merchant.shortId.getShortId) suspectFlagRequest
      merchantAdminRole <- QRole.findByDashboardAccessType MERCHANT_ADMIN >>= fromMaybeM (RoleDoesNotExist "MERCHANT_ADMIN")
      merchantReceiversList <- QP.findAllByRole merchantAdminRole.id
      let notificationMetaDataFromAdmin = encodeToText suspectList
      mapM_
        ( \receiver -> do
            notification <- DS.buildNotification tokenInfo.merchantId merchant.shortId.getShortId (length suspectFlagRequest) Domain.Types.Notification.PARTNER_FLAGGED_SUSPECT notificationMetaDataFromAdmin receiver.id.getId tokenInfo.personId.getId
            SQN.create notification
        )
        merchantReceiversList
      merchantConfigs <- SQMC.findByRequestWebHook True
      let suspectListForWebhook = map (\flagReq' -> SAF.buildSuspectBody merchant.shortId.getShortId flagReq') suspectFlagRequest
          webhookBody = A.encode $ SAF.WebhookReqBody {suspectList = suspectListForWebhook}
      fork "Sending webhook to partners" $ do
        Webhook.sendWebHook merchantConfigs webhookBody
      mapM_
        ( \flagReq -> do
            suspectStatusHistory <- SAF.buildSuspectStatusHistory tokenInfo merchant.shortId.getShortId Domain.Types.SuspectFlagRequest.Approved flagReq
            SQSH.create suspectStatusHistory
        )
        suspectFlagRequest
    let message = case length suspectsNeedToFlag of
          0 -> "All DLs or VoterIds are already flagged before"
          _ -> "Dl and VoterId List provided who were already Flagged before."
    return $ API.Types.UI.Suspect.SuspectBulkUploadResp {dlList = map (\suspect -> suspect.dl) suspectAlreadyFlagged, voterIdList = map (\suspect -> suspect.voterId) suspectAlreadyFlagged, message = message}

postCheckWebhook :: TokenInfo -> API.Types.UI.Admin.WebhookCheck -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postCheckWebhook _ _ = do
  logDebug $ "Webhook Check Request: checked_ we reached till here"
  return Kernel.Types.APISuccess.Success

deleteMerchantUserDelete :: TokenInfo -> API.Types.UI.Admin.DeleteMerchantUserReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteMerchantUserDelete tokenInfo req = do
  person <- QP.findByEmail req.email >>= fromMaybeM (PersonNotFound req.email)
  QAccess.deleteAllByMerchantIdAndPersonId tokenInfo.merchantId person.id
  Auth.cleanCachedTokensByMerchantId person.id tokenInfo.merchantId
  QReg.deleteAllByPersonIdAndMerchantId person.id tokenInfo.merchantId
  return Kernel.Types.APISuccess.Success

buildUpdateSuspectRequest :: API.Types.UI.Admin.SuspectFlagChangeRequestList -> Domain.Types.Suspect.Suspect -> Environment.Flow Domain.Types.Suspect.Suspect
buildUpdateSuspectRequest req Domain.Types.Suspect.Suspect {..} = do
  now <- getCurrentTime
  newFlaggedCounter <- if req.flaggedStatus == Domain.Types.Suspect.Clean then pure 0 else pure flaggedCounter
  let suspect =
        Domain.Types.Suspect.Suspect
          { flaggedStatus = req.flaggedStatus,
            flagUpdatedAt = now,
            statusChangedReason = req.reasonToChange,
            flaggedCounter = newFlaggedCounter,
            ..
          }
  return suspect

buildAdminCleanSuspectWebhookBody :: [Domain.Types.Suspect.Suspect] -> Environment.Flow LBS.ByteString
buildAdminCleanSuspectWebhookBody suspectList = do
  let adminCleanSuspectList =
        map
          ( \suspect ->
              AdminCleanSuspect
                { id = suspect.id.getId,
                  dl = suspect.dl,
                  voterId = suspect.voterId,
                  flaggedStatus = suspect.flaggedStatus,
                  flagUpdatedAt = suspect.flagUpdatedAt,
                  statusChangedReason = suspect.statusChangedReason,
                  flaggedCounter = suspect.flaggedCounter,
                  createdAt = suspect.createdAt,
                  updatedAt = suspect.updatedAt
                }
          )
          suspectList
  return $ A.encode adminCleanSuspectList

buildSuspectStatus :: TokenInfo -> Text -> Domain.Types.Suspect.Suspect -> Environment.Flow Domain.Types.SuspectStatusHistory.SuspectStatusHistory
buildSuspectStatus tokenInfo merchantShortId Domain.Types.Suspect.Suspect {..} = do
  now <- getCurrentTime
  id_ <- generateGUID
  let suspectStatusHistory =
        Domain.Types.SuspectStatusHistory.SuspectStatusHistory
          { id = id_,
            dl = dl,
            firstName = Just firstName,
            lastName = Just firstName,
            flaggedStatus = flaggedStatus,
            statusChangedReason = statusChangedReason,
            voterId = voterId,
            createdAt = now,
            updatedAt = now,
            merchantId = Just tokenInfo.merchantId,
            merchantShortId = Just merchantShortId,
            flaggedBy = Just flaggedBy,
            adminApproval = Nothing
          }
  return suspectStatusHistory
