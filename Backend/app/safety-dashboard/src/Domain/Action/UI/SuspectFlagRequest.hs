{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SuspectFlagRequest where

import qualified API.Types.UI.Notification as Notification
import API.Types.UI.SuspectFlagRequest
import qualified API.Types.UI.SuspectFlagRequest
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import Data.OpenApi (ToSchema)
import Data.Text as T hiding (concat, length, map)
import qualified Domain.Action.UI.Suspect as DS
import Domain.Action.UI.Webhook as Webhook
import qualified "lib-dashboard" Domain.Types.Merchant
import Domain.Types.MerchantConfigs as MC
import qualified Domain.Types.Notification as Domain.Types.Notification
import qualified "lib-dashboard" Domain.Types.Person
import qualified Domain.Types.Suspect as Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest as Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusHistory as Domain.Types.SuspectStatusHistory
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id, length, map, mapM_, readMaybe)
import Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager, tlsManagerSettings)
import Network.HTTP.Types (HeaderName)
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified "lib-dashboard" Storage.Queries.MerchantAccess as QMerchantAccess
import qualified Storage.Queries.MerchantConfigs as SQMC
import qualified Storage.Queries.Notification as SQN
import "lib-dashboard" Storage.Queries.Person as QP
import qualified "lib-dashboard" Storage.Queries.Role as QRole
import qualified Storage.Queries.Suspect as SQ
import Storage.Queries.SuspectExtra
import qualified Storage.Queries.SuspectFlagRequest as SQF
import Storage.Queries.SuspectFlagRequestExtra as SQFE
import qualified Storage.Queries.SuspectStatusHistory as SQSH
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

newtype WebhookReqBody = WebhookReqBody
  { suspectList :: [SuspectBody]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data SuspectBody = SuspectBody
  { dl :: Maybe Text,
    voterId :: Maybe Text,
    flaggedCategory :: Text,
    flaggedReason :: Text,
    flaggedBy :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getListSuspectsFlag :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.SuspectFlagRequest.AdminApproval -> Environment.Flow API.Types.UI.SuspectFlagRequest.SuspectFlagRequestList
getListSuspectsFlag tokenInfo mblimit mboffset adminApproval = do
  flagReqList <- SQF.findByMerchantIdAndAdminApproval mblimit mboffset (Just tokenInfo.merchantId) adminApproval
  let resp = map mkListFlagResp flagReqList
  let cnt = length resp
  let summary = Notification.Summary {totalCount = 10000, count = cnt}
  return $
    SuspectFlagRequestList
      { flagRequestList = resp,
        summary = summary
      }

mkListFlagResp :: Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> API.Types.UI.SuspectFlagRequest.SuspectFlagRequestResp
mkListFlagResp Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..} = do
  SuspectFlagRequestResp
    { id = id.getId,
      ..
    }

postProcessSuspectFlagRequest :: TokenInfo -> API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postProcessSuspectFlagRequest tokenInfo req = do
  transaction <- buildTransaction Safety.ProcessSuspectFlagRequestListEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    let flagReqIdList = map (\id -> Kernel.Types.Id.Id $ id) req.suspectFlagRequestIdList
    flagReqList <- SQFE.findAllByRequestId flagReqIdList
    let dlList = map (\flagReq -> flagReq.dl) flagReqList
    let voterIdList = map (\flagReq -> flagReq.voterId) flagReqList
    pendingSuspectFlagList <- SQFE.findAllPAByDlAndVoterIdAndMerchantId dlList voterIdList (Just tokenInfo.merchantId)
    let notificationMetadataForMerchant = encodeToText pendingSuspectFlagList
    merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
    case req.adminApproval of
      Domain.Types.SuspectFlagRequest.Approved -> do
        person <- findById tokenInfo.personId >>= fromMaybeM (PersonNotFound tokenInfo.personId.getId)
        let approvedBy = person.firstName <> " " <> person.lastName
        SQFE.updateManyAdminApprovalById req.adminApproval approvedBy flagReqIdList
        merchantAdminRole <- QRole.findByDashboardAccessType MERCHANT_ADMIN >>= fromMaybeM (RoleDoesNotExist "MERCHANT_ADMIN")
        adminRole <- QRole.findByDashboardAccessType DASHBOARD_ADMIN >>= fromMaybeM (RoleDoesNotExist "DASHBOARD_ADMIN")
        merchantReceiversList <- QP.findAllByRole merchantAdminRole.id
        mapM_
          ( \receiver -> do
              notification <- DS.buildNotification tokenInfo.merchantId merchant.shortId.getShortId (length pendingSuspectFlagList) Domain.Types.Notification.FLAG_REQUEST_APPROVED notificationMetadataForMerchant receiver.id.getId tokenInfo.personId.getId
              SQN.create notification
          )
          merchantReceiversList
        suspectList <- mapM (\suspect -> addOrUpdateSuspect suspect merchant.shortId.getShortId) pendingSuspectFlagList
        adminReceiversList <- QP.findAllByRole adminRole.id
        let notificationMetaDataForAdmin = encodeToText suspectList
        mapM_
          ( \receiver -> do
              notification <- DS.buildNotification tokenInfo.merchantId merchant.shortId.getShortId (length pendingSuspectFlagList) Domain.Types.Notification.PARTNER_FLAGGED_SUSPECT notificationMetaDataForAdmin receiver.id.getId tokenInfo.personId.getId
              SQN.create notification
          )
          adminReceiversList
        merchantConfigs <- SQMC.findByRequestWebHook True
        let suspectListForWebhook = map (\flagReq' -> buildSuspectBody merchant.shortId.getShortId flagReq') pendingSuspectFlagList
            webhookBody = A.encode $ WebhookReqBody {suspectList = suspectListForWebhook}
        Webhook.sendWebHook merchantConfigs webhookBody
        mapM_
          ( \flagReq -> do
              suspectStatusHistory <- buildSuspectStatusHistory tokenInfo merchant.shortId.getShortId Domain.Types.SuspectFlagRequest.Approved flagReq
              SQSH.create suspectStatusHistory
          )
          pendingSuspectFlagList
        return Kernel.Types.APISuccess.Success
      Domain.Types.SuspectFlagRequest.Rejected -> do
        allMerchantUsers <- QMerchantAccess.findAllUserAccountForMerchant merchant.id
        mapM_
          ( \receiver -> do
              notification <- DS.buildNotification tokenInfo.merchantId merchant.shortId.getShortId (length pendingSuspectFlagList) Domain.Types.Notification.FLAG_REQUEST_APPROVED notificationMetadataForMerchant receiver.personId.getId tokenInfo.personId.getId
              SQN.create notification
          )
          allMerchantUsers
        mapM_
          ( \flagReq -> do
              SQF.updateAdminApprovalById (req.adminApproval) flagReq.id
          )
          pendingSuspectFlagList
        return Kernel.Types.APISuccess.Success
      _ -> throwError InvalidAdminApproval

addOrUpdateSuspect :: Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Text -> Environment.Flow Domain.Types.Suspect.Suspect
addOrUpdateSuspect req merchantShortId = do
  case req.dl of
    Just _ -> do
      mbSuspect <- SQ.findByDl req.dl
      case mbSuspect of
        Just suspect -> do
          let flaggedBy = Domain.Types.Suspect.FlaggedBy req.flaggedCategory merchantShortId : suspect.flaggedBy
          SQ.updateFlaggedCounterByDl (suspect.flaggedCounter + 1) Domain.Types.Suspect.Flagged flaggedBy req.dl
          pure suspect
        Nothing -> do
          suspect <- buildSuspect merchantShortId req
          SQ.create suspect
          pure suspect
    Nothing -> do
      mbSuspect <- SQ.findByVoterId req.voterId
      case mbSuspect of
        Just suspect -> do
          let flaggedBy = Domain.Types.Suspect.FlaggedBy req.flaggedCategory merchantShortId : suspect.flaggedBy
          SQ.updateFlaggedCounterByVoterId (suspect.flaggedCounter + 1) Domain.Types.Suspect.Flagged flaggedBy req.voterId
          pure suspect
        Nothing -> do
          suspect <- buildSuspect merchantShortId req
          SQ.create suspect
          pure suspect

buildSuspect :: Text -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Environment.Flow Domain.Types.Suspect.Suspect
buildSuspect merchantShortId Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..} = do
  now <- getCurrentTime
  id_ <- generateGUID
  let flaggedBy' = Domain.Types.Suspect.FlaggedBy flaggedCategory merchantShortId
  let suspect =
        Domain.Types.Suspect.Suspect
          { id = id_,
            dl = dl,
            firstName = firstName,
            lastName = lastName,
            voterId = voterId,
            flaggedStatus = Domain.Types.Suspect.Flagged,
            flagUpdatedAt = now,
            flaggedBy = [flaggedBy'],
            statusChangedReason = Nothing,
            flaggedCounter = 1,
            createdAt = now,
            updatedAt = now
          }
  return suspect

buildSuspectStatusHistory :: TokenInfo -> Text -> Domain.Types.SuspectFlagRequest.AdminApproval -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> Environment.Flow Domain.Types.SuspectStatusHistory.SuspectStatusHistory
buildSuspectStatusHistory tokenInfo merchantShortId approval Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..} = do
  now <- getCurrentTime
  id_ <- generateGUID
  let flaggedBy' = Domain.Types.Suspect.FlaggedBy flaggedCategory merchantShortId
  let suspectStatusHistory =
        Domain.Types.SuspectStatusHistory.SuspectStatusHistory
          { id = id_,
            dl = dl,
            firstName = Just firstName,
            lastName = Just lastName,
            flaggedStatus = flaggedStatus,
            statusChangedReason = Just flaggedReason,
            voterId = voterId,
            flaggedBy = Just [flaggedBy'],
            createdAt = now,
            updatedAt = now,
            merchantId = Just tokenInfo.merchantId,
            merchantShortId = Just merchantShortId,
            adminApproval = Just approval
          }
  return suspectStatusHistory

buildSuspectBody :: Text -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> SuspectBody
buildSuspectBody merchantshortId req =
  SuspectBody
    { dl = req.dl,
      voterId = req.voterId,
      flaggedCategory = req.flaggedCategory,
      flaggedReason = req.flaggedReason,
      flaggedBy = merchantshortId
    }
