{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.SearchSuspect where

import qualified API.Types.UI.Notification as Notification
import API.Types.UI.SearchSuspect
import qualified API.Types.UI.SearchSuspect
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person
import Domain.Types.Suspect as Domain.Types.Suspect
import qualified Domain.Types.Suspect as Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest as Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusHistory as Domain.Types.SuspectStatusHistory
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id, length, map, mapM_, readMaybe)
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Time
import Servant hiding (throwError)
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import qualified Storage.Queries.PortalConfigs as PC
import qualified Storage.Queries.Suspect as SQ
import qualified Storage.Queries.SuspectExtra as SQE
import qualified Storage.Queries.SuspectFlagRequest as SQF
import qualified Storage.Queries.SuspectStatusHistoryExtra as SQSH
import "lib-dashboard" Tools.Auth
import Tools.Auth.Webhook
import Tools.Error
import "lib-dashboard" Tools.Error

postSearchSuspectList :: TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
postSearchSuspectList _ req = do
  portalConfig <- PC.findByConfigName "BULK_SEARCH_COUNT"
  let bulkSearchCount = case portalConfig of
        Just config -> read (T.unpack config.value) :: Int
        Nothing -> 100
  when (length req.suspectReqList > bulkSearchCount) $ throwError (BulkSearchLimitExceeded bulkSearchCount)
  let dlList = mapMaybe (\suspect -> suspect.dl) $ req.suspectReqList
  let voterIdList = mapMaybe (\suspect -> suspect.voterId) $ req.suspectReqList
  suspectList <- SQE.findAllByDlOrVoterId dlList voterIdList
  let resp = map buildSearchSuspectResp suspectList
  let count = length suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

postCheckSuspectStatusHistory :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.Flow API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp
postCheckSuspectStatusHistory _ mbLimit mbOffset req = do
  case (req.dl, req.voterId) of
    (Nothing, Nothing) -> throwError SuspectDlOrVoterIdRequired
    (Just dl, _) -> do
      suspect <- SQSH.findAllByDl mbLimit mbOffset (Just dl)
      let resp = map buildCheckSuspectStatusHistoryResp suspect
      return CheckSuspectStatusHistoryResp {suspectStatusHistory = resp}
    _ -> do
      suspect <- SQSH.findAllByVoterId mbLimit mbOffset req.voterId
      let resp = map buildCheckSuspectStatusHistoryResp suspect
      return CheckSuspectStatusHistoryResp {suspectStatusHistory = resp}

postPartnerSearchAgent :: AuthToken -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
postPartnerSearchAgent _ req = do
  portalConfig <- PC.findByConfigName "BULK_SEARCH_COUNT"
  let bulkSearchCount = case portalConfig of
        Just config -> read (T.unpack config.value) :: Int
        Nothing -> 100
  when (length req.suspectReqList > bulkSearchCount) $ throwError (BulkSearchLimitExceeded bulkSearchCount)
  let dlList = mapMaybe (\suspect -> suspect.dl) $ req.suspectReqList
  let voterIdList = mapMaybe (\suspect -> suspect.voterId) $ req.suspectReqList
  suspectList <- SQE.findAllByDlOrVoterId dlList voterIdList
  let resp = map buildSearchSuspectResp suspectList
  let count = length suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

getSuspectList :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
getSuspectList _ mbFrom mbLimit mbOffset mbTo = do
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
  let offset = fromMaybe 0 mbOffset
  let defaultFrom = UTCTime (utctDay now) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  suspectList <- SQE.findAll limit offset from_ to
  let count = length suspectList
  let resp = map buildSearchSuspectResp suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

getPartnerSuspectList :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.Flow API.Types.UI.SearchSuspect.SuspectsList
getPartnerSuspectList _ mbFrom mbLimit mbOffset mbTo = do
  now <- getCurrentTime
  let limit = fromMaybe 10 mbLimit
  let offset = fromMaybe 0 mbOffset
  let defaultFrom = UTCTime (utctDay now) 0
      from_ = fromMaybe defaultFrom mbFrom
      to = fromMaybe now mbTo
  suspectList <- SQE.findAll limit offset from_ to
  let count = length suspectList
  let resp = map buildSearchSuspectResp suspectList
  return $ SuspectsList {suspects = resp, summary = Notification.Summary {totalCount = 10000, count = count}}

postMerchantCheckSuspectStatusHistory :: TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.Flow API.Types.UI.SearchSuspect.FlagHistoryResp
postMerchantCheckSuspectStatusHistory tokenInfo req = do
  case (req.dl, req.voterId) of
    (Nothing, Nothing) -> throwError SuspectDlOrVoterIdRequired
    (Just dl, _) -> do
      merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
      flagReqList <- SQF.findAllByMerchantIdAndDl (Just tokenInfo.merchantId) (Just dl)
      suspectList <- SQE.findAllByNotFlagStatus Domain.Types.Suspect.Flagged
      let flagRequestList = map (\flagReq -> buildFlagRequestHistory (merchant.shortId.getShortId) flagReq) flagReqList
      let suspectStatusChangeList = map buildFlagRequestHistoryBySuspect suspectList
      let sortedList = sortOn (\x -> x.createdAt) $ flagRequestList ++ suspectStatusChangeList
      return FlagHistoryResp {flagRequestHistory = sortedList}
    _ -> do
      merchant <- QMerchant.findById tokenInfo.merchantId >>= fromMaybeM (MerchantNotFound tokenInfo.merchantId.getId)
      flagReqList <- SQF.findAllByMerchantIdAndVoterId (Just tokenInfo.merchantId) req.voterId
      suspectList <- SQE.findAllByNotFlagStatus Domain.Types.Suspect.Flagged
      let flagRequestList = map (\flagReq -> buildFlagRequestHistory (merchant.shortId.getShortId) flagReq) flagReqList
      let suspectStatusChangeList = map buildFlagRequestHistoryBySuspect suspectList
      let sortedList = sortOn (\x -> x.createdAt) $ flagRequestList ++ suspectStatusChangeList
      return FlagHistoryResp {flagRequestHistory = sortedList}

buildSearchSuspectResp :: Domain.Types.Suspect.Suspect -> API.Types.UI.SearchSuspect.SearchSuspectResp
buildSearchSuspectResp Domain.Types.Suspect.Suspect {..} = do
  API.Types.UI.SearchSuspect.SearchSuspectResp
    { id = id.getId,
      dl = dl,
      voterId = voterId,
      firstName = firstName,
      lastName = lastName,
      flaggedStatus = flaggedStatus,
      flaggedCounter = flaggedCounter,
      statusChangedReason = statusChangedReason,
      flaggedBy = Just flaggedBy,
      createdAt = createdAt,
      updatedAt = updatedAt,
      flagUpdatedAt = flagUpdatedAt
    }

buildFlagRequestHistory :: Text -> Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> API.Types.UI.SearchSuspect.FlagRequestHistory
buildFlagRequestHistory mShortId Domain.Types.SuspectFlagRequest.SuspectFlagRequest {..} = do
  API.Types.UI.SearchSuspect.FlagRequestHistory
    { dl = dl,
      voterId = voterId,
      firstName = Just firstName,
      lastName = Just lastName,
      flaggedStatus = flaggedStatus,
      merchantShortId = Just mShortId,
      adminApproval = Just adminApproval,
      flaggedCategory = Just flaggedCategory,
      flaggedReason = Just flaggedReason,
      approvedBy = approvedBy,
      flaggedBy = Just flaggedBy,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

buildFlagRequestHistoryBySuspect :: Domain.Types.Suspect.Suspect -> API.Types.UI.SearchSuspect.FlagRequestHistory
buildFlagRequestHistoryBySuspect Domain.Types.Suspect.Suspect {..} = do
  API.Types.UI.SearchSuspect.FlagRequestHistory
    { dl = dl,
      voterId = voterId,
      firstName = Just firstName,
      lastName = Just lastName,
      flaggedStatus = flaggedStatus,
      merchantShortId = Nothing,
      adminApproval = Nothing,
      flaggedCategory = Nothing,
      flaggedReason = statusChangedReason,
      approvedBy = Nothing,
      flaggedBy = Nothing,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

buildCheckSuspectStatusHistoryResp :: Domain.Types.SuspectStatusHistory.SuspectStatusHistory -> API.Types.UI.SearchSuspect.StatusHistory
buildCheckSuspectStatusHistoryResp Domain.Types.SuspectStatusHistory.SuspectStatusHistory {..} = do
  API.Types.UI.SearchSuspect.StatusHistory
    { id = id.getId,
      dl = dl,
      firstName = firstName,
      lastName = lastName,
      voterId = voterId,
      flaggedStatus = flaggedStatus,
      statusChangedReason = statusChangedReason,
      flaggedBy = flaggedBy,
      adminApproval = adminApproval,
      merchantShortId = merchantShortId,
      createdAt = createdAt,
      updatedAt = updatedAt
    }
