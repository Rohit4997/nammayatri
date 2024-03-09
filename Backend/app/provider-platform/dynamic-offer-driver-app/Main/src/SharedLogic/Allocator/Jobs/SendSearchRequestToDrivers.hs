{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Domain.Types.Booking (BookingStatus (..))
import Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
import qualified Domain.Types.FarePolicy as DFP
import Domain.Types.GoHomeConfig (GoHomeConfig)
import Domain.Types.Merchant (Merchant)
import Domain.Types.SearchRequest (SearchRequest)
import Domain.Types.SearchTry (SearchTry)
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude hiding (handle)
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle
import qualified SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal as I
import qualified SharedLogic.Booking as SBooking
import SharedLogic.DriverPool
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.SearchTry as SST
import qualified Storage.CachedQueries.GoHomeConfig as CQGHC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics

sendSearchRequestToDrivers ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    MonadFlow m,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "isBecknSpecVersion2" r Bool,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Job 'SendSearchRequestToDriver ->
  m ExecutionResult
sendSearchRequestToDrivers Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
  let searchTryId = jobData.searchTryId
  searchTry <- B.runInReplica $ QST.findById searchTryId >>= fromMaybeM (SearchTryNotFound searchTryId.getId)
  searchReq <- B.runInReplica $ QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  merchant <- CQM.findById searchReq.providerId >>= fromMaybeM (MerchantNotFound (searchReq.providerId.getId))
  driverPoolConfig <- getDriverPoolConfig searchReq.merchantOperatingCityId searchTry.vehicleVariant searchTry.tripCategory jobData.estimatedRideDistance
  goHomeCfg <- CQGHC.findByMerchantOpCityId searchReq.merchantOperatingCityId Nothing
  (res, _, _) <- sendSearchRequestToDrivers' driverPoolConfig searchReq searchTry merchant jobData.driverExtraFeeBounds goHomeCfg
  return res

sendSearchRequestToDrivers' ::
  ( EncFlow m r,
    TranslateFlow m r,
    EsqDBReplicaFlow m r,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Log m,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "isBecknSpecVersion2" r Bool,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  DriverPoolConfig ->
  SearchRequest ->
  SearchTry ->
  Merchant ->
  Maybe DFP.DriverExtraFeeBounds ->
  GoHomeConfig ->
  m (ExecutionResult, PoolType, Maybe Seconds)
sendSearchRequestToDrivers' driverPoolConfig searchReq searchTry merchant driverExtraFeeBounds goHomeCfg = do
  -- In case of static offer flow we will have booking created before driver ride request is sent
  mbBooking <- if DTC.isDynamicOfferTrip searchTry.tripCategory then pure Nothing else QRB.findByQuoteId searchTry.estimateId
  handler (handle mbBooking) goHomeCfg
  where
    handle mbBooking =
      Handle
        { isBatchNumExceedLimit = I.isBatchNumExceedLimit driverPoolConfig searchTry.id,
          isReceivedMaxDriverQuotes = I.isReceivedMaxDriverQuotes driverPoolConfig searchTry.id,
          getNextDriverPoolBatch = I.getNextDriverPoolBatch driverPoolConfig searchReq searchTry,
          sendSearchRequestToDrivers = I.sendSearchRequestToDrivers searchReq searchTry driverExtraFeeBounds driverPoolConfig,
          getRescheduleTime = I.getRescheduleTime driverPoolConfig.singleBatchProcessTime,
          setBatchDurationLock = I.setBatchDurationLock searchTry.id driverPoolConfig.singleBatchProcessTime,
          createRescheduleTime = I.createRescheduleTime driverPoolConfig.singleBatchProcessTime,
          metrics =
            MetricsHandle
              { incrementTaskCounter = Metrics.incrementTaskCounter merchant.name,
                incrementFailedTaskCounter = Metrics.incrementFailedTaskCounter merchant.name,
                putTaskDuration = Metrics.putTaskDuration merchant.name
              },
          isSearchTryValid = I.isSearchTryValid searchTry.id,
          initiateDriverSearchBatch = SST.initiateDriverSearchBatch sendSearchRequestToDrivers' merchant searchReq searchTry.tripCategory searchTry.vehicleVariant searchTry.estimateId searchTry.customerExtraFee searchTry.messageId False,
          isScheduledBooking = searchTry.isScheduled,
          cancelSearchTry = I.cancelSearchTry searchTry.id,
          isBookingValid = do
            case mbBooking of
              Just booking -> booking.status `notElem` [COMPLETED, CANCELLED]
              Nothing -> True,
          cancelBookingIfApplies = do
            whenJust mbBooking $ \booking -> do
              SBooking.cancelBooking booking Nothing merchant
        }
