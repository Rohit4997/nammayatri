{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnStatus
  ( onStatus,
    DOnStatusReq (..),
    RideDetails (..),
    OnStatusFareBreakup (..),
    NewRideInfo (..),
    RideStartedInfo (..),
    RideCompletedInfo (..),
  )
where

import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide

data DOnStatusReq = DOnStatusReq
  { bppBookingId :: Id DB.BPPBooking,
    rideDetails :: RideDetails
  }

data NewRideInfo = NewRideInfo
  { bppRideId :: Id DRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: UTCTime,
    otp :: Text,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleModel :: Text
  }

data RideStartedInfo = RideStartedInfo
  { rideStartTime :: UTCTime,
    driverArrivalTime :: Maybe UTCTime
  }

data RideCompletedInfo = RideCompletedInfo
  { rideEndTime :: UTCTime,
    fare :: Money,
    totalFare :: Money,
    fareBreakups :: [OnStatusFareBreakup],
    chargeableDistance :: HighPrecMeters,
    traveledDistance :: HighPrecMeters,
    paymentUrl :: Maybe Text
  }

data RideDetails
  = NewBookingDetails
  | RideAssignedDetails NewRideInfo
  | RideStartedDetails NewRideInfo RideStartedInfo
  | RideCompletedDetails NewRideInfo RideStartedInfo RideCompletedInfo
  | BookingCancelledDetails (Maybe NewRideInfo) DBCR.CancellationSource
  | BookingReallocationDetails NewRideInfo DBCR.CancellationSource

-- the same as OnUpdateFareBreakup
data OnStatusFareBreakup = OnStatusFareBreakup
  { amount :: HighPrecMoney,
    description :: Text
  }

data RideEntity = UpdatedRide DUpdatedRide | RenewedRide DRide.Ride

data DUpdatedRide = DUpdatedRide
  { ride :: DRide.Ride,
    rideOldStatus :: DRide.RideStatus
  }

buildRideEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DB.Booking -> (DRide.Ride -> DRide.Ride) -> NewRideInfo -> m RideEntity
buildRideEntity booking updRide newRideInfo = do
  mbExistingRide <- B.runInReplica $ QRide.findByBPPRideId newRideInfo.bppRideId
  case mbExistingRide of
    Nothing -> do
      mbMerchant <- CQM.findById booking.merchantId
      newRide <- buildNewRide mbMerchant booking newRideInfo
      pure $ RenewedRide (updRide newRide)
    Just existingRide -> do
      unless (existingRide.bookingId == booking.id) $ throwError (InvalidRequest "Invalid rideId")
      pure $ UpdatedRide $ DUpdatedRide {ride = updRide existingRide, rideOldStatus = existingRide.status}

rideBookingTransaction :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DB.BookingStatus -> DRide.RideStatus -> DB.Booking -> RideEntity -> m ()
rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity = do
  unless (booking.status == bookingNewStatus) $ do
    QB.updateStatus booking.id bookingNewStatus
  case rideEntity of
    UpdatedRide (DUpdatedRide {ride, rideOldStatus}) -> do
      unless (rideOldStatus == rideNewStatus) $ do
        QRide.updateMultiple ride.id ride
    RenewedRide renewedRide -> do
      QRide.create renewedRide

isStatusChanged :: DB.BookingStatus -> DB.BookingStatus -> RideEntity -> Bool
isStatusChanged bookingOldStatus bookingNewStatus rideEntity = do
  let bookingStatusChanged = bookingOldStatus == bookingNewStatus
  let rideStatusChanged = case rideEntity of
        UpdatedRide (DUpdatedRide {ride, rideOldStatus}) -> rideOldStatus == ride.status
        RenewedRide {} -> True
  bookingStatusChanged || rideStatusChanged

onStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DOnStatusReq -> m ()
onStatus req = do
  booking <- QB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> req.bppBookingId.getId)
  case req.rideDetails of
    NewBookingDetails -> do
      mbExistingRide <- B.runInReplica $ QRide.findActiveByRBId booking.id
      unless (booking.status == bookingNewStatus) $ do
        QB.updateStatus booking.id bookingNewStatus
      whenJust mbExistingRide \existingRide -> do
        unless (existingRide.status == rideNewStatus) $ do
          QRide.updateStatus existingRide.id rideNewStatus
      where
        bookingNewStatus = DB.NEW
        rideNewStatus = DRide.CANCELLED
    RideAssignedDetails newRideInfo -> do
      rideEntity <- buildRideEntity booking updateNewRide newRideInfo
      rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
      where
        bookingNewStatus = DB.TRIP_ASSIGNED
        rideNewStatus = DRide.NEW
        updateNewRide newRide = newRide{status = rideNewStatus}
    RideStartedDetails newRideInfo rideStartedInfo -> do
      rideEntity <- buildRideEntity booking updateRideStarted newRideInfo
      rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
      where
        bookingNewStatus = DB.TRIP_ASSIGNED
        rideNewStatus = DRide.INPROGRESS
        updateRideStarted newRide =
          newRide{status = rideNewStatus,
                  rideStartTime = Just rideStartedInfo.rideStartTime,
                  driverArrivalTime = rideStartedInfo.driverArrivalTime
                 }
    RideCompletedDetails newRideInfo rideStartedInfo rideCompletedInfo -> do
      rideEntity <- buildRideEntity booking updateRideCompleted newRideInfo
      rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
      when (isStatusChanged booking.status bookingNewStatus rideEntity) $ do
        breakups <- traverse (buildFareBreakup booking.id) rideCompletedInfo.fareBreakups
        QFareBreakup.deleteAllByBookingId booking.id
        QFareBreakup.createMany breakups
        whenJust rideCompletedInfo.paymentUrl $ QB.updatePaymentUrl booking.id
      where
        bookingNewStatus = DB.COMPLETED
        rideNewStatus = DRide.COMPLETED
        updateRideCompleted newRide =
          newRide{status = rideNewStatus,
                  rideStartTime = Just rideStartedInfo.rideStartTime,
                  driverArrivalTime = rideStartedInfo.driverArrivalTime,
                  fare = Just rideCompletedInfo.fare,
                  totalFare = Just rideCompletedInfo.totalFare,
                  chargeableDistance = Just rideCompletedInfo.chargeableDistance,
                  -- traveledDistance = Just rideCompletedInfo.traveledDistance, -- did not changed in on_update
                  rideEndTime = Just rideCompletedInfo.rideEndTime
                 }
    BookingCancelledDetails mbNewRideInfo cancellationSource -> do
      mbRideEntity <- forM mbNewRideInfo (buildRideEntity booking updateRideCancelled)
      let mbRideId = case mbRideEntity of
            Just (UpdatedRide (DUpdatedRide {ride})) -> Just ride.id
            Just (RenewedRide ride) -> Just ride.id
            Nothing -> Nothing
      let bookingCancellationReason = mkBookingCancellationReason booking.id mbRideId cancellationSource booking.merchantId
      whenJust mbRideEntity \rideEntity -> do
        rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
      when (maybe (booking.status == bookingNewStatus) (isStatusChanged booking.status bookingNewStatus) mbRideEntity) $ do
        unless (cancellationSource == DBCR.ByUser) $
          QBCR.upsert bookingCancellationReason
      where
        bookingNewStatus = DB.CANCELLED
        rideNewStatus = DRide.CANCELLED
        updateRideCancelled newRide = newRide{status = rideNewStatus}
    BookingReallocationDetails newRideInfo reallocationSource -> do
      rideEntity <- buildRideEntity booking updateReallocatedRide newRideInfo
      let rideId = case rideEntity of
            UpdatedRide (DUpdatedRide {ride}) -> ride.id
            RenewedRide ride -> ride.id
      let bookingCancellationReason = mkBookingCancellationReason booking.id (Just rideId) reallocationSource booking.merchantId
      rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
      when (isStatusChanged booking.status bookingNewStatus rideEntity) $ do
        QBCR.upsert bookingCancellationReason
      where
        bookingNewStatus = DB.AWAITING_REASSIGNMENT
        rideNewStatus = DRide.CANCELLED
        updateReallocatedRide newRide = newRide{status = rideNewStatus}

buildNewRide :: MonadFlow m => Maybe DM.Merchant -> DB.Booking -> NewRideInfo -> m DRide.Ride
buildNewRide mbMerchant booking NewRideInfo {..} = do
  id <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  let fromLocation = booking.fromLocation
      toLocation = case booking.bookingDetails of
        DB.OneWayDetails details -> Just details.toLocation
        DB.RentalDetails _ -> Nothing
        DB.DriverOfferDetails details -> Just details.toLocation
        DB.OneWaySpecialZoneDetails details -> Just details.toLocation
        DB.InterCityDetails details -> Just details.toLocation
  let allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
  let createdAt = now
      updatedAt = now
      merchantId = Just booking.merchantId
      merchantOperatingCityId = Just booking.merchantOperatingCityId
      bookingId = booking.id
      status = DRide.NEW
      vehicleVariant = booking.vehicleVariant
      trackingUrl = Nothing
      fare = Nothing
      totalFare = Nothing
      chargeableDistance = Nothing
      traveledDistance = Nothing
      driverArrivalTime = Nothing
      rideStartTime = Nothing
      rideEndTime = Nothing
      rideRating = Nothing
      isFreeRide = Nothing
      safetyCheckStatus = Nothing
      endOtp = Nothing
      startOdometerReading = Nothing
      endOdometerReading = Nothing
  pure $ DRide.Ride {..}

mkBookingCancellationReason ::
  Id DB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  Id DM.Merchant ->
  DBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId = do
  DBCR.BookingCancellationReason
    { bookingId = bookingId,
      rideId = mbRideId,
      merchantId = Just merchantId,
      source = cancellationSource,
      reasonCode = Nothing,
      reasonStage = Nothing,
      additionalInfo = Nothing,
      driverCancellationLocation = Nothing,
      driverDistToPickup = Nothing
    }

buildFareBreakup :: MonadGuid m => Id DB.Booking -> OnStatusFareBreakup -> m DFareBreakup.FareBreakup
buildFareBreakup bookingId OnStatusFareBreakup {..} = do
  guid <- generateGUID
  pure
    DFareBreakup.FareBreakup
      { id = guid,
        ..
      }
