{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnUpdate
  ( onUpdate,
    validateRequest,
    OnUpdateReq (..),
    OnUpdateFareBreakup (..),
    EstimateRepetitionEstimateInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    DEstimate.FareRange (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
    DRideAssigned (..),
    DRideStarted (..),
    DRideCompleted (..),
    DBookingCancelled (..),
    DBookingReallocation (..),
    DDriverArrived (..),
    DEstimateRepetition (..),
    DNewMessage (..),
    DSafetyAlert (..),
    DStopArrived (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import Domain.Action.UI.HotSpot
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.HotSpot
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.VehicleVariant
import Environment ()
import Kernel.Beam.Functions
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import Tools.Maps (LatLong)
import Tools.Metrics (HasBAPMetrics, incrementRideCreatedRequestCount)
import qualified Tools.Notifications as Notify

data OnUpdateReq
  = RideAssignedReq DRideAssigned
  | RideStartedReq DRideStarted
  | RideCompletedReq DRideCompleted
  | BookingCancelledReq DBookingCancelled
  | BookingReallocationReq DBookingReallocation
  | DriverArrivedReq DDriverArrived
  | EstimateRepetitionReq DEstimateRepetition
  | NewMessageReq DNewMessage
  | SafetyAlertReq DSafetyAlert
  | StopArrivedReq DStopArrived

data ValidatedOnUpdateReq
  = ValidatedRideAssignedReq DRideAssigned SRB.Booking
  | ValidatedRideStartedReq DRideStarted SRB.Booking SRide.Ride
  | ValidatedRideCompletedReq DRideCompleted SRB.Booking SRide.Ride DP.Person
  | ValidatedBookingCancelledReq DBookingCancelled SRB.Booking (Maybe SRide.Ride)
  | ValidatedBookingReallocationReq DBookingReallocation SRB.Booking SRide.Ride
  | ValidatedDriverArrivedReq DDriverArrived SRB.Booking SRide.Ride
  | ValidatedEstimateRepetitionReq DEstimateRepetition SRB.Booking SRide.Ride DSR.SearchRequest DEstimate.Estimate
  | ValidatedNewMessageReq DNewMessage SRB.Booking SRide.Ride
  | ValidatedSafetyAlertReq DSafetyAlert SRB.Booking SRide.Ride
  | ValidatedStopArrivedReq SRB.Booking SRide.Ride

data DRideAssigned = DRideAssigned
  { bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: UTCTime,
    isDriverBirthDay :: Bool,
    isFreeRide :: Bool,
    otp :: Text,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleModel :: Text
  }

data DRideStarted = DRideStarted
  { bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    tripStartLocation :: Maybe LatLong,
    endOtp_ :: Maybe Text,
    startOdometerReading :: Maybe Centesimal
  }

data DRideCompleted = DRideCompleted
  { bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    fare :: Money,
    totalFare :: Money,
    fareBreakups :: [OnUpdateFareBreakup],
    chargeableDistance :: HighPrecMeters,
    traveledDistance :: HighPrecMeters,
    paymentUrl :: Maybe Text,
    tripEndLocation :: Maybe LatLong,
    endOdometerReading :: Maybe Centesimal
  }

data DBookingCancelled = DBookingCancelled
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: SBCR.CancellationSource
  }

data DBookingReallocation = DBookingReallocation
  { bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    reallocationSource :: SBCR.CancellationSource
  }

data DDriverArrived = DDriverArrived
  { bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    arrivalTime :: Maybe UTCTime -- not used
  }

data DEstimateRepetition = DEstimateRepetition
  { searchRequestId :: Id DSR.SearchRequest,
    bppEstimateId :: Id DEstimate.BPPEstimate,
    bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    cancellationSource :: SBCR.CancellationSource
  }

data DNewMessage = DNewMessage
  { bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    message :: Text
  }

data DSafetyAlert = DSafetyAlert
  { bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    reason :: Text,
    code :: Text
  }

newtype DStopArrived = DStopArrived
  { bppRideId :: Id SRide.BPPRide
  }

data OnUpdateFareBreakup = OnUpdateFareBreakup
  { amount :: HighPrecMoney,
    description :: Text
  }

data EstimateRepetitionEstimateInfo = EstimateRepetitionEstimateInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    totalFareRange :: DEstimate.FareRange,
    descriptions :: [Text],
    estimateBreakupList :: [EstimateBreakupInfo],
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: WaitingChargesInfo,
    driversLocation :: [LatLong]
  }

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }

data WaitingChargesInfo = WaitingChargesInfo
  { waitingTimeEstimatedThreshold :: Maybe Seconds,
    waitingChargePerMin :: Maybe Money
  }

data EstimateBreakupInfo = EstimateBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

data BreakupPriceInfo = BreakupPriceInfo
  { currency :: Text,
    value :: Money
  }

onUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["isBecknSpecVersion2" ::: Bool],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  ValidatedOnUpdateReq ->
  m ()
onUpdate (ValidatedRideAssignedReq DRideAssigned {..} booking) = do
  mbMerchant <- CQM.findById booking.merchantId
  ride <- buildRide mbMerchant
  triggerRideCreatedEvent RideEventData {ride = ride, personId = booking.riderId, merchantId = booking.merchantId}
  let category = case booking.specialLocationTag of
        Just _ -> "specialLocation"
        Nothing -> "normal"
  incrementRideCreatedRequestCount booking.merchantId.getId booking.merchantOperatingCityId.getId category
  _ <- QRB.updateStatus booking.id SRB.TRIP_ASSIGNED
  _ <- QRide.createRide ride

  _ <- QPFS.updateStatus booking.riderId DPFS.RIDE_PICKUP {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, otp, vehicleNumber, fromLocation = Maps.getCoordinates booking.fromLocation, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
  Notify.notifyOnRideAssigned booking ride
  when isDriverBirthDay $ do
    Notify.notifyDriverBirthDay booking.riderId driverName
  withLongRetry $ CallBPP.callTrack booking ride
  where
    buildRide :: MonadFlow m => Maybe DMerchant.Merchant -> m SRide.Ride
    buildRide mbMerchant = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      let fromLocation = booking.fromLocation
          toLocation = case booking.bookingDetails of
            SRB.OneWayDetails details -> Just details.toLocation
            SRB.RentalDetails _ -> Nothing
            SRB.DriverOfferDetails details -> Just details.toLocation
            SRB.OneWaySpecialZoneDetails details -> Just details.toLocation
            SRB.InterCityDetails details -> Just details.toLocation
      let allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
      return
        SRide.Ride
          { id = guid,
            bookingId = booking.id,
            merchantId = Just booking.merchantId,
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            status = SRide.NEW,
            trackingUrl = Nothing,
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            traveledDistance = Nothing,
            driverArrivalTime = Nothing,
            vehicleVariant = booking.vehicleVariant,
            createdAt = now,
            updatedAt = now,
            rideStartTime = Nothing,
            rideEndTime = Nothing,
            rideRating = Nothing,
            safetyCheckStatus = Nothing,
            isFreeRide = Just isFreeRide,
            endOtp = Nothing,
            startOdometerReading = Nothing,
            endOdometerReading = Nothing,
            ..
          }
onUpdate (ValidatedRideStartedReq DRideStarted {..} booking ride) = do
  fork "ride start geohash frequencyUpdater" $ do
    case tripStartLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripStart
      Nothing -> return ()
  rideStartTime <- getCurrentTime
  let updRideForStartReq =
        ride{status = SRide.INPROGRESS,
             rideStartTime = Just rideStartTime,
             rideEndTime = Nothing,
             endOtp = endOtp_,
             startOdometerReading = startOdometerReading
            }
  triggerRideStartedEvent RideEventData {ride = updRideForStartReq, personId = booking.riderId, merchantId = booking.merchantId}
  _ <- QRide.updateMultiple updRideForStartReq.id updRideForStartReq
  _ <- QPFS.updateStatus booking.riderId DPFS.RIDE_STARTED {rideId = ride.id, bookingId = booking.id, trackingUrl = ride.trackingUrl, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
  fork "notify emergency contacts" $ Notify.notifyRideStartToEmergencyContacts booking ride
  Notify.notifyOnRideStarted booking ride
onUpdate (ValidatedRideCompletedReq DRideCompleted {..} booking ride person) = do
  fork "ride end geohash frequencyUpdater" $ do
    case tripEndLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripEnd
      Nothing -> return ()
  SMC.updateTotalRidesCounters booking.riderId
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  SMC.updateTotalRidesInWindowCounters booking.riderId merchantConfigs

  rideEndTime <- getCurrentTime
  let updRide =
        ride{status = SRide.COMPLETED,
             fare = Just fare,
             totalFare = Just totalFare,
             chargeableDistance = Just chargeableDistance,
             rideEndTime = Just rideEndTime,
             endOdometerReading = endOdometerReading
            }
  breakups <- traverse (buildFareBreakup booking.id) fareBreakups
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
          Nothing -> True
  triggerRideEndEvent RideEventData {ride = updRide, personId = booking.riderId, merchantId = booking.merchantId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = SRB.COMPLETED}}
  when shouldUpdateRideComplete $ void $ QP.updateHasTakenValidRide booking.riderId
  unless (booking.status == SRB.COMPLETED) $ void $ QRB.updateStatus booking.id SRB.COMPLETED
  whenJust paymentUrl $ QRB.updatePaymentUrl booking.id
  _ <- QRide.updateMultiple updRide.id updRide
  _ <- QFareBreakup.createMany breakups
  void $ QPFS.updateStatus booking.riderId DPFS.PENDING_RATING {rideId = ride.id}
  QPFS.clearCache booking.riderId
  -- uncomment for update api test; booking.paymentMethodId should be present
  -- whenJust booking.paymentMethodId $ \paymentMethodId -> do
  --   merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  --   paymentMethod <-
  --     CQMPM.findByIdAndMerchantId paymentMethodId booking.merchantId
  --       >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId.getId)
  --   let dUpdateReq = ACL.PaymentCompletedBuildReq
  --         { bppBookingId,
  --           bppRideId = ride.bppRideId,
  --           paymentMethodInfo = DMPM.mkPaymentMethodInfo paymentMethod,
  --           bppId = booking.providerId,
  --           bppUrl = booking.providerUrl,
  --           transactionId = booking.transactionId,
  --           merchant
  --         }
  --   becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  --   void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq

  Notify.notifyOnRideCompleted booking updRide
  where
    buildFareBreakup :: MonadFlow m => Id SRB.Booking -> OnUpdateFareBreakup -> m DFareBreakup.FareBreakup
    buildFareBreakup bookingId OnUpdateFareBreakup {..} = do
      guid <- generateGUID
      pure
        DFareBreakup.FareBreakup
          { id = guid,
            ..
          }
onUpdate (ValidatedBookingCancelledReq DBookingCancelled {..} booking mbRide) = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show cancellationSource)
  let bookingCancellationReason = mkBookingCancellationReason booking.id (mbRide <&> (.id)) cancellationSource booking.merchantId
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  fork "incrementing fraud counters" $ do
    let merchantOperatingCityId = booking.merchantOperatingCityId
    mFraudDetected <- SMC.anyFraudDetected booking.riderId merchantOperatingCityId merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer booking.riderId (Just mc.id)
  case mbRide of
    Just ride -> do
      case cancellationSource of
        SBCR.ByUser -> SMC.updateCustomerFraudCounters booking.riderId merchantConfigs
        SBCR.ByDriver -> SMC.updateCancelledByDriverFraudCounters booking.riderId merchantConfigs
        _ -> pure ()
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
    Nothing -> do
      logDebug "No ride found for the booking."
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}}
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  unless (booking.status == SRB.CANCELLED) $ void $ QRB.updateStatus booking.id SRB.CANCELLED
  whenJust mbRide $ \ride -> void $ do
    unless (ride.status == SRide.CANCELLED) $ void $ QRide.updateStatus ride.id SRide.CANCELLED
  unless (cancellationSource == SBCR.ByUser) $
    QBCR.upsert bookingCancellationReason
  QPFS.clearCache booking.riderId
  -- notify customer
  Notify.notifyOnBookingCancelled booking cancellationSource
onUpdate (ValidatedBookingReallocationReq DBookingReallocation {..} booking ride) = do
  mbRide <- QRide.findActiveByRBId booking.id
  let bookingCancellationReason = mkBookingCancellationReason booking.id (mbRide <&> (.id)) reallocationSource booking.merchantId
  _ <- QRB.updateStatus booking.id SRB.AWAITING_REASSIGNMENT
  _ <- QRide.updateStatus ride.id SRide.CANCELLED
  QBCR.upsert bookingCancellationReason
  Notify.notifyOnBookingReallocated booking
onUpdate (ValidatedDriverArrivedReq DDriverArrived {} booking ride) = do
  now <- getCurrentTime
  unless (isJust ride.driverArrivalTime) $ do
    _ <- QRide.updateDriverArrival ride.id
    void $ QPFS.updateStatus booking.riderId DPFS.DRIVER_ARRIVED {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, driverLocation = Nothing, driverArrivalTime = Just now}
onUpdate (ValidatedNewMessageReq DNewMessage {..} booking _ride) = do
  Notify.notifyOnNewMessage booking message
onUpdate (ValidatedEstimateRepetitionReq DEstimateRepetition {..} booking ride searchReq estimate) = do
  let bookingCancellationReason = mkBookingCancellationReason booking.id (Just ride.id) cancellationSource booking.merchantId
  logTagInfo ("EstimateId-" <> getId estimate.id) "Estimate repetition."

  _ <- QEstimate.updateStatus estimate.id DEstimate.DRIVER_QUOTE_REQUESTED
  _ <- QRB.updateStatus booking.id SRB.REALLOCATED
  _ <- QRide.updateStatus ride.id SRide.CANCELLED
  _ <- QBCR.upsert bookingCancellationReason
  _ <- QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimate.id, validTill = searchReq.validTill}
  QPFS.clearCache searchReq.riderId
  -- notify customer
  Notify.notifyOnEstimatedReallocated booking estimate.id
onUpdate (ValidatedSafetyAlertReq DSafetyAlert {..} booking _ride) = do
  Notify.notifySafetyAlert booking code
onUpdate (ValidatedStopArrivedReq booking ride) = do
  QRB.updateStop booking Nothing
  Notify.notifyOnStopReached booking ride

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  OnUpdateReq ->
  m ValidatedOnUpdateReq
validateRequest (RideAssignedReq req@DRideAssigned {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  unless (isAssignable booking) $ throwError (BookingInvalidStatus $ show booking.status)
  return $ ValidatedRideAssignedReq req booking
  where
    isAssignable booking = booking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT]
validateRequest (RideStartedReq req@DRideStarted {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (booking.status == SRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  return $ ValidatedRideStartedReq req booking ride
validateRequest (RideCompletedReq req@DRideCompleted {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  let bookingCanBeCompleted = booking.status == SRB.TRIP_ASSIGNED
      rideCanBeCompleted = ride.status == SRide.INPROGRESS
      bookingAlreadyCompleted = booking.status == SRB.COMPLETED
      rideAlreadyCompleted = ride.status == SRide.COMPLETED
  unless (bookingCanBeCompleted || (bookingAlreadyCompleted && rideCanBeCompleted)) $
    throwError (BookingInvalidStatus $ show booking.status)
  unless (rideCanBeCompleted || (rideAlreadyCompleted && bookingCanBeCompleted)) $
    throwError (RideInvalidStatus $ show ride.status)
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  return $ ValidatedRideCompletedReq req booking ride person
validateRequest (BookingCancelledReq req@DBookingCancelled {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [SRide.INPROGRESS, SRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == SRB.CANCELLED
  unless (isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))
  return $ ValidatedBookingCancelledReq req booking mbRide
  where
    isBookingCancellable booking =
      booking.status `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
validateRequest (BookingReallocationReq req@DBookingReallocation {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  return $ ValidatedBookingReallocationReq req booking ride
validateRequest (DriverArrivedReq req@DDriverArrived {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
  return $ ValidatedDriverArrivedReq req booking ride
  where
    isValidRideStatus status = status == SRide.NEW
validateRequest (NewMessageReq req@DNewMessage {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
  return $ ValidatedNewMessageReq req booking ride
  where
    isValidRideStatus status = status == SRide.NEW
validateRequest (EstimateRepetitionReq req@DEstimateRepetition {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist bppEstimateId.getId)
  return $ ValidatedEstimateRepetitionReq req booking ride searchReq estimate
validateRequest (SafetyAlertReq req@DSafetyAlert {..}) = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  unless (booking.status == SRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (ride.status == SRide.INPROGRESS) $ throwError (BookingInvalidStatus "$ show booking.status")
  return $ ValidatedSafetyAlertReq req booking ride
validateRequest (StopArrivedReq DStopArrived {..}) = do
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> ride.bookingId.getId)
  unless (ride.status == SRide.INPROGRESS) $ throwError $ RideInvalidStatus ("This ride " <> ride.id.getId <> " is not in progress")
  case booking.bookingDetails of
    SRB.OneWayDetails _ -> throwError $ InvalidRequest "Stops are not present in static offer on demand rides"
    SRB.DriverOfferDetails _ -> throwError $ InvalidRequest "Stops are not present in dynamic offer on demand rides"
    SRB.OneWaySpecialZoneDetails _ -> throwError $ InvalidRequest "Stops are not present in on ride otp rides"
    SRB.InterCityDetails _ -> throwError $ InvalidRequest "Stops are not present in intercity rides"
    SRB.RentalDetails SRB.RentalBookingDetails {..} -> do
      unless (isJust stopLocation) $ throwError (InvalidRequest $ "Can't find stop to be reached for bpp ride " <> bppRideId.getId)
      return $ ValidatedStopArrivedReq booking ride

mkBookingCancellationReason ::
  Id SRB.Booking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  SBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId =
  -- cancellationSource merchantId =
  SBCR.BookingCancellationReason
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
