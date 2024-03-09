{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Ride
  ( rideList,
    rideInfo,
    rideSync,
    multipleRideSync,
    rideRoute,
    currentActiveRide,
    bookingWithVehicleNumberAndPhone,
    mkLocationFromLocationMapping,
    ticketRideList,
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import Data.Coerce (coerce)
import Data.Either.Extra (mapLeft)
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Action.UI.Ride.EndRide as EHandler
import Domain.Action.UI.Ride.StartRide as SRide
import Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.CancellationReason as DCReason
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverOnboarding.DriverRCAssociation
import Domain.Types.DriverOnboarding.Error
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle as DVeh
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.External.Maps.HasCoordinates
import qualified Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.External.LocationTrackingService.Types
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.SyncRide as SyncRide
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Clickhouse.DriverEdaKafka as CHDriverEda
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.DriverOnboarding.DriverRCAssociation as DAQuery
import qualified Storage.Queries.DriverOnboarding.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.DriverQuote as DQ
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as VQuery
import Tools.Error

---------------------------------------------------------------------
rideList ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Common.BookingStatus ->
  Maybe (ShortId Common.Ride) ->
  Maybe Text ->
  Maybe Text ->
  Maybe Money ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Flow Common.RideListRes
rideList merchantShortId opCity mbLimit mbOffset mbBookingStatus mbReqShortRideId mbCustomerPhone mbDriverPhone mbFareDiff mbfrom mbto = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let limit = min maxLimit . fromMaybe defaultLimit $ mbLimit -- TODO move to common code
      offset = fromMaybe 0 mbOffset
  let mbShortRideId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbReqShortRideId
  mbCustomerPhoneDBHash <- getDbHash `traverse` mbCustomerPhone
  mbDriverPhoneDBHash <- getDbHash `traverse` mbDriverPhone
  now <- getCurrentTime
  when (isNothing mbBookingStatus && isNothing mbShortRideId && isNothing mbCustomerPhoneDBHash && isNothing mbDriverPhoneDBHash && isNothing mbFareDiff) $
    throwError $ InvalidRequest "At least one filter is required"
  case (mbfrom, mbto) of
    (Just from, Just to) -> when (from > to) $ throwError $ InvalidRequest "from date should be less than to date"
    _ -> pure ()
  rideItems <- runInReplica $ QRide.findAllRideItems merchant merchantOpCity limit offset mbBookingStatus mbShortRideId mbCustomerPhoneDBHash mbDriverPhoneDBHash mbFareDiff now mbfrom mbto
  logDebug (T.pack "rideItems: " <> T.pack (show $ length rideItems))
  rideListItems <- traverse buildRideListItem rideItems
  let count = length rideListItems
  -- should we consider filters in totalCount, e.g. count all canceled rides?
  -- totalCount <- runInReplica $ QRide.countRides merchant.id
  let summary = Common.Summary {totalCount = 10000, count}
  pure Common.RideListRes {totalItems = count, summary, rides = rideListItems}
  where
    maxLimit = 20
    defaultLimit = 10

buildRideListItem :: EncFlow m r => QRide.RideItem -> m Common.RideListItem
buildRideListItem QRide.RideItem {..} = do
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  pure
    Common.RideListItem
      { rideId = cast @DRide.Ride @Common.Ride rideDetails.id,
        rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) rideShortId,
        customerName,
        customerPhoneNo,
        driverName = rideDetails.driverName,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        tripCategory = castTripCategory tripCategory,
        fareDiff,
        bookingStatus,
        rideCreatedAt = rideCreatedAt
      }

---------------------------------------------------------------------------------------------------
ticketRideList :: ShortId DM.Merchant -> Context.City -> Maybe (ShortId Common.Ride) -> Maybe Text -> Maybe Text -> Maybe Text -> Flow Common.TicketRideListRes
ticketRideList merchantShortId opCity mbRideShortId countryCode mbPhoneNumber _ = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let totalRides = 5
  let mbShortId = coerce @(ShortId Common.Ride) @(ShortId DRide.Ride) <$> mbRideShortId
  case (mbShortId, mbPhoneNumber) of
    (Just shortId, _) -> do
      ride <- QRide.findRideByRideShortId shortId >>= fromMaybeM (InvalidRequest "Ride ShortId Not Found")
      rideDetail <- rideInfo merchant.id merchantOpCityId (cast ride.id)
      let ticketRideDetail = makeRequiredRideDetail ride.driverId (ride, rideDetail)
      return Common.TicketRideListRes {rides = [ticketRideDetail]}
    (Nothing, Just number) -> do
      no <- getDbHash number
      let code = fromMaybe "+91" countryCode
      person <- runInReplica $ QPerson.findByMobileNumberAndMerchant code no merchant.id >>= fromMaybeM (PersonWithPhoneNotFound number)
      ridesAndBooking <- QRide.findAllByDriverId person.id (Just totalRides) (Just 0) Nothing Nothing Nothing
      let lastNRides = map fst ridesAndBooking
      ridesDetail <- mapM (\ride -> rideInfo merchant.id merchantOpCityId (cast ride.id)) lastNRides
      let rdList = zipWith (curry (makeRequiredRideDetail person.id)) lastNRides ridesDetail
      return Common.TicketRideListRes {rides = rdList}
    (Nothing, Nothing) -> throwError $ InvalidRequest "Ride Short Id or Phone Number Not Received"
  where
    makeRequiredRideDetail :: Id DP.Person -> (DRide.Ride, Common.RideInfoRes) -> Common.RideInfo
    makeRequiredRideDetail personId (ride, detail) =
      Common.RideInfo
        { rideShortId = coerce @(ShortId DRide.Ride) @(ShortId Common.Ride) ride.shortId,
          customerName = detail.customerName,
          customerPhoneNo = detail.customerPhoneNo,
          driverName = detail.driverName,
          driverPhoneNo = detail.driverPhoneNo,
          vehicleNo = detail.vehicleNo,
          status = detail.bookingStatus,
          rideCreatedAt = ride.createdAt,
          pickupLocationLat = Just detail.customerPickupLocation.lat,
          pickupLocationLon = Just detail.customerPickupLocation.lon,
          pickupLocationStreet = detail.customerPickupLocation.street,
          pickupLocationCity = detail.customerPickupLocation.city,
          pickupLocationState = detail.customerPickupLocation.state,
          pickupLocationCountry = detail.customerPickupLocation.country,
          pickupLocationBuilding = detail.customerPickupLocation.building,
          pickupLocationAreaCode = detail.customerPickupLocation.areaCode,
          pickupLocationArea = detail.customerPickupLocation.area,
          dropLocationLat = (.lat) <$> detail.customerDropLocation,
          dropLocationLon = (.lon) <$> detail.customerDropLocation,
          dropLocationStreet = (.street) =<< detail.customerDropLocation,
          dropLocationCity = (.city) =<< detail.customerDropLocation,
          dropLocationState = (.state) =<< detail.customerDropLocation,
          dropLocationCountry = (.country) =<< detail.customerDropLocation,
          dropLocationBuilding = (.building) =<< detail.customerDropLocation,
          dropLocationAreaCode = (.areaCode) =<< detail.customerDropLocation,
          dropLocationArea = (.area) =<< detail.customerDropLocation,
          fare = detail.actualFare,
          personId = cast personId,
          classification = Ticket.DRIVER
        }

---------------------------------------------------------------------------------------------------
getActualRoute :: MonadFlow m => (Maybe Double, Maybe Double, UTCTime, Maybe Double, Maybe CHDriverEda.Status) -> m Common.ActualRoute
getActualRoute (lat, lon, timestamp, accuracy, rideStatus) =
  case (lat, lon) of
    (Just lat', Just lon') -> do
      let rideStatus' = castRideStatus <$> rideStatus
      pure Common.ActualRoute {lat = lat', lon = lon', rideStatus = rideStatus', ..}
    _ -> throwError $ InvalidRequest "Couldn't find driver's location."
  where
    castRideStatus :: CHDriverEda.Status -> Common.Status
    castRideStatus CHDriverEda.ON_RIDE = Common.ON_RIDE
    castRideStatus CHDriverEda.ON_PICKUP = Common.ON_PICKUP
    castRideStatus CHDriverEda.IDLE = Common.IDLE

rideRoute :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow Common.RideRouteRes
rideRoute merchantShortId opCity reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId) $ throwError (RideDoesNotExist rideId.getId)
  -- version 2
  let firstDate = ride.createdAt
      lastDate = addUTCTime (intToNominalDiffTime 86400) ride.createdAt
  driverEdaKafkaList <- CHDriverEda.findAllTuple firstDate lastDate ride.driverId (Just rideId)
  logDebug $ "clickhouse driverEdaKafka res v2: " <> show driverEdaKafkaList
  actualRoute <- mapM getActualRoute driverEdaKafkaList

  when (null actualRoute) $ throwError $ InvalidRequest "No route found for this ride."

  pure
    Common.RideRouteRes
      { actualRoute
      }

---------------------------------------------------------------------
rideInfo ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig]
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Common.Ride ->
  m Common.RideInfoRes
rideInfo merchantId merchantOpCityId reqRideId = do
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  rideDetails <- runInReplica $ QRideDetails.findById rideId >>= fromMaybeM (RideNotFound rideId.getId) -- FIXME RideDetailsNotFound
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound rideId.getId)
  mQuote <- runInReplica $ DQ.findById (Id booking.quoteId)
  let driverId = ride.driverId

  -- merchant access checking
  unless (merchantId == booking.providerId && merchantOpCityId == booking.merchantOperatingCityId) $ throwError (RideDoesNotExist rideId.getId)

  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound rideId.getId)
  mDriverLocation <- do
    driverLocations <- LF.driversLocation [driverId]
    return $ listToMaybe driverLocations

  mbBCReason <-
    if ride.status == DRide.CANCELLED
      then runInReplica $ QBCReason.findByRideId rideId -- it can be Nothing if cancelled by user
      else pure Nothing
  let cancellationReason =
        (coerce @DCReason.CancellationReasonCode @Common.CancellationReasonCode <$>) . join $ mbBCReason <&> (.reasonCode)
  let cancelledBy = castCancellationSource <$> (mbBCReason <&> (.source))
  let cancelledTime = case ride.status of
        DRide.CANCELLED -> Just ride.updatedAt
        _ -> Nothing
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  driverPhoneNo <- mapM decrypt rideDetails.driverNumber
  (nextStopLoc, lastStopLoc) <- case booking.tripCategory of
    DTC.Rental _ -> calculateLocations booking.id booking.stopLocationId
    _ -> return (Nothing, Nothing)
  now <- getCurrentTime
  pure
    Common.RideInfoRes
      { rideId = cast @DRide.Ride @Common.Ride ride.id,
        customerName = booking.riderName,
        customerPhoneNo,
        rideOtp = ride.otp,
        customerPickupLocation = mkLocationAPIEntity booking.fromLocation,
        customerDropLocation = mkLocationAPIEntity <$> booking.toLocation,
        actualDropLocation = ride.tripEndPos,
        driverId = cast @DP.Person @Common.Driver driverId,
        driverName = rideDetails.driverName,
        pickupDropOutsideOfThreshold = ride.pickupDropOutsideOfThreshold,
        driverPhoneNo,
        vehicleNo = rideDetails.vehicleNumber,
        driverStartLocation = ride.tripStartPos,
        driverCurrentLocation = getCoordinates <$> mDriverLocation,
        rideBookingTime = booking.createdAt,
        estimatedDriverArrivalTime = (\quote -> realToFrac quote.durationToPickup `addUTCTime` ride.createdAt) <$> mQuote,
        actualDriverArrivalTime = ride.driverArrivalTime,
        rideStartTime = ride.tripStartTime,
        rideEndTime = ride.tripEndTime,
        rideDistanceEstimated = booking.estimatedDistance,
        rideDistanceActual = roundToIntegral ride.traveledDistance,
        chargeableDistance = ride.chargeableDistance,
        maxEstimatedDistance = highPrecMetersToMeters <$> booking.maxEstimatedDistance,
        estimatedRideDuration = secondsToMinutes <$> booking.estimatedDuration,
        estimatedFare = booking.estimatedFare,
        actualFare = ride.fare,
        driverOfferedFare = (.fareParams.driverSelectedFare) =<< mQuote,
        pickupDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just ride.createdAt),
        rideDuration = timeDiffInMinutes <$> ride.tripEndTime <*> ride.tripStartTime,
        bookingStatus = mkBookingStatus ride now,
        cancelledTime,
        cancelledBy,
        cancellationReason,
        driverInitiatedCallCount = -1,
        scheduledAt = Just booking.startTime,
        tripCategory = castTripCategory booking.tripCategory,
        bookingToRideStartDuration = timeDiffInMinutes <$> ride.tripStartTime <*> (Just booking.createdAt),
        distanceCalculationFailed = ride.distanceCalculationFailed,
        driverDeviatedFromRoute = ride.driverDeviatedFromRoute,
        vehicleVariant = castDVehicleVariant <$> rideDetails.vehicleVariant,
        nextStopLocation = mkLocationAPIEntity <$> nextStopLoc,
        lastStopLocation = mkLocationAPIEntity <$> lastStopLoc,
        endOtp = ride.endOtp
      }

calculateLocations ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id SRB.Booking ->
  Maybe (Id DLoc.Location) ->
  m (Maybe DLoc.Location, Maybe DLoc.Location)
calculateLocations bookingId stopLocationId = do
  maxOrder <- QLM.maxOrderByEntity bookingId.getId
  case stopLocationId of
    Nothing -> do
      lastLoc <- if maxOrder == 0 then pure Nothing else mkLocationFromLocationMapping bookingId.getId maxOrder
      return (Nothing, lastLoc)
    Just nextStopId -> do
      nextLoc <- QL.findById nextStopId
      lastLoc <- mkLocationFromLocationMapping bookingId.getId (maxOrder - 1)
      return (nextLoc, lastLoc)

mkLocationAPIEntity :: DLoc.Location -> Common.LocationAPIEntity
mkLocationAPIEntity DLoc.Location {..} = do
  let DLoc.LocationAddress {..} = address
  Common.LocationAPIEntity {..}

mkLocationFromLocationMapping ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Text ->
  Int ->
  m (Maybe DLoc.Location)
mkLocationFromLocationMapping bookingId order = do
  locMap <- listToMaybe <$> QLM.findByEntityIdOrderAndVersion bookingId order QLM.latestTag
  case locMap of
    Nothing -> pure Nothing
    Just locMap_ -> QL.findById locMap_.locationId

castCancellationSource :: DBCReason.CancellationSource -> Common.CancellationSource
castCancellationSource = \case
  DBCReason.ByUser -> Common.ByUser
  DBCReason.ByDriver -> Common.ByDriver
  DBCReason.ByMerchant -> Common.ByMerchant
  DBCReason.ByAllocator -> Common.ByAllocator
  DBCReason.ByApplication -> Common.ByApplication

castTripCategory :: DTC.TripCategory -> Common.TripCategory
castTripCategory = \case
  DTC.OneWay _ -> Common.OneWay
  DTC.RoundTrip _ -> Common.RoundTrip
  DTC.Rental _ -> Common.Rental
  DTC.RideShare _ -> Common.RideShare
  DTC.InterCity _ -> Common.InterCity

timeDiffInMinutes :: UTCTime -> UTCTime -> Minutes
timeDiffInMinutes t1 = secondsToMinutes . nominalDiffTimeToSeconds . diffUTCTime t1

-- ride considered as ONGOING_6HRS if ride.status = INPROGRESS, but somehow ride.tripStartTime = Nothing
mkBookingStatus :: DRide.Ride -> UTCTime -> Common.BookingStatus
mkBookingStatus ride now = do
  let sixHours = secondsToNominalDiffTime $ Seconds 21600
  let ongoing6HrsCond = maybe True (\tripStartTime -> diffUTCTime now tripStartTime >= sixHours) ride.tripStartTime
  let upcoming6HrsCond = diffUTCTime now ride.createdAt >= sixHours
  case ride.status of
    DRide.NEW | not upcoming6HrsCond -> Common.UPCOMING
    DRide.NEW -> Common.UPCOMING_6HRS
    DRide.INPROGRESS | not ongoing6HrsCond -> Common.ONGOING
    DRide.INPROGRESS -> Common.ONGOING_6HRS
    DRide.COMPLETED -> Common.COMPLETED
    DRide.CANCELLED -> Common.CANCELLED

castDVehicleVariant :: DVeh.Variant -> Common.Variant
castDVehicleVariant = \case
  DVeh.SUV -> Common.SUV
  DVeh.HATCHBACK -> Common.HATCHBACK
  DVeh.SEDAN -> Common.SEDAN
  DVeh.AUTO_RICKSHAW -> Common.AUTO_RICKSHAW
  DVeh.TAXI -> Common.TAXI
  DVeh.TAXI_PLUS -> Common.TAXI_PLUS

---------------------------------------------------------------------
rideSync :: ShortId DM.Merchant -> Context.City -> Id Common.Ride -> Flow Common.RideSyncRes
rideSync merchantShortId opCity reqRideId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let rideId = cast @Common.Ride @DRide.Ride reqRideId
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound rideId.getId)

  -- merchant access checking
  unless (merchant.id == booking.providerId && merchantOpCity == booking.merchantOperatingCityId) $ throwError (RideDoesNotExist rideId.getId)

  logTagInfo "dashboard -> syncRide : " $ show rideId <> "; status: " <> show ride.status

  SyncRide.rideSync Nothing (Just ride) booking merchant

---------------------------------------------------------------------

multipleRideSync :: ShortId DM.Merchant -> Context.City -> Common.MultipleRideSyncReq -> Flow Common.MultipleRideSyncRes
multipleRideSync merchantShortId opCity rideSyncReq = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  let rideIds = map (cast @Common.Ride @DRide.Ride) rideSyncReq.rideIds
  ridesBookingsZip <- QRide.findAllRidesBookingsByRideId merchant.id rideIds

  -- merchant access checking
  for_ ridesBookingsZip $ \(ride, booking) -> do unless (merchant.id == booking.providerId && merchantOpCity.id == booking.merchantOperatingCityId) $ throwError (RideDoesNotExist ride.id.getId)
  logTagInfo "dashboard -> syncRide : " $ show rideIds <> "; status: " <> show (map fun ridesBookingsZip)
  rideDataResult <-
    mapM
      ( \(ride, booking) ->
          mapLeft show
            <$> ( try @_ @SomeException $
                    mkMultipleRideData ride.id <$> SyncRide.rideSync Nothing (Just ride) booking merchant
                )
      )
      ridesBookingsZip
  return
    Common.MultipleRideSyncRes
      { list = rideDataResult
      }
  where
    fun (ride, _) = ride.status

mkMultipleRideData :: Id DRide.Ride -> Common.RideSyncRes -> Common.MultipleRideData
mkMultipleRideData rideId Common.RideSyncRes {..} =
  Common.MultipleRideData
    { rideId = cast @DRide.Ride @Common.Ride rideId,
      ..
    }

---------------------------------------------------------------------
currentActiveRide :: ShortId DM.Merchant -> Text -> Flow (Id Common.Ride)
currentActiveRide _ vehicleNumber = do
  vehicle <- VQuery.findByRegistrationNo vehicleNumber >>= fromMaybeM (VehicleNotFound vehicleNumber)
  activeRide <- runInReplica $ QRide.getActiveByDriverId vehicle.driverId >>= fromMaybeM NoActiveRidePresent
  let rideId = cast @DRide.Ride @Common.Ride activeRide.id
  pure rideId

---------------------------------------------------------------------

bookingWithVehicleNumberAndPhone :: DM.Merchant -> Id DMOC.MerchantOperatingCity -> Common.BookingWithVehicleAndPhoneReq -> Flow Common.BookingWithVehicleAndPhoneRes
bookingWithVehicleNumberAndPhone merchant merchantOpCityId req = do
  alreadyInProcess :: Maybe Bool <- Redis.safeGet apiProcessKey
  if isNothing alreadyInProcess
    then do
      Redis.setExp apiProcessKey True 60
      phoneNumberHash <- getDbHash req.phoneNumber
      person <- QPerson.findByMobileNumberAndMerchant req.countryCode phoneNumberHash merchant.id >>= fromMaybeM (DriverNotFound req.phoneNumber)
      mblinkedVehicle <- VQuery.findById person.id
      mbRecentRide :: Maybe Text <- Redis.safeGet $ SRide.makeStartRideIdKey person.id
      when (isJust mbRecentRide) $ throwError RecentActiveRide
      when req.endRideForVehicle do
        mbvehicle <- VQuery.findByRegistrationNo req.vehicleNumber
        whenJust mbvehicle $ \vehicle -> do
          activeRideId <- runInReplica $ QRide.getActiveByDriverId vehicle.driverId
          whenJust activeRideId $ \rideId -> endActiveRide rideId.id merchant.id merchantOpCityId
      when req.endRideForDriver do
        activeRideId <- runInReplica $ QRide.getActiveByDriverId person.id
        whenJust activeRideId $ \rideId -> endActiveRide rideId.id merchant.id merchantOpCityId
      now <- getCurrentTime
      case mblinkedVehicle of
        Just vehicle -> do
          unless (vehicle.registrationNo == req.vehicleNumber) $ do
            tryLinkinRC person.id merchant.id now
        Nothing -> do
          tryLinkinRC person.id merchant.id now
      Redis.del apiProcessKey
      return
        Common.BookingWithVehicleAndPhoneRes
          { driverId = person.id.getId
          }
    else throwError $ InvalidRequest "Multiple call for same API"
  where
    apiProcessKey = "bookingWithVehicleNumberAndPhone:phoneNumber-" <> (show req.phoneNumber)
    tryLinkinRC personId merchantId now = do
      vehicleRC <- RCQuery.findLastVehicleRCWrapper req.vehicleNumber >>= fromMaybeM VehicleIsNotRegistered
      mRCAssociation <- DAQuery.findLatestByRCIdAndDriverId vehicleRC.id personId
      case mRCAssociation of
        Just assoc -> do
          when (maybe True (now >) assoc.associatedTill) $ -- if that association is old, create new association for that driver
            createRCAssociation personId vehicleRC
        Nothing -> createRCAssociation personId vehicleRC
      let rcStatusReq =
            DomainRC.RCStatusReq
              { rcNo = req.vehicleNumber,
                isActivate = True
              }
      void $ DomainRC.linkRCStatus (personId, merchantId, merchantOpCityId) rcStatusReq
    createRCAssociation driverId rc = do
      driverRCAssoc <- makeRCAssociation driverId rc.id (DomainRC.convertTextToUTC (Just "2099-12-12"))
      DAQuery.create driverRCAssoc

endActiveRide :: Id DRide.Ride -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow ()
endActiveRide rideId merchantId merchantOperatingCityId = do
  let dashboardReq = EHandler.DashboardEndRideReq {point = Nothing, merchantId, merchantOperatingCityId, odometer = Nothing}
  shandle <- EHandler.buildEndRideHandle merchantId merchantOperatingCityId Nothing
  void $ EHandler.dashboardEndRide shandle rideId dashboardReq
