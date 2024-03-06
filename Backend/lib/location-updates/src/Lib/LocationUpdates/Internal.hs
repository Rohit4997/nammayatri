{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.LocationUpdates.Internal
  ( RideInterpolationHandler (..),
    recalcDistanceBatches,
    addPointsImplementation,
    getWaypointsNumberImplementation,
    getFirstNwaypointsImplementation,
    getAllWaypointsImplementation,
    deleteFirstNwaypointsImplementation,
    interpolatePointsAndCalculateDistanceImplementation,
    clearLocationUpdatesImplementation,
    addInterpolatedPointsImplementation,
    clearInterpolatedPointsImplementation,
    expireInterpolatedPointsImplementation,
    getInterpolatedPointsImplementation,
    isDistanceCalculationFailedImplementation,
    wrapDistanceCalculationImplementation,
    processWaypoints,
    mkRideInterpolationHandler,
  )
where

import qualified Control.Monad.Catch as C
import qualified Data.List.NonEmpty as NE
import EulerHS.Prelude hiding (id, state)
import GHC.Records.Extra
import Kernel.External.Maps as Maps
import Kernel.Storage.Hedis
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id (Id)
import Kernel.Utils.Common

data RideInterpolationHandler person m = RideInterpolationHandler
  { batchSize :: Integer,
    addPoints :: Id person -> NonEmpty LatLong -> m (),
    clearLocationUpdates :: Id person -> m (),
    getWaypointsNumber :: Id person -> m Integer,
    getFirstNwaypoints :: Id person -> Integer -> m [LatLong],
    getAllWaypoints :: Id person -> m [LatLong],
    deleteFirstNwaypoints :: Id person -> Integer -> m (),
    addInterpolatedPoints :: Id person -> NonEmpty LatLong -> m (),
    clearInterpolatedPoints :: Id person -> m (),
    expireInterpolatedPoints :: Id person -> m (),
    getInterpolatedPoints :: Id person -> m [LatLong],
    interpolatePointsAndCalculateDistance :: [LatLong] -> Bool -> m (HighPrecMeters, [LatLong], [MapsService], Bool),
    wrapDistanceCalculation :: Id person -> m () -> m (),
    isDistanceCalculationFailed :: Id person -> m Bool,
    updateDistance :: Id person -> HighPrecMeters -> Int -> Int -> m (),
    updateRouteDeviation :: Id person -> [LatLong] -> m Bool,
    updateSafetyDeviation :: Id person -> [LatLong] -> m Bool,
    checkBatchForDeviation :: Id person -> [LatLong] -> m Bool,
    getTravelledDistance :: Id person -> Meters -> m Meters,
    getEstimateSliceDistance :: Id person -> [LatLong] -> m Meters
  }

--------------------------------------------------------------------------------

wrapDistanceCalculationImplementation :: (C.MonadMask m, Log m, HedisFlow m r) => Id person -> m () -> m ()
wrapDistanceCalculationImplementation driverId action =
  action `C.catchAll` \e -> C.mask_ $ do
    logError $ "failed distance calculation: " <> show e
    let oneDayInSeconds = 60 * 60 * 24
    Hedis.setExp (getFailedDistanceCalculationKey driverId) () oneDayInSeconds

getFailedDistanceCalculationKey :: Id person -> Text
getFailedDistanceCalculationKey driverId = mconcat [driverId.getId, ":locationUpdatesFailed"]

isDistanceCalculationFailedImplementation :: (HedisFlow m r) => Id person -> m Bool
isDistanceCalculationFailedImplementation driverId = isJust <$> Hedis.get @() (getFailedDistanceCalculationKey driverId)

resetFailedDistanceCalculationFlag :: (HedisFlow m r) => Id person -> m ()
resetFailedDistanceCalculationFlag driverId = Hedis.del $ getFailedDistanceCalculationKey driverId

processWaypoints ::
  (CacheFlow m r, Log m, MonadThrow m) =>
  RideInterpolationHandler person m ->
  Id person ->
  Bool ->
  Meters ->
  Bool ->
  NonEmpty LatLong ->
  m ()
processWaypoints ih@RideInterpolationHandler {..} driverId ending estDist pickupDropOutsideThreshold waypoints = do
  prevBatchTwoEndPoints :: Maybe [LatLong] <- Redis.safeGet $ lastTwoOnRidePointsRedisKey driverId
  let modifiedWaypoints =
        case prevBatchTwoEndPoints of
          Just points -> NE.fromList points <> waypoints
          _ -> waypoints
  let currentLastTwoPoints = takeLastTwo (toList waypoints)
  Redis.setExp (lastTwoOnRidePointsRedisKey driverId) currentLastTwoPoints 21600 -- 6 hours
  _ <- updateSafetyDeviation driverId (toList modifiedWaypoints)
  calculationFailed <- isDistanceCalculationFailed driverId
  if calculationFailed
    then logWarning "Failed to calculate actual distance for this ride, ignoring"
    else wrapDistanceCalculation driverId $ do
      addPoints driverId waypoints
      recalcDistanceBatches ih ending driverId estDist pickupDropOutsideThreshold modifiedWaypoints

lastTwoOnRidePointsRedisKey :: Id person -> Text
lastTwoOnRidePointsRedisKey driverId = "Driver-Location-Last-Two-OnRide-Points:DriverId-" <> driverId.getId

data SnapToRoadState = SnapToRoadState
  { distanceTravelled :: HighPrecMeters,
    googleSnapToRoadCalls :: Int,
    osrmSnapToRoadCalls :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

onRideSnapToRoadStateKey :: Id person -> Text
onRideSnapToRoadStateKey driverId = "Driver-Location-OnRide-SnapToRoad:DriverId-" <> driverId.getId

takeLastTwo :: [a] -> [a]
takeLastTwo xs = drop (max 0 (length xs - 2)) xs

fromBool :: Num a => Bool -> a
fromBool False = 0
fromBool True = 1

recalcDistanceBatches ::
  (CacheFlow m r, Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Bool ->
  Id person ->
  Meters ->
  Bool ->
  NonEmpty LatLong ->
  m ()
recalcDistanceBatches h@RideInterpolationHandler {..} ending driverId estDist pickupDropOutsideThreshold waypoints = do
  routeDeviation <- updateRouteDeviation driverId (toList waypoints)
  let snapToRoadCallCondition = routeDeviation || pickupDropOutsideThreshold
  if ending
    then do
      if snapToRoadCallCondition
        then do
          currSnapToRoadState <- processSnapToRoadCall
          updateDistance driverId currSnapToRoadState.distanceTravelled currSnapToRoadState.googleSnapToRoadCalls currSnapToRoadState.osrmSnapToRoadCalls
        else do
          distanceToBeUpdated <- getTravelledDistance driverId estDist
          updateDistance driverId (metersToHighPrecMeters distanceToBeUpdated) 0 0
    else do
      isAtLeastBatchPlusOne <- atLeastBatchPlusOne
      when (snapToRoadCallCondition && isAtLeastBatchPlusOne) $ do
        currSnapToRoadState <- processSnapToRoadCall
        Redis.setExp (onRideSnapToRoadStateKey driverId) currSnapToRoadState 21600 -- 6 hours
  where
    pointsRemaining = (> 0) <$> getWaypointsNumber driverId
    continueCondition = if ending then pointsRemaining else atLeastBatchPlusOne
    atLeastBatchPlusOne = (> batchSize) <$> getWaypointsNumber driverId

    recalcDistanceBatches' snapToRoad'@SnapToRoadState {..} snapToRoadCallFailed = do
      batchLeft <- continueCondition
      if batchLeft
        then do
          (dist, servicesUsed, snapCallFailed) <- recalcDistanceBatchStep h driverId pickupDropOutsideThreshold
          let googleCalled = Google `elem` servicesUsed
              osrmCalled = OSRM `elem` servicesUsed
          if snapCallFailed
            then pure (SnapToRoadState distanceTravelled (googleSnapToRoadCalls + fromBool googleCalled) (osrmSnapToRoadCalls + fromBool osrmCalled), snapCallFailed)
            else recalcDistanceBatches' (SnapToRoadState (distanceTravelled + dist) (googleSnapToRoadCalls + fromBool googleCalled) (osrmSnapToRoadCalls + fromBool osrmCalled)) snapCallFailed
        else pure (snapToRoad', snapToRoadCallFailed)

    processSnapToRoadCall = do
      prevSnapToRoadState :: SnapToRoadState <- (Redis.safeGet $ onRideSnapToRoadStateKey driverId) >>= pure . fromMaybe (SnapToRoadState 0 0 0)
      (currSnapToRoadState, snapToRoadCallFailed) <- recalcDistanceBatches' prevSnapToRoadState False
      when snapToRoadCallFailed $ do
        updateDistance driverId currSnapToRoadState.distanceTravelled currSnapToRoadState.googleSnapToRoadCalls currSnapToRoadState.osrmSnapToRoadCalls
        throwError $ InternalError $ "Snap to road call failed for driverId =" <> driverId.getId
      return currSnapToRoadState

calculateDistanceByCallingSnapToRoad :: (Monad m, Log m) => RideInterpolationHandler person m -> Id person -> [LatLong] -> m (HighPrecMeters, [MapsService], Bool)
calculateDistanceByCallingSnapToRoad h@RideInterpolationHandler {..} driverId batchwaypoints = do
  (distance, interpolatedWps, servicesUsed, snapToRoadFailed) <- interpolatePointsAndCalculateDistance batchwaypoints False
  unless snapToRoadFailed $ do
    addInterpolatedPointsAndDeleteBatchWaypoints h driverId batchwaypoints interpolatedWps distance
  pure (distance, servicesUsed, snapToRoadFailed)

calculateDistanceByCallingOSRMWithEstimatedFallBack :: (Monad m, Log m) => RideInterpolationHandler person m -> Id person -> [LatLong] -> m (HighPrecMeters, [MapsService], Bool)
calculateDistanceByCallingOSRMWithEstimatedFallBack h@RideInterpolationHandler {..} driverId batchwaypoints = do
  (distance, interpolatedWps, servicesUsed, snapToRoadFailed) <- interpolatePointsAndCalculateDistance batchwaypoints True
  case snapToRoadFailed of
    True -> do
      distance <- getEstimateSliceDistance driverId batchwaypoints
      addInterpolatedPointsAndDeleteBatchWaypoints h driverId batchwaypoints interpolatedWps (metersToHighPrecMeters distance)
      pure ((metersToHighPrecMeters distance), servicesUsed, False)
    False -> do
      addInterpolatedPointsAndDeleteBatchWaypoints h driverId batchwaypoints interpolatedWps distance
      pure (distance, servicesUsed, False)
  where
    getProvidersList = pure [OSRM]

recalcDistanceBatchStep ::
  (Monad m, Log m) =>
  RideInterpolationHandler person m ->
  Id person ->
  Bool ->
  m (HighPrecMeters, [MapsService], Bool)
recalcDistanceBatchStep h@RideInterpolationHandler {..} driverId pickupDropOutsideThreshold = do
  batchwaypoints <- getFirstNwaypoints driverId (batchSize + 1)
  case pickupDropOutsideThreshold of
    True -> calculateDistanceByCallingSnapToRoad h driverId batchwaypoints
    False -> do
      batchDeviation <- checkBatchForDeviation driverId batchwaypoints
      case batchDeviation of
        True -> calculateDistanceByCallingSnapToRoad h driverId batchwaypoints
        False -> calculateDistanceByCallingOSRMWithEstimatedFallBack h driverId batchwaypoints

redisOnRideSnapToRoadKeysCleanup :: (HedisFlow m env) => Id person -> m ()
redisOnRideSnapToRoadKeysCleanup driverId = do
  Redis.del (lastTwoOnRidePointsRedisKey driverId)
  Redis.del (onRideSnapToRoadStateKey driverId)

addInterpolatedPointsAndDeleteBatchWaypoints :: (HedisFlow m env) => RideInterpolationHandler person m -> Id person -> [LatLong] -> [LatLong] -> HighPrecMeters -> m ()
addInterpolatedPointsAndDeleteBatchWaypoints h@RideInterpolationHandler {..} driverId batchwaypoints interpolatedWps distance = do
  logInfo $ mconcat ["points interpolation: input=", show batchwaypoints, "; output=", show interpolatedWps]
  logInfo $ mconcat ["calculated distance for ", show (length interpolatedWps), " points, ", "distance is ", show distance]
  addInterpolatedPoints driverId $ NE.fromList interpolatedWps
  deleteFirstNwaypoints driverId batchSize

mkRideInterpolationHandler ::
  ( HedisFlow m r,
    HasPrettyLogger m r,
    HasCallStack,
    Metrics.CoreMetrics m,
    EncFlow m r,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters]
  ) =>
  Bool ->
  (Id person -> HighPrecMeters -> Int -> Int -> m ()) ->
  (Id person -> [LatLong] -> m Bool) ->
  (Id person -> [LatLong] -> m Bool) ->
  (Id person -> [LatLong] -> m Bool) ->
  (Id person -> Meters -> m Meters) ->
  (Id person -> [LatLong] -> m Bool) ->
  (Maps.SnapToRoadReq -> Bool -> m ([Maps.MapsService], Either String Maps.SnapToRoadResp)) ->
  RideInterpolationHandler person m
mkRideInterpolationHandler isEndRide updateDistance updateRouteDeviation updateSafetyDeviation checkBatchForDeviation getTravelledDistance getEstimateSliceDistance snapToRoadCall =
  RideInterpolationHandler
    { batchSize = 98,
      addPoints = addPointsImplementation,
      clearLocationUpdates = clearLocationUpdatesImplementation,
      getWaypointsNumber = getWaypointsNumberImplementation,
      getFirstNwaypoints = getFirstNwaypointsImplementation,
      getAllWaypoints = getAllWaypointsImplementation,
      deleteFirstNwaypoints = deleteFirstNwaypointsImplementation,
      addInterpolatedPoints = addInterpolatedPointsImplementation,
      clearInterpolatedPoints = clearInterpolatedPointsImplementation,
      getInterpolatedPoints = getInterpolatedPointsImplementation,
      expireInterpolatedPoints = expireInterpolatedPointsImplementation,
      interpolatePointsAndCalculateDistance = interpolatePointsAndCalculateDistanceImplementation isEndRide snapToRoadCall,
      isDistanceCalculationFailed = isDistanceCalculationFailedImplementation,
      wrapDistanceCalculation = wrapDistanceCalculationImplementation,
      ..
    }

makeWaypointsRedisKey :: Id person -> Text
makeWaypointsRedisKey driverId = mconcat ["waypoints", ":", driverId.getId]

addPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addPointsImplementation driverId waypoints = do
  let key = makeWaypointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " points for driverId = ", driverId.getId]

clearLocationUpdatesImplementation :: (HedisFlow m env) => Id person -> m ()
clearLocationUpdatesImplementation driverId = do
  let key = makeWaypointsRedisKey driverId
  clearList key
  logInfo $ mconcat ["cleared location updates for driverId = ", driverId.getId]
  resetFailedDistanceCalculationFlag driverId

getWaypointsNumberImplementation :: (HedisFlow m env) => Id person -> m Integer
getWaypointsNumberImplementation = lLen . makeWaypointsRedisKey

getFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m [LatLong]
getFirstNwaypointsImplementation driverId num = lRange (makeWaypointsRedisKey driverId) 0 (num - 1)

getAllWaypointsImplementation :: (HedisFlow m env) => Id person -> m [LatLong]
getAllWaypointsImplementation driverId = lRange (makeWaypointsRedisKey driverId) 0 (-1)

deleteFirstNwaypointsImplementation :: (HedisFlow m env) => Id person -> Integer -> m ()
deleteFirstNwaypointsImplementation driverId numToDel = lTrim (makeWaypointsRedisKey driverId) numToDel (-1)

makeInterpolatedPointsRedisKey :: Id person -> Text
makeInterpolatedPointsRedisKey driverId = mconcat ["interpolatedPoints", ":", driverId.getId]

addInterpolatedPointsImplementation :: (HedisFlow m env) => Id person -> NonEmpty LatLong -> m ()
addInterpolatedPointsImplementation driverId waypoints = do
  let key = makeInterpolatedPointsRedisKey driverId
      numPoints = length waypoints
  rPush key waypoints
  logInfo $ mconcat ["added ", show numPoints, " interpolated points for driverId = ", driverId.getId]

clearInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
clearInterpolatedPointsImplementation driverId = do
  let key = makeInterpolatedPointsRedisKey driverId
  clearList key
  redisOnRideSnapToRoadKeysCleanup driverId
  logInfo $ mconcat ["cleared interpolated location updates for driverId = ", driverId.getId]

getInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m [LatLong]
getInterpolatedPointsImplementation = Hedis.getList . makeInterpolatedPointsRedisKey

expireInterpolatedPointsImplementation :: HedisFlow m env => Id person -> m ()
expireInterpolatedPointsImplementation driverId = do
  let key = makeInterpolatedPointsRedisKey driverId
  Hedis.expire key 86400 -- 24 hours

interpolatePointsAndCalculateDistanceImplementation ::
  ( HasCallStack,
    EncFlow m r,
    Metrics.CoreMetrics m,
    HasFlowEnv m r '["snapToRoadSnippetThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["droppedPointsThreshold" ::: HighPrecMeters],
    HasFlowEnv m r '["osrmMatchThreshold" ::: HighPrecMeters]
  ) =>
  Bool ->
  (Maps.SnapToRoadReq -> Bool -> m ([Maps.MapsService], Either String Maps.SnapToRoadResp)) ->
  [LatLong] ->
  Bool ->
  m (HighPrecMeters, [LatLong], [Maps.MapsService], Bool)
interpolatePointsAndCalculateDistanceImplementation isEndRide snapToRoadCall wps isBatchSnap = do
  if isEndRide && isAllPointsEqual wps
    then pure (0, take 1 wps, [], False)
    else do
      (servicesUsed, res) <- snapToRoadCall (Maps.SnapToRoadReq {points = wps}) isBatchSnap
      case res of
        Left _ -> pure (0, [], [], True)
        Right response -> pure (response.distance, response.snappedPoints, servicesUsed, False)

isAllPointsEqual :: [LatLong] -> Bool
isAllPointsEqual [] = True
isAllPointsEqual [_] = True
isAllPointsEqual (x : xs) = all (\t -> (abs (x.lat - t.lat) <= eps) && (abs (x.lon - t.lon) <= eps)) xs

eps :: Double
eps = 0.0001
