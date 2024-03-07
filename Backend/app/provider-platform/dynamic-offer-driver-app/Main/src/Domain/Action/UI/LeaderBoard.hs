{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.LeaderBoard where

import Control.Monad
import Data.Aeson hiding (Success)
import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar ()
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Domain.Action.UI.Ride.EndRide.Internal as RideEndInt
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.LeaderBoardConfigs as LConfig
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.Person as SP
import qualified Kernel.Beam.Functions as B
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Storage.CachedQueries.Merchant.LeaderBoardConfig as QLeaderConfig
import qualified Storage.Queries.Person as QPerson

data DriversInfo = DriversInfo
  { rank :: Integer,
    name :: Text,
    totalRides :: Int,
    totalDistance :: Meters,
    isCurrentDriver :: Bool,
    gender :: Maybe SP.Gender
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data LeaderBoardRes = LeaderBoardRes
  { driverList :: [DriversInfo],
    lastUpdatedAt :: Maybe UTCTime,
    totalEligibleDrivers :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getDailyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  Maybe Text ->
  m LeaderBoardRes
getDailyDriverLeaderBoard (personId, merchantId, merchantOpCityId) day fillData = do
  now <- getCurrentTime
  let currentDate = RideEndInt.getCurrentDate now
  let dateDiff = diffDays currentDate day
  dailyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.DAILY merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless dailyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  let backFill = fromMaybe False fillData
  when backFill
      fork "Daily Backfill thread" $ do
        startDailyBackFill merchantId merchantOpCityId day dailyLeaderBoardConfig
  let useCityBasedLeaderboard = dailyLeaderBoardConfig.useOperatingCityBasedLeaderBoard
  let numberOfSets = fromIntegral dailyLeaderBoardConfig.numberOfSets
  when (dateDiff > numberOfSets - 1 || dateDiff < 0) $
    throwError $ InvalidRequest "Date outside Range"
  driversWithScoresMap :: [(Text, Double)] <- concat <$> Redis.withNonCriticalRedis (Redis.get $ RideEndInt.makeCachedDailyDriverLeaderBoardKey dailyLeaderBoardConfig.useOperatingCityBasedLeaderBoard merchantId merchantOperatingCityId day)
  let driverIds = map (Id . fst) driversWithScoresMap
  driverDetailsMap :: HM.HashMap Text (Text, Gender) <- HM.fromList . map (\driver -> (driver.id.getId, (fromMaybe "Driver" $ getPersonFullName driver, driver.gender))) <$> B.runInReplica (QPerson.getDriversByIdIn driverIds)
  (drivers', isCurrentDriverInTop) <-
    foldlM
      ( \(acc, isCurrentDriverInTop) ((driverId, score), index) -> do
          (fullName, gender) <- HM.lookup driverId driverDetailsMap & fromMaybeM (PersonFieldNotPresent "DriverDetails")
          let (totalRides, totalDistance) = RideEndInt.getRidesAndDistancefromZscore score dailyLeaderBoardConfig.zScoreBase
          let isCurrentDriver = personId.getId == driverId
          pure
            ( acc
                <> [ DriversInfo
                       { rank = index,
                         name = fullName,
                         gender = Just gender,
                         ..
                       }
                   ],
              isCurrentDriverInTop || isCurrentDriver
            )
      )
      ([], False)
      (zip driversWithScoresMap [1, 2 ..])
  totalEligibleDrivers <- Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeDailyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId day)
  if not isCurrentDriverInTop && dateDiff == 0
    then do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mbCurrPersonRank <- Redis.withNonCriticalRedis $ Redis.zRevRank (RideEndInt.makeDailyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId day) personId.getId
      mbCurrDriverZscore <- Redis.withNonCriticalRedis $ Redis.zScore (RideEndInt.makeDailyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId day) personId.getId
      let currentDriverScore = fromMaybe 0 mbCurrDriverZscore
      currPersonRank <-
        case mbCurrPersonRank of
          Nothing -> Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeDailyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId day)
          Just rank -> pure rank
      let (currPersonTotalRides, currPersonTotalDistance) = RideEndInt.getRidesAndDistancefromZscore currentDriverScore dailyLeaderBoardConfig.zScoreBase
      currPersonFullName <- getPersonFullName person & fromMaybeM (PersonFieldNotPresent "firstName")
      let currDriverInfo = DriversInfo (currPersonRank + 1) currPersonFullName currPersonTotalRides currPersonTotalDistance True (Just person.gender)
      return $ LeaderBoardRes (currDriverInfo : drivers') (Just now) (fromIntegral totalEligibleDrivers)
    else return $ LeaderBoardRes drivers' (Just now) (fromIntegral totalEligibleDrivers)

getYearFromDay :: Day -> Integer
getYearFromDay day = let (year, _, _) = toGregorian day in year

getLastDayOfYear :: Integer -> Day
getLastDayOfYear year = fromGregorian year 12 31

getWeeklyDriverLeaderBoard ::
  (Esq.EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) =>
  (Id Person, Id DM.Merchant, Id DMOC.MerchantOperatingCity) ->
  Day ->
  Day ->
  Maybe Text ->
  m LeaderBoardRes
getWeeklyDriverLeaderBoard (personId, merchantId, merchantOpCityId) fromDate toDate _ = do
  now <- getCurrentTime
  let currentDate = RideEndInt.getCurrentDate now
  let (currWeekNumber, _) = sundayStartWeek currentDate
  let (reqWeekNumber, reqDayIndex) = sundayStartWeek fromDate
  let (lastWeekOfYear, _) = sundayStartWeek $ getLastDayOfYear $ getYearFromDay fromDate
  let weekDiff = (currWeekNumber - reqWeekNumber + lastWeekOfYear) `mod` lastWeekOfYear
  weeklyLeaderBoardConfig <- QLeaderConfig.findLeaderBoardConfigbyType LConfig.WEEKLY merchantOpCityId >>= fromMaybeM (InternalError "Leaderboard configs not present")
  unless weeklyLeaderBoardConfig.isEnabled . throwError $ InvalidRequest "Leaderboard Not Available"
  let numberOfSets = weeklyLeaderBoardConfig.numberOfSets
  when (weekDiff > numberOfSets - 1 || weekDiff < 0) $
    throwError $ InvalidRequest "Week outside Range"
  when (diffDays toDate fromDate /= 6 || reqDayIndex /= 0) $
    throwError $ InvalidRequest "Invalid Input"
  -- let backFill = fromMaybe False fillData
  -- when backFill
  --   fork "Weekly Backfill thread" $ do
  --     startWeeklyBackFill merchantId merchantOpCityId fromDate toDate weeklyLeaderBoardConfig
  let useCityBasedLeaderboard = weeklyLeaderBoardConfig.useOperatingCityBasedLeaderBoard
  driversWithScoresMap :: [(Text, Double)] <- concat <$> Redis.withNonCriticalRedis (Redis.get $ RideEndInt.makeCachedWeeklyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId fromDate toDate)
  let driverIds = map (Id . fst) driversWithScoresMap
  driverDetailsMap <- HM.fromList . map (\driver -> (driver.id.getId, (fromMaybe "Driver" $ getPersonFullName driver, driver.gender))) <$> B.runInReplica (QPerson.getDriversByIdIn driverIds)
  (drivers', isCurrentDriverInTop) <-
    foldlM
      ( \(acc, isCurrentDriverInTop) ((driverId, score), index) -> do
          (fullName, gender) <- HM.lookup driverId driverDetailsMap & fromMaybeM (PersonFieldNotPresent "DriverDetails")
          let (totalRides, totalDistance) = RideEndInt.getRidesAndDistancefromZscore score weeklyLeaderBoardConfig.zScoreBase
          let isCurrentDriver = personId.getId == driverId
          pure
            ( acc
                <> [ DriversInfo
                       { rank = index,
                         name = fullName,
                         gender = Just gender,
                         ..
                       }
                   ],
              isCurrentDriverInTop || isCurrentDriver
            )
      )
      ([], False)
      (zip driversWithScoresMap [1, 2 ..])
  totalEligibleDrivers <- Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeWeeklyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId fromDate toDate)
  if not isCurrentDriverInTop && weekDiff == 0
    then do
      person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
      mbCurrPersonRank <- Redis.withNonCriticalRedis $ Redis.zRevRank (RideEndInt.makeWeeklyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId fromDate toDate) personId.getId
      mbCurrDriverZscore <- Redis.withNonCriticalRedis $ Redis.zScore (RideEndInt.makeWeeklyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId fromDate toDate) personId.getId
      let currDriverZscore = fromMaybe 0 mbCurrDriverZscore
      currPersonRank <-
        case mbCurrPersonRank of
          Nothing -> Redis.withNonCriticalRedis $ Redis.zCard (RideEndInt.makeWeeklyDriverLeaderBoardKey useCityBasedLeaderboard merchantId merchantOpCityId fromDate toDate)
          Just rank -> pure rank
      let (currPersonTotalRides, currPersonTotalDistance) = RideEndInt.getRidesAndDistancefromZscore currDriverZscore weeklyLeaderBoardConfig.zScoreBase
      currPersonFullName <- getPersonFullName person & fromMaybeM (PersonFieldNotPresent "firstName")
      let currDriverInfo = DriversInfo (currPersonRank + 1) currPersonFullName currPersonTotalRides currPersonTotalDistance True (Just person.gender)
      return $ LeaderBoardRes (currDriverInfo : drivers') (Just now) (fromIntegral totalEligibleDrivers)
    else return $ LeaderBoardRes drivers' (Just now) (fromIntegral totalEligibleDrivers)

startDailyBackFill :: (Esq.EsqDBFlow m r, EncFlow m r, Redis.HedisFlow m r, CacheFlow m r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Day -> LConfig.LeaderBoardConfig -> m ()
startDailyBackFill merchantId merchantOpCityId day config = do
  oldDriverScoresMap :: [(Text, Double)] <- Redis.zrevrangeWithscores (RideEndInt.makeDailyDriverLeaderBoardKey (Just False) merchantId merchantOpCityId day) 0 3000000
  let oldDriverScoreMapBatchList = chunksOf 10 oldDriverScoresMap
  backfillData oldDriverScoreMapBatchList
  let limit = config.leaderBoardLengthLimit
  driversListWithScores <- Redis.zrevrangeWithscores (RideEndInt.makeDailyDriverLeaderBoardKey (Just True) merchantId merchantOpCityId day) 0 (limit -1)
  Redis.setExp (RideEndInt.makeCachedDailyDriverLeaderBoardKey (Just True) merchantId merchantOpCityId day) driversListWithScores (config.leaderBoardExpiry.getSeconds * config.numberOfSets)
  where
    backfillData [] = pure ()
    backfillData (driverBatch : remaining) = pure ()
      let driverIds = map (Id . fst) driverBatch
      let driverScores = map snd driverBatch
      driverIdToCityMapping <- HM.fromList . map (\driver -> (driver.id, driver.merchantOperatingCityId)) <$> B.runInReplica (QPerson.getDriversByIdIn driverIds)
      fillData driverIds driverScores driverIdToCityMapping
    fillData [] _ _ = pure ()
    fillData _ [] _ = pure ()
    fillData (driverId:rem) (score:restScr) driverIdToCityMapping = do
      opcityid <- HM.lookup driverId driverIdToCityMapping
      case opcityid of
        Just cityId -> do
          Hedis.zAddExp (RideEndInt.makeDailyDriverLeaderBoardKey (Just True) merchantId merchantOpCityId day) driverId.getId (double2Int score) 86400
          fillData rem rest driverIdToCityMapping
        Nothing -> fillData rem rest driverIdToCityMapping