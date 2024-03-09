{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import qualified Client.Main as CM
import Control.Lens.Combinators
import Control.Lens.Fold
import Data.Aeson as DA
import qualified Data.Aeson.Key as DAK
import qualified Data.Aeson.KeyMap as DAKM
import Data.Aeson.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.Text as Text hiding (find)
import qualified Domain.Types.Cac as DTC
import qualified Domain.Types.Common as DTC
import Domain.Types.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as Variant
import EulerHS.Language as L (getOption)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude as KP
import qualified Kernel.Storage.Queries.SystemConfigs as KSQS
import Kernel.Types.Cac
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import Kernel.Utils.Logging
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP
import qualified System.Environment as SE
import qualified System.Environment as Se
import System.Random

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getSearchDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Maybe Meters ->
  m DriverPoolConfig
getSearchDriverPoolConfig merchantOpCityId mbDist = do
  let distance = fromMaybe 0 mbDist
      vehicle = Nothing
      tripCategory = "All"
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  findDriverPoolConfig configs vehicle tripCategory distance

getDriverPoolConfigFromDB ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Variant.Variant ->
  DTC.TripCategory ->
  Maybe Meters ->
  m DriverPoolConfig
getDriverPoolConfigFromDB merchantOpCityId vehicle tripCategory mbDist = do
  let distance = fromMaybe 0 mbDist
  configs <- CDP.findAllByMerchantOpCityId merchantOpCityId
  let mbApplicableConfig = find (filterByDistAndDveh (Just vehicle) (show tripCategory) distance) configs
  case configs of
    [] -> throwError $ InvalidRequest $ "DriverPool Configs not found for MerchantOperatingCity: " <> merchantOpCityId.getId
    _ ->
      case mbApplicableConfig of
        Just applicableConfig -> return applicableConfig
        Nothing -> do
          let alternativeConfigs = find (filterByDistAndDveh (Just vehicle) "All" distance) configs
          case alternativeConfigs of
            Just cfg -> return cfg
            Nothing -> findDriverPoolConfig configs Nothing "All" distance

readWithInfo :: (Read a, Show a) => Value -> a
readWithInfo s = case s of
  String str -> case KP.readMaybe (Text.unpack str) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: " ++ Text.unpack str
  Number scientific -> case KP.readMaybe (show scientific) of
    Just val -> val
    Nothing -> error . Text.pack $ "Failed to parse: " ++ show scientific
  _ -> error $ "Not able to parse value" <> show s

stringValueToObject :: [(Key, Value)] -> [(Key, Value)]
stringValueToObject [] = []
stringValueToObject ((k, v) : xs) =
  case DAK.toText k of
    "distanceBasedBatchSplit" -> ("distanceBasedBatchSplit", toJSON ((readWithInfo v) :: [BatchSplitByPickupDistance])) : stringValueToObject xs
    _ -> (k, v) : stringValueToObject xs

getConfigFromCACStrict :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Variant.Variant -> DTC.TripCategory -> Meters -> m DriverPoolConfig
getConfigFromCACStrict merchantOpCityId mbvt tripCategory dist = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId merchantOpCityId)), (pack "variant", DA.String (Text.pack (show mbvt))), (pack "tripCategory", DA.String (Text.pack (show tripCategory))), (pack "tripDistance", DA.String (Text.pack (show dist)))])
  logDebug $ "the context value is " <> show dpcCond
  tenant <- liftIO (SE.lookupEnv "TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  config <- liftIO $ CM.evalExperimentAsString tenant dpcCond toss
  let res' = (config ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverPoolConfig:") (itraversed . indices (\k -> Text.isPrefixOf "driverPoolConfig:" (DAK.toText k))))
      res'' = stringValueToObject res'
      res = (DA.Object $ DAKM.fromList res'') ^? _JSON :: (Maybe DriverPoolConfig)
  maybe (error "error in fetching the context value driverPoolConfig: ") pure res

helper :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Variant.Variant -> DTC.TripCategory -> Meters -> m DriverPoolConfig
helper merchantOpCityId mbvt tripCategory dist = do
  mbHost <- liftIO $ Se.lookupEnv "CAC_HOST"
  mbInterval <- liftIO $ Se.lookupEnv "CAC_INTERVAL"
  tenant <- liftIO (SE.lookupEnv "TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  config <- KSQS.findById' $ Text.pack tenant
  _ <- initializeCACThroughConfig CM.createClientFromConfig config.configValue tenant (fromMaybe "http://localhost:8080" mbHost) (fromMaybe 10 (readMaybe =<< mbInterval))
  getConfigFromCACStrict merchantOpCityId mbvt tripCategory dist

getDriverPoolConfigFromCAC :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Variant.Variant -> DTC.TripCategory -> Meters -> m DriverPoolConfig
getDriverPoolConfigFromCAC merchantOpCityId mbvt tripCategory dist = do
  dpcCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(pack "merchantOperatingCityId", DA.String (getId merchantOpCityId)), (pack "variant", DA.String (Text.pack (show mbvt))), (pack "tripCategory", DA.String (Text.pack (show tripCategory))), (pack "tripDistance", DA.String (Text.pack (show dist)))])
  tenant <- liftIO (SE.lookupEnv "TENANT") >>= pure . fromMaybe "atlas_driver_offer_bpp_v2"
  gen <- newStdGen
  let (toss, _) = randomR (1, 100) gen :: (Int, StdGen)
  contextValue <- liftIO $ CM.evalExperimentAsString tenant dpcCond toss
  let res' = (contextValue ^@.. _Value . _Object . reindexed (dropPrefixFromConfig "driverPoolConfig:") (itraversed . indices (\k -> Text.isPrefixOf "driverPoolConfig:" (DAK.toText k))))
      res'' = stringValueToObject res'
      res = (DA.Object $ DAKM.fromList res'') ^? _JSON :: (Maybe DriverPoolConfig)
  maybe (helper merchantOpCityId mbvt tripCategory dist) pure res

getConfigFromInMemory :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Variant.Variant -> DTC.TripCategory -> Meters -> m DriverPoolConfig
getConfigFromInMemory id mbvt tripCategory dist = do
  tenant <- liftIO $ Se.lookupEnv "TENANT"
  dpc <- L.getOption DTC.DriverPoolConfig
  isExp <- liftIO $ CM.isExperimentsRunning (fromMaybe "driver_offer_bpp_v2" tenant)
  bool
    ( maybe
        ( getDriverPoolConfigFromCAC id mbvt tripCategory dist
            >>= ( \config -> do
                    L.setOption DTC.DriverPoolConfig config
                    pure config
                )
        )
        pure
        dpc
    )
    (getDriverPoolConfigFromCAC id mbvt tripCategory dist)
    isExp

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Variant.Variant ->
  DTC.TripCategory ->
  Maybe Meters ->
  m DriverPoolConfig
getDriverPoolConfig merchantOpCityId vehicle tripCategory mbDist = do
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (\sc -> sc.useCAC) systemConfigs
  case useCACConfig of
    False -> getDriverPoolConfigFromDB merchantOpCityId vehicle tripCategory mbDist
    True -> getConfigFromInMemory merchantOpCityId vehicle tripCategory $ fromMaybe 0 mbDist

filterByDistAndDveh :: Maybe Variant.Variant -> Text -> Meters -> DriverPoolConfig -> Bool
filterByDistAndDveh vehicle tripCategory dist cfg =
  dist >= cfg.tripDistance && cfg.vehicleVariant == vehicle && cfg.tripCategory == tripCategory

findDriverPoolConfig :: (EsqDBFlow m r) => [DriverPoolConfig] -> Maybe Variant.Variant -> Text -> Meters -> m DriverPoolConfig
findDriverPoolConfig configs vehicle tripCategory dist = do
  find (filterByDistAndDveh vehicle tripCategory dist) configs
    & fromMaybeM (InvalidRequest $ "DriverPool Config not found: " <> show vehicle <> show tripCategory <> show dist)
