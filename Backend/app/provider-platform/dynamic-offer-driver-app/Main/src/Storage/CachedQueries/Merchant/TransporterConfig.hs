{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.TransporterConfig
  ( create,
    findByMerchantOpCityId,
    clearCache,
    update,
    updateFCMConfig,
    updateReferralLinkPassword,
  )
where

import qualified Client.Main as CM
import qualified Data.Aeson as DA
-- import Domain.Types.Merchant.DriverPoolConfig as DPC

-- import Data.Aeson.Key
-- import qualified Data.Time.Clock as DTC
-- import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.Text.Encoding as DTE
import Data.Aeson.Types as DAT
-- import Data.Coerce (coerce)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
-- import Domain.Types.Common
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.TransporterConfig
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Merchant.TransporterConfig as Queries

create :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
create = Queries.create
import Domain.Types.Person
-- import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Random
import qualified System.Environment as Se



getConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity ->  Int  -> m (Maybe TransporterConfig)
getConfig id toss = do
  confCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(Text.pack "merchantOperatingCityId", DA.String (getId id))])
  logDebug $ "transporterConfig Cond: " <> show confCond
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  context' <- liftIO $ CM.evalExperiment (fromMaybe "driver_offer_bpp_v2" tenant)  confCond toss
  logDebug $ "transporterConfig: " <> show context'
  let ans = case context' of
        Left err -> error $ (Text.pack "error in fetching the context value ") <> (Text.pack err)
        Right contextValue' ->
          case (DAT.parse jsonToTransporterConfig contextValue') of
            Success dpc -> dpc
            DAT.Error err -> error $ (Text.pack "error in parsing the context value for transporter config ") <> (Text.pack err)
  -- pure $ Just ans
  logDebug $ "transporterConfig: " <> show ans
  pure $ Just ans

findByMerchantOpCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Maybe (Id Person)-> m (Maybe TransporterConfig)
findByMerchantOpCityId id (Just personId) =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeCACTransporterConfigKey personId) >>= \case
    Just (a :: Int) -> do
      getConfig id a
    Nothing -> do
      gen <- newStdGen
      let (toss,_) = randomR (1, 100) gen :: (Int, StdGen)
      logDebug $ "the toss value is for transporter config " <> show toss
      _ <- cacheToss personId toss
      getConfig id toss

findByMerchantOpCityId id Nothing = do
  gen <- newStdGen
  let (toss,_) = randomR (1, 100) gen :: (Int, StdGen)
  logDebug $ "the toss value is for transporter config " <> show toss
  tenant <- liftIO $ Se.lookupEnv "DRIVER_TENANT"
  confCond <- liftIO $ CM.hashMapToString $ HashMap.fromList ([(Text.pack "merchantOperatingCityId", DA.String (getId id))])
  logDebug $ "transporterConfig Cond: " <> show confCond
  context' <- liftIO $ CM.evalExperiment (fromMaybe "driver_offer_bpp_v2" tenant)  confCond toss
  logDebug $ "transporterConfig: " <> show context'
  let ans = case context' of
        Left err -> error $ (Text.pack "error in fetching the context value ") <> (Text.pack err)
        Right contextValue' ->
          case (DAT.parse jsonToTransporterConfig contextValue') of
            Success dpc -> dpc
            DAT.Error err -> error $ (Text.pack "error in parsing the context value for transporter config ") <> (Text.pack err)
  -- pure $ Just ans
  logDebug $ "transporterConfig: " <> show ans
  pure $ Just ans
      

-- cacheTransporterConfig :: (CacheFlow m r) => TransporterConfig -> m ()
-- cacheTransporterConfig cfg = do
--   expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
--   let merchantIdKey = makeMerchantOpCityIdKey cfg.merchantOperatingCityId
--   Hedis.withCrossAppRedis $ Hedis.setExp merchantIdKey (coerce @TransporterConfig @(TransporterConfigD 'Unsafe) cfg) expTime


cacheToss :: (CacheFlow m r) => Id Person -> Int -> m ()
cacheToss personId toss = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeCACTransporterConfigKey personId) toss expTime

makeCACTransporterConfigKey :: Id Person -> Text
makeCACTransporterConfigKey id = "driver-offer:CAC:CachedQueries:TransporterConfig:PersonId-" <> id.getId

makeMerchantOpCityIdKey :: Id MerchantOperatingCity -> Text
makeMerchantOpCityIdKey id = "driver-offer:CachedQueries:TransporterConfig:MerchantOperatingCityId-" <> id.getId

-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> m ()
clearCache = Hedis.withCrossAppRedis . Hedis.del . makeMerchantOpCityIdKey

updateFCMConfig :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> BaseUrl -> Text -> m ()
updateFCMConfig = Queries.updateFCMConfig

updateReferralLinkPassword :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> Text -> m ()
updateReferralLinkPassword = Queries.updateReferralLinkPassword

update :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => TransporterConfig -> m ()
update = Queries.update
