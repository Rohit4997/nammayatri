{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.OnboardingDocumentConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Data.Aeson (fromJSON)
import qualified Data.Aeson as A
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.OnboardingDocumentConfig
import qualified Domain.Types.Merchant.OnboardingDocumentConfig as Domain
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.OnboardingDocumentConfig as BeamODC

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => OnboardingDocumentConfig -> m ()
create = createWithKV

findAllByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m [OnboardingDocumentConfig]
findAllByMerchantOpCityId (Id merchantOperatingCityId) = findAllWithKV [Se.Is BeamODC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => OnboardingDocumentConfig -> m ()
update config = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamODC.checkExtraction (config.checkExtraction),
      Se.Set BeamODC.checkExpiry (config.checkExpiry),
      Se.Set BeamODC.supportedVehicleClassesJSON $ BeamODC.getConfigJSON config.supportedVehicleClasses,
      Se.Set BeamODC.vehicleClassCheckType (config.vehicleClassCheckType),
      Se.Set BeamODC.rcNumberPrefix (config.rcNumberPrefix),
      Se.Set BeamODC.rcNumberPrefixList (config.rcNumberPrefixList),
      Se.Set BeamODC.maxRetryCount (config.maxRetryCount),
      Se.Set BeamODC.dlNumberVerification (config.dlNumberVerification),
      Se.Set BeamODC.updatedAt now
    ]
    [Se.Is BeamODC.merchantOperatingCityId $ Se.Eq $ getId config.merchantOperatingCityId, Se.Is BeamODC.documentType $ Se.Eq config.documentType]

updateSupportedVehicleClassesJSON :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> SupportedVehicleClasses -> m ()
updateSupportedVehicleClassesJSON merchantOperatingCityId supportedVehicleClasses = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamODC.supportedVehicleClassesJSON $ BeamODC.getConfigJSON supportedVehicleClasses,
      Se.Set BeamODC.updatedAt now
    ]
    [ Se.Is BeamODC.merchantOperatingCityId $ Se.Eq $ getId merchantOperatingCityId,
      Se.Is BeamODC.documentType $ Se.Eq RC
    ]

instance FromTType' BeamODC.OnboardingDocumentConfig OnboardingDocumentConfig where
  fromTType' BeamODC.OnboardingDocumentConfigT {..} = do
    supportedVehicleClasses' <- case documentType of
      Domain.DL -> Domain.DLValidClasses <$> valueToVehicleClassMap supportedVehicleClassesJSON
      Domain.RC -> Domain.RCValidClasses <$> valueToVehicleClassMap supportedVehicleClassesJSON
      _ -> return $ Domain.RCValidClasses []
    pure $
      Just
        OnboardingDocumentConfig
          { merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            documentType = documentType,
            checkExtraction = checkExtraction,
            checkExpiry = checkExpiry,
            supportedVehicleClasses = supportedVehicleClasses',
            vehicleClassCheckType = vehicleClassCheckType,
            rcNumberPrefix = rcNumberPrefix,
            rcNumberPrefixList = rcNumberPrefixList,
            maxRetryCount = maxRetryCount,
            dlNumberVerification = dlNumberVerification,
            createdAt = createdAt,
            updatedAt = updatedAt
          }
    where
      valueToVehicleClassMap value = case fromJSON value of
        A.Error err -> throwError $ InternalError $ "Unable to decode OnboardingDocumentConfigT.supportedVehicleClassesJSON: " <> show err
        A.Success a -> pure a

instance ToTType' BeamODC.OnboardingDocumentConfig OnboardingDocumentConfig where
  toTType' OnboardingDocumentConfig {..} = do
    BeamODC.OnboardingDocumentConfigT
      { BeamODC.merchantId = getId merchantId,
        BeamODC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamODC.documentType = documentType,
        BeamODC.checkExtraction = checkExtraction,
        BeamODC.checkExpiry = checkExpiry,
        BeamODC.supportedVehicleClassesJSON = BeamODC.getConfigJSON supportedVehicleClasses,
        BeamODC.vehicleClassCheckType = vehicleClassCheckType,
        BeamODC.rcNumberPrefix = rcNumberPrefix,
        BeamODC.rcNumberPrefixList = rcNumberPrefixList,
        BeamODC.maxRetryCount = maxRetryCount,
        BeamODC.dlNumberVerification = dlNumberVerification,
        BeamODC.createdAt = createdAt,
        BeamODC.updatedAt = updatedAt
      }
