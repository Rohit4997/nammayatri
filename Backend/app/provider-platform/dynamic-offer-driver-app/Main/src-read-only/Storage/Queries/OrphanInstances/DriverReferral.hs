{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DriverReferral where

import qualified Data.Text
import qualified Domain.Types.DriverReferral
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverReferral as Beam

instance FromTType' Beam.DriverReferral Domain.Types.DriverReferral.DriverReferral where
  fromTType' Beam.DriverReferralT {..} = do
    pure $
      Just
        Domain.Types.DriverReferral.DriverReferral
          { driverId = Kernel.Types.Id.Id driverId,
            linkedAt = linkedAt,
            referralCode = Kernel.Types.Id.Id referralCode,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverReferral Domain.Types.DriverReferral.DriverReferral where
  toTType' Domain.Types.DriverReferral.DriverReferral {..} = do
    Beam.DriverReferralT
      { Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.linkedAt = linkedAt,
        Beam.referralCode = Kernel.Types.Id.getId referralCode,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
