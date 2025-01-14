{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PlanTranslation where

import qualified Data.Text
import qualified Domain.Types.Plan
import qualified Domain.Types.PlanTranslation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PlanTranslation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.PlanTranslation.PlanTranslation -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.PlanTranslation.PlanTranslation] -> m ()
createMany = traverse_ create

findByPlanIdAndLanguage :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Plan.Plan -> Kernel.External.Types.Language -> m (Maybe (Domain.Types.PlanTranslation.PlanTranslation))
findByPlanIdAndLanguage (Kernel.Types.Id.Id planId) language = do
  findOneWithKV
    [ Se.Is Beam.planId $ Se.Eq planId,
      Se.Is Beam.language $ Se.Eq language
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Plan.Plan -> m (Maybe (Domain.Types.PlanTranslation.PlanTranslation))
findByPrimaryKey (Kernel.Types.Id.Id planId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.planId $ Se.Eq planId
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.PlanTranslation.PlanTranslation -> m ()
updateByPrimaryKey Domain.Types.PlanTranslation.PlanTranslation {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.language language,
      Se.Set Beam.name name,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.planId $ Se.Eq (Kernel.Types.Id.getId planId)
        ]
    ]

instance FromTType' Beam.PlanTranslation Domain.Types.PlanTranslation.PlanTranslation where
  fromTType' Beam.PlanTranslationT {..} = do
    pure $
      Just
        Domain.Types.PlanTranslation.PlanTranslation
          { description = description,
            language = language,
            name = name,
            planId = Kernel.Types.Id.Id planId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PlanTranslation Domain.Types.PlanTranslation.PlanTranslation where
  toTType' Domain.Types.PlanTranslation.PlanTranslation {..} = do
    Beam.PlanTranslationT
      { Beam.description = description,
        Beam.language = language,
        Beam.name = name,
        Beam.planId = Kernel.Types.Id.getId planId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
