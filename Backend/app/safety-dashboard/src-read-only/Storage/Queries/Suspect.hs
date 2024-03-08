{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Suspect (module Storage.Queries.Suspect, module ReExport) where

import qualified Domain.Types.Suspect
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Suspect as Beam
import Storage.Queries.SuspectExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.Suspect.Suspect -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.Suspect.Suspect] -> m ()
createMany = traverse_ create

findByDl :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.Suspect.Suspect))
findByDl dl = do
  findOneWithKV
    [ Se.Is Beam.dl $ Se.Eq dl
    ]

findByVoterId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.Suspect.Suspect))
findByVoterId voterId = do
  findOneWithKV
    [ Se.Is Beam.voterId $ Se.Eq voterId
    ]

updateFlaggedCounterByDl :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Int -> Domain.Types.Suspect.FlaggedStatus -> [Domain.Types.Suspect.FlaggedBy] -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ()
updateFlaggedCounterByDl flaggedCounter flaggedStatus flaggedBy dl = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedCounter flaggedCounter,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.flaggedBy flaggedBy
    ]
    [ Se.Is Beam.dl $ Se.Eq dl
    ]

updateFlaggedCounterByVoterId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Int -> Domain.Types.Suspect.FlaggedStatus -> [Domain.Types.Suspect.FlaggedBy] -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ()
updateFlaggedCounterByVoterId flaggedCounter flaggedStatus flaggedBy voterId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedCounter flaggedCounter,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.flaggedBy flaggedBy
    ]
    [ Se.Is Beam.voterId $ Se.Eq voterId
    ]

updateFlaggedStatusById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Suspect.FlaggedStatus -> Kernel.Types.Id.Id Domain.Types.Suspect.Suspect -> m ()
updateFlaggedStatusById flaggedStatus (Kernel.Types.Id.Id id) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Suspect.Suspect -> m (Maybe (Domain.Types.Suspect.Suspect))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.Suspect.Suspect -> m ()
updateByPrimaryKey Domain.Types.Suspect.Suspect {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.dl dl,
      Se.Set Beam.firstName firstName,
      Se.Set Beam.flagUpdatedAt flagUpdatedAt,
      Se.Set Beam.flaggedBy flaggedBy,
      Se.Set Beam.flaggedCounter flaggedCounter,
      Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.lastName lastName,
      Se.Set Beam.statusChangedReason statusChangedReason,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.voterId voterId
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]
