{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FlaggedCategory (module Storage.Queries.FlaggedCategory, module ReExport) where

import qualified Domain.Types.FlaggedCategory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FlaggedCategory as Beam
import Storage.Queries.FlaggedCategoryExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FlaggedCategory.FlaggedCategory -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FlaggedCategory.FlaggedCategory] -> m ()
createMany = traverse_ create

deleteById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FlaggedCategory.FlaggedCategory -> m ()
deleteById (Kernel.Types.Id.Id id) = do
  deleteWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByName :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> m (Maybe (Domain.Types.FlaggedCategory.FlaggedCategory))
findByName name = do
  findOneWithKV
    [ Se.Is Beam.name $ Se.Eq name
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FlaggedCategory.FlaggedCategory -> m (Maybe (Domain.Types.FlaggedCategory.FlaggedCategory))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FlaggedCategory.FlaggedCategory -> m ()
updateByPrimaryKey Domain.Types.FlaggedCategory.FlaggedCategory {..} = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.name name,
      Se.Set Beam.updatedAt _now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]
