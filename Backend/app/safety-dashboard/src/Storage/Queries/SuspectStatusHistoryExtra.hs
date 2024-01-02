{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SuspectStatusHistoryExtra where

import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import qualified Domain.Types.SuspectStatusHistory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectStatusHistory as Beam
import Storage.Queries.OrphanInstances.SuspectStatusHistory

findAllByDl :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.SuspectStatusHistory.SuspectStatusHistory])
findAllByDl limit offset dl = do
  findAllWithOptionsKV
    [ Se.Is Beam.dl $ Se.Eq dl
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset

findAllByVoterId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Maybe Int -> Maybe Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Domain.Types.SuspectStatusHistory.SuspectStatusHistory])
findAllByVoterId limit offset voterId = do
  findAllWithOptionsKV
    [ Se.Is Beam.voterId $ Se.Eq voterId
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset

-- Extra code goes here --
