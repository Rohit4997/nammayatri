{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SuspectFlagRequestExtra where

import qualified "lib-dashboard" Domain.Types.Merchant
import Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SuspectFlagRequest as Beam
import Storage.Queries.OrphanInstances.SuspectFlagRequest

findAllByDlAndVoterIdAndMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Prelude.Text] -> [Kernel.Prelude.Text] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findAllByDlAndVoterIdAndMerchantId dls voterIds merchantId = do
  let dlList = map Just dls
      voterIdList = map Just voterIds
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.dl $ Se.In dlList,
              Se.Is Beam.voterId $ Se.In voterIdList
            ],
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Or
            [ Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending,
              Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Approved
            ]
        ]
    ]

findAllPAByDlAndVoterIdAndMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Maybe Kernel.Prelude.Text] -> [Maybe Kernel.Prelude.Text] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findAllPAByDlAndVoterIdAndMerchantId dls voterIds merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.dl $ Se.In dls,
              Se.Is Beam.voterId $ Se.In voterIds
            ],
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending
        ]
    ]

findAllApprovedByDlAndVoterIdAndMerchantId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Maybe Kernel.Prelude.Text] -> [Maybe Kernel.Prelude.Text] -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findAllApprovedByDlAndVoterIdAndMerchantId dls voterIds merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Or
            [ Se.Is Beam.dl $ Se.In dls,
              Se.Is Beam.voterId $ Se.In voterIds
            ],
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Approved
        ]
    ]

findByMerchantIdAndDl' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByMerchantIdAndDl' merchantId dl = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.dl $ Se.Eq dl,
          Se.Or
            [ Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending,
              Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Approved
            ]
        ]
    ]

findByMerchantIdAndVoterId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.SuspectFlagRequest.SuspectFlagRequest))
findByMerchantIdAndVoterId merchantId voterId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId <$> merchantId),
          Se.Is Beam.voterId $ Se.Eq voterId,
          Se.Or
            [ Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending,
              Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Approved
            ]
        ]
    ]

updateAllWIthDlAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> FlaggedStatus -> m ()
updateAllWIthDlAndFlaggedStatus dl flaggedStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt now
    ]
    [ Se.Is Beam.dl $ Se.Eq (Just dl)
    ]

updateAllWithVoteIdAndFlaggedStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> FlaggedStatus -> m ()
updateAllWithVoteIdAndFlaggedStatus voterId flaggedStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.flaggedStatus flaggedStatus,
      Se.Set Beam.updatedAt now
    ]
    [ Se.Is Beam.voterId $ Se.Eq (Just voterId)
    ]

updateManyAdminApprovalById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.SuspectFlagRequest.AdminApproval -> Text -> [Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> m ()
updateManyAdminApprovalById adminApproval approvedBy ids = traverse_ (updateAdminApprovalById' adminApproval approvedBy) ids

updateAdminApprovalById' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.SuspectFlagRequest.AdminApproval -> Text -> Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest -> m ()
updateAdminApprovalById' adminApproval approvedBy (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.adminApproval $ adminApproval,
      Se.Set Beam.approvedBy $ Just approvedBy,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

findAllByRequestId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [Kernel.Types.Id.Id Domain.Types.SuspectFlagRequest.SuspectFlagRequest] -> m ([Domain.Types.SuspectFlagRequest.SuspectFlagRequest])
findAllByRequestId ids = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> ids),
          Se.Is Beam.adminApproval $ Se.Eq Domain.Types.SuspectFlagRequest.Pending
        ]
    ]
