{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSTicketBooking where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Station
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSTicketBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.FRFSTicketBooking.FRFSTicketBooking] -> m ()
createMany = traverse_ createWithKV

findAllByRiderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.FRFSTicketBooking.FRFSTicketBooking])
findAllByRiderId (Kernel.Types.Id.Id riderId) = do
  findAllWithKV
    [ Se.Is Beam.riderId $ Se.Eq riderId
    ]

findAllByStatus :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> m ([Domain.Types.FRFSTicketBooking.FRFSTicketBooking])
findAllByStatus status = do
  findAllWithKV
    [ Se.Is Beam.status $ Se.Eq status
    ]

findByBppOrderId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe (Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByBppOrderId bppOrderId = do
  findOneWithKV
    [ Se.Is Beam.bppOrderId $ Se.Eq bppOrderId
    ]

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe (Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByQuoteId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe (Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByQuoteId (Kernel.Types.Id.Id quoteId) = do
  findOneWithKV
    [ Se.Is Beam.quoteId $ Se.Eq quoteId
    ]

findBySearchId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe (Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findBySearchId (Kernel.Types.Id.Id searchId) = do
  findOneWithKV
    [ Se.Is Beam.searchId $ Se.Eq searchId
    ]

updateBPPOrderIdAndStatusById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Text -> Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateBPPOrderIdAndStatusById bppOrderId status (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppOrderId $ bppOrderId,
      Se.Set Beam.status $ status,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

updateBppBankDetailsById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateBppBankDetailsById bppBankAccountNumber bppBankCode (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppBankAccountNumber $ bppBankAccountNumber,
      Se.Set Beam.bppBankCode $ bppBankCode,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

updateIsBookingCancellableByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateIsBookingCancellableByBookingId isBookingCancellable (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.isBookingCancellable $ isBookingCancellable,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

updatePriceById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updatePriceById price (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.price $ price,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

updateRefundCancellationChargesAndIsCancellableByBookingId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateRefundCancellationChargesAndIsCancellableByBookingId refundAmount cancellationCharges isBookingCancellable (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.refundAmount $ refundAmount,
      Se.Set Beam.cancellationCharges $ cancellationCharges,
      Se.Set Beam.isBookingCancellable $ isBookingCancellable,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

updateStatusById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateStatusById status (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status $ status,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

updateStatusValidTillAndPaymentTxnById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicketBooking.FRFSTicketBookingStatus -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateStatusValidTillAndPaymentTxnById status validTill paymentTxnId (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.status $ status,
      Se.Set Beam.validTill $ validTill,
      Se.Set Beam.paymentTxnId $ paymentTxnId,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

updateValidTillById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateValidTillById validTill (Kernel.Types.Id.Id id) = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.validTill $ validTill,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m (Maybe (Domain.Types.FRFSTicketBooking.FRFSTicketBooking))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.FRFSTicketBooking.FRFSTicketBooking -> m ()
updateByPrimaryKey Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam._type $ _type,
      Se.Set Beam.bppBankAccountNumber $ bppBankAccountNumber,
      Se.Set Beam.bppBankCode $ bppBankCode,
      Se.Set Beam.bppItemId $ bppItemId,
      Se.Set Beam.bppOrderId $ bppOrderId,
      Se.Set Beam.bppSubscriberId $ bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl $ bppSubscriberUrl,
      Se.Set Beam.cancellationCharges $ cancellationCharges,
      Se.Set Beam.fromStationId $ (Kernel.Types.Id.getId fromStationId),
      Se.Set Beam.isBookingCancellable $ isBookingCancellable,
      Se.Set Beam.paymentTxnId $ paymentTxnId,
      Se.Set Beam.price $ price,
      Se.Set Beam.providerDescription $ providerDescription,
      Se.Set Beam.providerId $ providerId,
      Se.Set Beam.providerName $ providerName,
      Se.Set Beam.quantity $ quantity,
      Se.Set Beam.quoteId $ (Kernel.Types.Id.getId quoteId),
      Se.Set Beam.refundAmount $ refundAmount,
      Se.Set Beam.riderId $ (Kernel.Types.Id.getId riderId),
      Se.Set Beam.searchId $ (Kernel.Types.Id.getId searchId),
      Se.Set Beam.stationsJson $ stationsJson,
      Se.Set Beam.status $ status,
      Se.Set Beam.toStationId $ (Kernel.Types.Id.getId toStationId),
      Se.Set Beam.validTill $ validTill,
      Se.Set Beam.vehicleType $ vehicleType,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  fromTType' Beam.FRFSTicketBookingT {..} = do
    pure $
      Just
        Domain.Types.FRFSTicketBooking.FRFSTicketBooking
          { _type = _type,
            bppBankAccountNumber = bppBankAccountNumber,
            bppBankCode = bppBankCode,
            bppItemId = bppItemId,
            bppOrderId = bppOrderId,
            bppSubscriberId = bppSubscriberId,
            bppSubscriberUrl = bppSubscriberUrl,
            cancellationCharges = cancellationCharges,
            fromStationId = Kernel.Types.Id.Id fromStationId,
            id = Kernel.Types.Id.Id id,
            isBookingCancellable = isBookingCancellable,
            paymentTxnId = paymentTxnId,
            price = price,
            providerDescription = providerDescription,
            providerId = providerId,
            providerName = providerName,
            quantity = quantity,
            quoteId = Kernel.Types.Id.Id quoteId,
            refundAmount = refundAmount,
            riderId = Kernel.Types.Id.Id riderId,
            searchId = Kernel.Types.Id.Id searchId,
            stationsJson = stationsJson,
            status = status,
            toStationId = Kernel.Types.Id.Id toStationId,
            validTill = validTill,
            vehicleType = vehicleType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSTicketBooking Domain.Types.FRFSTicketBooking.FRFSTicketBooking where
  toTType' Domain.Types.FRFSTicketBooking.FRFSTicketBooking {..} = do
    Beam.FRFSTicketBookingT
      { Beam._type = _type,
        Beam.bppBankAccountNumber = bppBankAccountNumber,
        Beam.bppBankCode = bppBankCode,
        Beam.bppItemId = bppItemId,
        Beam.bppOrderId = bppOrderId,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.bppSubscriberUrl = bppSubscriberUrl,
        Beam.cancellationCharges = cancellationCharges,
        Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isBookingCancellable = isBookingCancellable,
        Beam.paymentTxnId = paymentTxnId,
        Beam.price = price,
        Beam.providerDescription = providerDescription,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.quantity = quantity,
        Beam.quoteId = Kernel.Types.Id.getId quoteId,
        Beam.refundAmount = refundAmount,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam.stationsJson = stationsJson,
        Beam.status = status,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.validTill = validTill,
        Beam.vehicleType = vehicleType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
