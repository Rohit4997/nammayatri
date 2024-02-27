{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.FRFS.OnCancel where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as Booking
import qualified Domain.Types.FRFSTicketBooking as FTBooking
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import Domain.Types.Merchant as Merchant
import Environment
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.FRFSTicket as QTicket
import qualified Storage.Queries.FRFSTicketBooking as QTBooking
import qualified Storage.Queries.FRFSTicketBookingPayment as QTBP

data DOnCancel = DOnCancel
  { providerId :: Text,
    totalPrice :: HighPrecMoney,
    bppOrderId :: Text,
    bppItemId :: Text,
    transactionId :: Text,
    messageId :: Text,
    orderStatus :: Spec.OnCancelOrderStatus,
    refundAmount :: HighPrecMoney,
    baseFare :: HighPrecMoney,
    cancellationCharges :: HighPrecMoney
  }

validateRequest :: DOnCancel -> Flow (Merchant, FTBooking.FRFSTicketBooking)
validateRequest DOnCancel {..} = do
  booking <- runInReplica $ QTBooking.findBySearchId (Id transactionId) >>= fromMaybeM (BookingDoesNotExist messageId)
  merchantId <- booking.merchantId & fromMaybeM (InternalError "MerchantId not found in booking")
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  when (totalPrice /= baseFare + refundAmount + cancellationCharges) $ throwError (InternalError "Fare Mismatch in onCancel Req")
  return (merchant, booking)

onCancel :: Merchant -> Booking.FRFSTicketBooking -> DOnCancel -> Flow ()
onCancel _ booking' dOnCancel = do
  let booking = booking' {Booking.bppOrderId = Just dOnCancel.bppOrderId}
  case dOnCancel.orderStatus of
    Spec.ON_CANCEL_SOFT_CANCEL -> do
      void $ QTBooking.updateRefundCancellationChargesAndIsCancellableByBookingId (Just dOnCancel.refundAmount) (Just dOnCancel.cancellationCharges) (Just True) booking.id
    Spec.ON_CANCEL_CANCELLED -> do
      -- add check if refund and cancellation changed
      void $ QTBooking.updateStatusById FTBooking.CANCELLED booking.id
      void $ QTicket.updateAllStausByBookingId DFRFSTicket.CANCELLED booking.id
      void $ QTBP.updateStatusByTicketBookingId DTBP.REFUND_PENDING booking.id
      void $ QTBooking.updateIsBookingCancellableByBookingId (Just True) booking.id
  return ()
