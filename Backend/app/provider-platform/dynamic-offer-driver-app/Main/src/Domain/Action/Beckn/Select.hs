{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Domain.Action.Beckn.Select
  ( DSelectReq (..),
    validateRequest,
    handler,
  )
where

import qualified Client.Main as CM
import Data.Aeson as DA
import Data.HashMap.Strict as HashMap
import Data.Text as Text
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.RiderDetails as SRD
import SharedLogic.SearchTry
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    estimateId :: Id DEst.Estimate,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupTime :: UTCTime,
    autoAssignEnabled :: Bool,
    customerExtraFee :: Maybe Money,
    customerPhoneNum :: Maybe Text
  }

handler :: DM.Merchant -> DSelectReq -> DEst.Estimate -> Flow ()
handler merchant sReq estimate = do
  now <- getCurrentTime
  searchReq <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
  case sReq.customerPhoneNum of
    Just number -> do
      (riderDetails, isNewRider) <- SRD.getRiderDetails merchant.id (fromMaybe "+91" merchant.mobileCountryCode) number now False
      when isNewRider $ QRD.create riderDetails
      QSR.updateRiderId searchReq.id riderDetails.id
    Nothing -> do
      logWarning "Failed to get rider details as BAP Phone Number is NULL"
  QDQ.setInactiveAllDQByEstId sReq.estimateId now
  when sReq.autoAssignEnabled $ QSR.updateAutoAssign searchReq.id sReq.autoAssignEnabled

  initiateDriverSearchBatch sendSearchRequestToDrivers' merchant searchReq estimate.tripCategory estimate.vehicleVariant (getId estimate.id) sReq.customerExtraFee sReq.messageId False

validateRequest :: Id DM.Merchant -> DSelectReq -> Flow (DM.Merchant, DEst.Estimate)
validateRequest merchantId sReq = do
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  estimate <- QEst.findById sReq.estimateId >>= fromMaybeM (EstimateDoesNotExist sReq.estimateId.getId)
  return (merchant, estimate)
