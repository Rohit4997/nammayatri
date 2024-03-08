{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Beckn.OnDemand.Transformer.OnSearch where

import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.OnSearch
import qualified BecknV2.OnDemand.Types
import qualified BecknV2.OnDemand.Utils.Common
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnSearch
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.App
import Kernel.Types.Id
import qualified Kernel.Types.Id
import Kernel.Utils.Common (type (:::))
import qualified Kernel.Utils.Error
import qualified Tools.Error

buildOnSearchReq :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.OnSearchReq -> BecknV2.OnDemand.Types.Provider -> [BecknV2.OnDemand.Types.Item] -> [BecknV2.OnDemand.Types.Fulfillment] -> Kernel.Prelude.UTCTime -> m Domain.Action.Beckn.OnSearch.DOnSearchReq
buildOnSearchReq req provider items fulfillments validTill = do
  let paymentMethodsInfo_ = []
  providerInfo_ <- tfProviderInfo req
  (estimatesInfo_, quotesInfo_) <- partitionEithers <$> traverse (tfQuotesInfo provider fulfillments validTill) items
  requestId_ <- BecknV2.OnDemand.Utils.Common.getTransactionId req.onSearchReqContext
  pure $ Domain.Action.Beckn.OnSearch.DOnSearchReq {estimatesInfo = estimatesInfo_, paymentMethodsInfo = paymentMethodsInfo_, providerInfo = providerInfo_, quotesInfo = quotesInfo_, requestId = Id requestId_}

tfProviderInfo :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.OnSearchReq -> m Domain.Action.Beckn.OnSearch.ProviderInfo
tfProviderInfo req = do
  let mobileNumber_ = ""
  name_ <- Beckn.OnDemand.Utils.OnSearch.getProviderName req
  let ridesCompleted_ = 0
  providerId_ <- req.onSearchReqContext.contextBppId & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing bpp_id")
  url_ <- Beckn.OnDemand.Utils.Common.getContextBppUri req.onSearchReqContext >>= Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing bpp_uri")
  pure $ Domain.Action.Beckn.OnSearch.ProviderInfo {mobileNumber = mobileNumber_, name = name_, providerId = providerId_, ridesCompleted = ridesCompleted_, url = url_}

builRentalQuoteInfo :: BecknV2.OnDemand.Types.Item -> Text -> Maybe Domain.Action.Beckn.OnSearch.RentalQuoteDetails
builRentalQuoteInfo item quoteId_ = do
  let itemTags = item.itemTags
  let id = quoteId_
  baseFare <- Beckn.OnDemand.Utils.OnSearch.getRentalBaseFare itemTags
  perHourCharge <- Beckn.OnDemand.Utils.OnSearch.getRentalPerHourCharge itemTags
  perExtraMinRate <- Beckn.OnDemand.Utils.OnSearch.getRentalPerExtraMinRate itemTags
  perExtraKmRate <- Beckn.OnDemand.Utils.OnSearch.getRentalPerExtraKmRate itemTags
  includedKmPerHr <- Beckn.OnDemand.Utils.OnSearch.getRentalIncludedKmPerHr itemTags
  plannedPerKmRate <- Beckn.OnDemand.Utils.OnSearch.getRentalPlannedPerKmRate itemTags
  let nightShiftInfo = Beckn.OnDemand.Utils.OnSearch.buildNightShiftInfo item
  Just $ Domain.Action.Beckn.OnSearch.RentalQuoteDetails {..}

tfQuotesInfo :: (Monad m, Kernel.Types.App.MonadFlow m) => BecknV2.OnDemand.Types.Provider -> [BecknV2.OnDemand.Types.Fulfillment] -> Kernel.Prelude.UTCTime -> BecknV2.OnDemand.Types.Item -> m (Either Domain.Action.Beckn.OnSearch.EstimateInfo Domain.Action.Beckn.OnSearch.QuoteInfo)
tfQuotesInfo provider fulfillments validTill item = do
  let descriptions_ = []
  let discount_ = Nothing
  estimatedFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item
  estimatedTotalFare_ <- Beckn.OnDemand.Utils.OnSearch.getEstimatedFare item
  itemId_ <- Beckn.OnDemand.Utils.OnSearch.getItemId item
  let serviceTierName_ = Beckn.OnDemand.Utils.OnSearch.getDescriptorInfo item
  specialLocationTag_ <- Beckn.OnDemand.Utils.OnSearch.buildSpecialLocationTag item
  vehicleVariant_ <- Beckn.OnDemand.Utils.OnSearch.getVehicleVariant provider item
  quoteOrEstId_ <- Beckn.OnDemand.Utils.OnSearch.getQuoteFulfillmentId item
  fulfillment <- filter (\f -> f.fulfillmentId == Just quoteOrEstId_) fulfillments & Data.Maybe.listToMaybe & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment for item")
  fulfillmentType <- fulfillment.fulfillmentType & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment type")
  case fulfillmentType of
    "RENTAL" -> do
      quoteInfo <- builRentalQuoteInfo item quoteOrEstId_ & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing rental quote details")
      let quoteDetails_ = Domain.Action.Beckn.OnSearch.RentalDetails quoteInfo
      pure $ Right $ Domain.Action.Beckn.OnSearch.QuoteInfo {descriptions = descriptions_, discount = discount_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, quoteDetails = quoteDetails_, specialLocationTag = specialLocationTag_, vehicleVariant = vehicleVariant_, validTill, serviceTierName = serviceTierName_}
    "RIDE_OTP" -> do
      let quoteDetails_ = Domain.Action.Beckn.OnSearch.OneWaySpecialZoneDetails (Domain.Action.Beckn.OnSearch.OneWaySpecialZoneQuoteDetails {quoteId = quoteOrEstId_})
      pure $ Right $ Domain.Action.Beckn.OnSearch.QuoteInfo {descriptions = descriptions_, discount = discount_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, quoteDetails = quoteDetails_, specialLocationTag = specialLocationTag_, vehicleVariant = vehicleVariant_, validTill, serviceTierName = serviceTierName_}
    "INTER_CITY" -> do
      let quoteDetails_ = Domain.Action.Beckn.OnSearch.InterCityDetails (Domain.Action.Beckn.OnSearch.InterCityQuoteDetails {quoteId = quoteOrEstId_})
      pure $ Right $ Domain.Action.Beckn.OnSearch.QuoteInfo {descriptions = descriptions_, discount = discount_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, quoteDetails = quoteDetails_, specialLocationTag = specialLocationTag_, vehicleVariant = vehicleVariant_, validTill, serviceTierName = serviceTierName_}
    _ -> do
      let bppEstimateId_ = Id quoteOrEstId_
      driversLocation_ <- Beckn.OnDemand.Utils.OnSearch.getProviderLocation provider fulfillments vehicleVariant_
      let nightShiftInfo_ = Beckn.OnDemand.Utils.OnSearch.buildNightShiftInfo item
      totalFareRange_ <- Beckn.OnDemand.Utils.OnSearch.getTotalFareRange item
      waitingCharges_ <- Beckn.OnDemand.Utils.OnSearch.buildWaitingChargeInfo item
      estimateBreakupList_ <- Beckn.OnDemand.Utils.OnSearch.buildEstimateBreakupList item
      pure $ Left $ Domain.Action.Beckn.OnSearch.EstimateInfo {bppEstimateId = bppEstimateId_, descriptions = descriptions_, discount = discount_, driversLocation = driversLocation_, estimateBreakupList = estimateBreakupList_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, nightShiftInfo = nightShiftInfo_, specialLocationTag = specialLocationTag_, totalFareRange = totalFareRange_, vehicleVariant = vehicleVariant_, waitingCharges = waitingCharges_, validTill, serviceTierName = serviceTierName_}
