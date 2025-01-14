{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.FRFS.Utils where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified BecknV2.FRFS.Utils as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import Data.Aeson as A
import Domain.Action.Beckn.FRFS.Common
import qualified Domain.Action.Beckn.FRFS.Common as Domain
import Domain.Types (BknPaymentParams)
import Domain.Types.BecknConfig
import qualified Domain.Types.FRFSTicket as Ticket
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

buildContext ::
  (MonadFlow m) =>
  Spec.Action ->
  BecknConfig ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe BppData ->
  m Spec.Context
buildContext action bapConfig txnId msgId mTTL bppData = do
  now <- getCurrentTime
  let bapUrl = showBaseUrl bapConfig.subscriberUrl
  let bapId = bapConfig.subscriberId
      contextBppId = bppData <&> (.bppId)
      contextBppUri = bppData <&> (.bppUri)
  return $
    Spec.Context
      { contextAction = encodeToText' action,
        contextBapId = Just bapId,
        contextBapUri = Just bapUrl,
        contextBppId,
        contextBppUri,
        contextDomain = encodeToText' Spec.FRFS,
        contextKey = Nothing,
        contextLocation = Just $ tfLocation "std:044",
        contextMessageId = Just msgId,
        contextTimestamp = Just now,
        contextTransactionId = Just txnId,
        contextTtl = mTTL,
        contextVersion = Just "2.0.0"
      }

tfLocation :: Text -> Spec.Location
tfLocation location_code =
  Spec.Location
    { locationDescriptor = Nothing,
      locationGps = Nothing,
      locationCity =
        Just $
          Spec.City
            { cityCode = Just location_code,
              cityName = Nothing
            },
      locationCountry =
        Just $
          Spec.Country
            { countryCode = Just "IND",
              countryName = Nothing
            }
    }

getStartStop :: [Spec.Stop] -> Maybe Spec.Stop
getStartStop stops = stops & find (\stop -> stop.stopType == start)
  where
    start = encodeToText' Spec.START

mkFareBreakup :: (MonadFlow m) => Spec.QuotationBreakupInner -> m Domain.DFareBreakUp
mkFareBreakup fareBreakup = do
  title <- fareBreakup.quotationBreakupInnerTitle & fromMaybeM (InvalidRequest "Title not found")
  price <- fareBreakup.quotationBreakupInnerPrice >>= Utils.parseMoney & fromMaybeM (InvalidRequest "Price not found")

  breakupItem <- fareBreakup.quotationBreakupInnerItem & fromMaybeM (InvalidRequest "BreakupItem not found")
  let pricePerUnit = breakupItem.itemPrice >>= Utils.parseMoney & fromMaybe price
  let quantity = breakupItem.itemQuantity >>= (.itemQuantitySelected) >>= (.itemQuantitySelectedCount) & fromMaybe 1

  pure $
    Domain.DFareBreakUp
      { title,
        price,
        pricePerUnit,
        quantity
      }

parseTickets :: (MonadFlow m) => Spec.Item -> [Spec.Fulfillment] -> m [Domain.DTicket]
parseTickets item fulfillments = do
  fulfillmentIds <- item.itemFulfillmentIds & fromMaybeM (InvalidRequest "FulfillmentIds not found")
  when (null fulfillmentIds) $ throwError $ InvalidRequest "Empty fulfillmentIds"

  let ticketFulfillments = filterByIds fulfillmentIds
  when (null ticketFulfillments) $ throwError $ InvalidRequest "No ticket fulfillment found"

  traverse parseTicket ticketFulfillments
  where
    filterByIds fIds = filter (\f -> f.fulfillmentId `elem` (Just <$> fIds)) fulfillments

parseTicket :: (MonadFlow m) => Spec.Fulfillment -> m Domain.DTicket
parseTicket fulfillment = do
  fId <- fulfillment.fulfillmentId & fromMaybeM (InvalidRequest "FulfillmentId not found")
  stops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "FulfillmentStops not found")
  startStopAuth <- getStartStop stops >>= (.stopAuthorization) & fromMaybeM (InvalidRequest "StartStop Auth not found")

  qrData <- startStopAuth.authorizationToken & fromMaybeM (InvalidRequest "TicketQrData not found")
  validTill <- startStopAuth.authorizationValidTo & fromMaybeM (InvalidRequest "TicketValidTill not found")
  status <- startStopAuth.authorizationStatus & fromMaybeM (InvalidRequest "TicketStatus not found")

  tags <- fulfillment.fulfillmentTags & fromMaybeM (InvalidRequest "FulfillmentTags not found")
  ticketNumber <- Utils.getTag "TICKET_INFO" "NUMBER" tags & fromMaybeM (InvalidRequest "TicketNumber not found")

  pure $
    Domain.DTicket
      { qrData,
        validTill,
        bppFulfillmentId = fId,
        ticketNumber,
        status
      }

type TxnId = Text

type Amount = Text

mkPayment :: Spec.PaymentStatus -> Maybe Amount -> Maybe TxnId -> Maybe BknPaymentParams -> Maybe Text -> Spec.Payment
mkPayment paymentStatus mAmount mTxnId mPaymentParams mSettlementType =
  Spec.Payment
    { paymentCollectedBy = Just $ show Enums.BAP,
      paymentId = mTxnId,
      paymentParams =
        if anyTrue [isJust mTxnId, isJust mAmount, isJust mPaymentParams]
          then Just $ mkPaymentParams mPaymentParams mTxnId mAmount
          else Nothing,
      paymentStatus = encodeToText' paymentStatus,
      paymentTags = Just $ mkPaymentTags mSettlementType mAmount,
      paymentType = encodeToText' Spec.PRE_ORDER
    }
  where
    anyTrue = or

mkPaymentParams :: Maybe BknPaymentParams -> Maybe TxnId -> Maybe Amount -> Spec.PaymentParams
mkPaymentParams mPaymentParams mTxnId mAmount =
  Spec.PaymentParams
    { paymentParamsAmount = mAmount,
      paymentParamsBankAccountNumber = mPaymentParams >>= (.bankAccNumber),
      paymentParamsBankCode = mPaymentParams >>= (.bankCode),
      paymentParamsCurrency = Just "INR",
      paymentParamsTransactionId = mTxnId,
      paymentParamsVirtualPaymentAddress = mPaymentParams >>= (.vpa)
    }

mkPaymentTags :: Maybe Text -> Maybe Amount -> [Spec.TagGroup]
mkPaymentTags mSettlementType mAmount =
  catMaybes
    [ Just mkBuyerFinderFeeTagGroup,
      Just $ mkSettlementTagGroup mAmount,
      mkSettlementDetailsTagGroup mSettlementType
    ]

mkBuyerFinderFeeTagGroup :: Spec.TagGroup
mkBuyerFinderFeeTagGroup =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just "BUYER_FINDER_FEES",
              descriptorImages = Nothing,
              descriptorName = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just [feePercentage]
    }
  where
    feePercentage =
      Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "BUYER_FINDER_FEES_PERCENTAGE",
                  descriptorImages = Nothing,
                  descriptorName = Nothing
                },
          tagValue = Just "0"
        }

mkSettlementTagGroup :: Maybe Text -> Spec.TagGroup
mkSettlementTagGroup mAmount =
  Spec.TagGroup
    { tagGroupDescriptor =
        Just $
          Spec.Descriptor
            { descriptorCode = Just "SETTLEMENT_TERMS",
              descriptorImages = Nothing,
              descriptorName = Nothing
            },
      tagGroupDisplay = Just False,
      tagGroupList = Just settlementTags
    }
  where
    settlementTags =
      catMaybes
        [ mAmount <&> \amount ->
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "SETTLEMENT_AMOUNT",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just amount
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "SETTLEMENT_WINDOW",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "PT1D"
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "DELAY_INTEREST",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "0"
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "SETTLEMENT_BASIS",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "INVOICE_RECIEPT"
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "MANDATORY_ARBITRATION",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "TRUE"
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "COURT_JURISDICTION",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "Bengaluru" -- TODO: make it dynamic
              },
          Just $
            Spec.Tag
              { tagDescriptor =
                  Just $
                    Spec.Descriptor
                      { descriptorCode = Just "STATIC_TERMS",
                        descriptorImages = Nothing,
                        descriptorName = Nothing
                      },
                tagValue = Just "https://api.example-bap.com/booking/terms" -- TODO: update with actual terms url
              }
        ]

mkSettlementDetailsTagGroup :: Maybe Text -> Maybe Spec.TagGroup
mkSettlementDetailsTagGroup mSettlementType = do
  st <- mSettlementType
  return $
    Spec.TagGroup
      { tagGroupDescriptor =
          Just $
            Spec.Descriptor
              { descriptorCode = Just "SETTLEMENT_DETAILS",
                descriptorImages = Nothing,
                descriptorName = Nothing
              },
        tagGroupDisplay = Just False,
        tagGroupList = Just [stTag st]
      }
  where
    stTag st =
      Spec.Tag
        { tagDescriptor =
            Just $
              Spec.Descriptor
                { descriptorCode = Just "SETTLEMENT_TYPE",
                  descriptorImages = Nothing,
                  descriptorName = Nothing
                },
          tagValue = Just st
        }

encodeToText' :: (ToJSON a) => a -> Maybe Text
encodeToText' = A.decode . A.encode

type TicketNumber = Text

getTicketStatus :: (MonadFlow m) => DTicket -> m (TicketNumber, Ticket.FRFSTicketStatus)
getTicketStatus dTicket = do
  let validTill = dTicket.validTill
  now <- getCurrentTime

  if now > validTill
    then return (dTicket.ticketNumber, Ticket.EXPIRED)
    else do
      status <- castTicketStatus dTicket.status
      return (dTicket.ticketNumber, status)

castTicketStatus :: (MonadFlow m) => Text -> m Ticket.FRFSTicketStatus
castTicketStatus "UNCLAIMED" = return Ticket.ACTIVE
castTicketStatus "CLAIMED" = return Ticket.USED
castTicketStatus _ = throwError $ InternalError "Invalid ticket status"

data BppData = BppData
  { bppId :: Text,
    bppUri :: Text
  }
  deriving (Show, Eq, Generic)
