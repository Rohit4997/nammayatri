{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketDetails.View where

import Prelude
import Common.Types.App (LazyCheck(..))

import Data.Array 
import Effect.Aff 
import Types.App
import Control.Monad.Except.Trans 
import Control.Transformers.Back.Trans 
import Engineering.Helpers.Commons 
import Effect.Class 
import Data.Maybe 
import Effect 
import Font.Style as FontStyle
import PrestoDOM 
import Screens.TicketBookingFlow.MetroTicketDetails.Controller
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Mobility.Prelude
import Debug
import Helpers.Utils
import Effect.Uncurried
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Engineering.Helpers.Commons
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import Language.Strings
import Language.Types
import Data.String as DS
import Services.API (MetroBookingSoftCancelStatusResp(..), MetroBookingHardCancelStatusResp(..))
import Screens.Types
import Services.Backend as Remote
import Data.Either (Either(..))
import Engineering.Helpers.Commons as EHC
import Components.PopUpModal as PopUpModal
import PrestoDOM.Types.DomAttributes (Corners(..)) as PTD
import Components.PrimaryButton (view) as PrimaryButton
import Components.PrimaryButton.Controller as PrimaryButton
import Data.Tuple (Tuple(..))
import JBridge (startLottieProcess, lottieAnimationConfig)
import Screens.TicketBookingFlow.MetroTicketDetails.ComponentConfig as Config

screen :: ST.MetroTicketDetailsScreenState -> Screen Action ST.MetroTicketDetailsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketDetailsScreen"
  , globalEvents : [getCancelStatus]
  , eval :
    \action state -> do
        let _ = spy "MetroTicketDetailsScreen action " action
        let _ = spy "MetroTicketDetailsScreen state " state
        eval action state
  }
  where
    getCancelStatus push = do
        void $ launchAff $ EHC.flowRunner defaultGlobalState $ getCancelStatusPolling initialState.data.bookingId 5 3000.0 initialState push
        pure $ pure unit

getCancelStatusPolling :: forall action. String-> Int -> Number -> ST.MetroTicketDetailsScreenState -> (Action -> Effect Unit) -> Flow GlobalState Unit
getCancelStatusPolling bookingId count delayDuration state push = do
  let stage = state.props.stage
  if bookingId /= "" && count > 0 then do
    if stage == MetroSoftCancelStatusStage then do
      metroBookingSoftCancelStatusResp <- Remote.metroBookingSoftCancelStatus bookingId
      case metroBookingSoftCancelStatusResp of
        Right (MetroBookingSoftCancelStatusResp response) -> do
          let (MetroBookingSoftCancelStatusResp response) = MetroBookingSoftCancelStatusResp {isCancellable : Just true, cancellationCharges : Just 0.0, refundAmount : Just 0.0}
          if (isJust response.isCancellable) then do
            doAff do liftEffect $ push $ ShowCancelBookingPopup (MetroBookingSoftCancelStatusResp response)
          else do
            void $ delay $ Milliseconds delayDuration
            getCancelStatusPolling bookingId (count - 1) delayDuration state push
        Left _ -> pure unit
    else if stage == MetroHardCancelStatusStage then do
      metroBookingHardCancelStatusResp <- Remote.metroBookingHardCancelStatus bookingId
      case metroBookingHardCancelStatusResp of
        Right (MetroBookingHardCancelStatusResp response) -> do
          let (MetroBookingHardCancelStatusResp response) = MetroBookingHardCancelStatusResp {cancellationCharges : Just 0.0, refundAmount : Just 9.0}
          if (isJust response.refundAmount) then do
              doAff do liftEffect $ push $ ShowMetroBookingCancelledView (MetroBookingHardCancelStatusResp response)
            else do
              void $ delay $ Milliseconds delayDuration
              getCancelStatusPolling bookingId (count - 1) delayDuration state push 
        Left _ -> pure unit 
    else
      pure unit
  else
    pure unit

view :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state =  
  let 
    bodyView = case state.props.stage of 
                  ST.MetroTicketDetailsStage -> metroTicketDetailsView
                  ST.MetroMapStage -> mapView
                  ST.MetroRouteDetailsStage -> routeDetailsView
                  ST.MetroSoftCancelStatusStage -> metroTicketDetailsView
                  ST.MetroHardCancelStatusStage -> metroTicketDetailsView
                  ST.MetroBookingCancelledStage -> cancelledBookingView
  in
    Anim.screenAnimation $ 
    relativeLayout [
      width MATCH_PARENT
    , height MATCH_PARENT
    ] [
      linearLayout[
        width MATCH_PARENT
      , height MATCH_PARENT
      , background Color.grey700
      , clickable true
      , onBackPressed push $ const BackPressed
      , padding $ PaddingVertical safeMarginTop 16
      , orientation VERTICAL
      ][
        headerView push state
      , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ][]
      , bodyView push state
      ]
      , if (isJust state.props.isBookingCancellable) then cancelBookingPopUpView push state else linearLayout [][]
      , softCancelStatusLottieLoader push state
    ]
 
headerView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
headerView push state = 
  let 
    headerText = getString $ case state.props.stage of 
                  ST.MetroTicketDetailsStage -> TICKET_DETAILS
                  ST.MetroMapStage -> MAP_STR
                  ST.MetroRouteDetailsStage -> ROUTE_DETAILS
                  ST.MetroSoftCancelStatusStage -> TICKET_DETAILS
                  ST.MetroHardCancelStatusStage -> TICKET_DETAILS
                  ST.MetroBookingCancelledStage -> TICKET_DETAILS
    (Config.StatusPillConfig config) = Config.getStatusPillConfig state
    shareButtonVisibility =  boolToVisibility $ config.ticketStatus == "ACTIVE"
  in 
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 16 16 16 
    , background Color.white900
    , gravity CENTER_VERTICAL
    ][
      imageView [
        width $ V 24
      , height $ V 27
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
      , onClick push $ const BackPressed
      ]
    , textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text headerText
      , color Color.black800
      , margin $ MarginLeft 8
      ] <> FontStyle.h3 TypoGraphy 
    , linearLayout [
        height WRAP_CONTENT
      , weight 1.0
      ][]
    , linearLayout [
        width WRAP_CONTENT
      , height MATCH_PARENT
      , onClick push $ const ShareTicketClick
      , gravity CENTER_VERTICAL 
      , visibility shareButtonVisibility
      ][
        imageView [
          width $ V 16
        , height $ V 16
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_share"
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString SHARE_TICKET
        , color Color.blue900
        , margin $ MarginLeft 8
        ] <> FontStyle.body1 TypoGraphy
      ]
    ]

metroTicketDetailsView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
metroTicketDetailsView push state = 
  scrollView [
    width MATCH_PARENT
  , height WRAP_CONTENT
  ][
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , id $ getNewIDWithTag "MetroTicketView"
    ][
      ticketDetailsView push state
    , paymentInfoView push state
    , PrimaryButton.view (push <<< CancelBookingAction) (Config.cancelBookingButtonConfig state)
    ]
  ]

cancelBookingPopUpView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
cancelBookingPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    ] [PopUpModal.view (push <<< CancelBookingPopUpAC) (Config.cancelBookingPopupConfig state)]

softCancelStatusLottieLoader :: forall w. (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
softCancelStatusLottieLoader push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gravity CENTER
  , clickable true
  , background Color.blackLessTrans
  , visibility if state.props.showLoader then VISIBLE else GONE
  ][  lottieAnimationView
      [ height $ if os == "IOS" then V 170 else V 130
      , width $ V 130
      , padding $ PaddingBottom 80
      , margin (MarginTop ((screenHeight unit)/ 7 - (if os == "IOS" then 140 else 90)))
      , gravity CENTER
      , id (getNewIDWithTag "softCancelStatusLoader")
      , afterRender (\action -> do
        void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/search_loader.json", lottieId = (getNewIDWithTag "softCancelStatusLoader"), scaleType="CENTER_CROP", repeat = true, speed = 0.8 }
        push action
        ) (const NoAction)
      ]
    ]

cancelledBookingView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
cancelledBookingView push state = 
  linearLayout[
    width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation $ VERTICAL
  ][ cancelledBookingViewTop push state
   , cancelledBookingViewDetails push state
  ]

cancelledBookingViewTop :: forall w. (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
cancelledBookingViewTop push state = 
  let
    Tuple refundAmount refundVisibility = findRefundAmountAndVisibility state
  in
    linearLayout
      [ width $ MATCH_PARENT
      , height $ WRAP_CONTENT
      , gravity CENTER
      , margin $ MarginTop 20
      , orientation VERTICAL
      , padding $ Padding 20 0 20 0
      ]
      [ imageView
        [ width $ V 65
        , height $ V 65
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_payment_failed"
        ]
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString BOOKING_CANCELLED
        , color Color.black900
        , gravity CENTER
        , margin $ MarginTop 10
        ] <> FontStyle.h1 TypoGraphy
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString REFUND_IS_IN_PROCESS
        , color Color.black700
        , gravity CENTER
        , margin $ MarginTop 5
        , visibility refundVisibility
        ] <> FontStyle.subHeading1 TypoGraphy
      ]


cancelledBookingViewDetails :: forall w. (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
cancelledBookingViewDetails push state =
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 30 16 16
  , margin $ Margin 16 24 16 0
  , background Color.white900
  , cornerRadii $ Corners 8.0 true true true true
  ][
     metroHeaderView push state (FontStyle.body10 TypoGraphy) false
   , cancellationDetailsView push state
   , originAndDestinationView push state HORIZONTAL (FontStyle.body3 TypoGraphy) (FontStyle.body6 TypoGraphy) RIGHT
  ]

cancellationDetailsView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
cancellationDetailsView push state = 
  let
    Tuple totalRefund refundVisibility = findRefundAmountAndVisibility state
    noOfTickets = show $ length state.data.ticketsInfo
    cancellationDate = convertUTCtoISC state.data.bookingUpdatedAt "DD, MMM YYYY"
  in
    linearLayout [
      height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 10
    ][ fieldView push state (getString TOTAL_REFUND) (parseFloat totalRefund 0) refundVisibility
     , fieldView push state (getString CANCELLATION_DATE) cancellationDate VISIBLE  
     , fieldView push state (getString NO_OF_TICKETS) noOfTickets VISIBLE
    ]

findRefundAmountAndVisibility :: ST.MetroTicketDetailsScreenState -> Tuple Number Visibility
findRefundAmountAndVisibility state = 
  let
    totalRefund = fromMaybe 0.0 state.props.refundAmount
    refundVisibility = if totalRefund == 0.0 then GONE else VISIBLE
  in Tuple totalRefund refundVisibility

fieldView :: forall w. (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> String -> String -> Visibility -> PrestoDOM (Effect Unit) w
fieldView push state fieldName fieldValue visibility_ = 
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility visibility_
  ][ linearLayout 
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation HORIZONTAL
     ]
     [ textView $ 
         [ width WRAP_CONTENT
         , height WRAP_CONTENT
         , text fieldName
         , color Color.black700
         ] <> FontStyle.body3 TypoGraphy
     , linearLayout 
         [ height WRAP_CONTENT
         , width MATCH_PARENT
         , gravity RIGHT
         ]
         [ textView $ 
             [ width WRAP_CONTENT
             , height WRAP_CONTENT 
             , text fieldValue
             ] <> FontStyle.body6 TypoGraphy
         ]
     ]
   ,  linearLayout 
       [ width MATCH_PARENT
       , height $ V 1
       , background $ Color.ghostWhite
       , margin $ MarginVertical 12 12
       ]
       []
   ]

ticketDetailsView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
ticketDetailsView push state = 
  let 
    statusPillConfig = Config.getStatusPillConfig state 
  in
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    , id $ getNewIDWithTag "metro_ticket_details_view"
    ][ relativeLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][
        linearLayout[
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , padding $ Padding 16 30 16 16
        , margin $ Margin 16 24 16 0
        , background Color.white900
        , cornerRadii $ Corners 8.0 true true false false
        ][
          metroHeaderView push state (FontStyle.body20 TypoGraphy) true
        , qrCodeView push state
        , ticketNumberAndValidView push state
        ]    
      , linearLayout[
          width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ MarginTop 12
        , gravity CENTER_HORIZONTAL
        ] [statusPillView statusPillConfig]
      ]
    , linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , background Color.white900
      , padding $ Padding 16 30 16 16
      , margin $ MarginHorizontal 16 16 
      , cornerRadii $ Corners 8.0 false false true true 
      ][
        originAndDestinationView push state VERTICAL (FontStyle.body3 TypoGraphy) (FontStyle.body1 TypoGraphy) LEFT
      ]
    ]

statusPillView :: Config.StatusPillConfig -> forall w . PrestoDOM (Effect Unit) w
statusPillView (Config.StatusPillConfig statusPillConfig) = 
  linearLayout [
    width WRAP_CONTENT
  , height WRAP_CONTENT
  , padding $ Padding 8 5 8 5
  , cornerRadius 12.0
  , background statusPillConfig.statusPillColor
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ statusPillConfig.statusString
    , color Color.white900
    ] <> FontStyle.tags TypoGraphy
  ]

metroHeaderView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> (forall properties. (Array (Prop properties))) -> Boolean -> PrestoDOM (Effect Unit) w
metroHeaderView push state headerFontStyle detailVisibility = 
  let 
    cityConfig = getMetroConfigFromCity state.data.city
  in
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    ][
      imageView [
        width $ V 41
      , height $ V 41
      , imageWithFallback $ fetchImage FF_COMMON_ASSET cityConfig.logoImage
      ]
    , linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin $ MarginLeft 10
      , orientation VERTICAL
      ][
        textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ cityConfig.title
        , color Color.black800
        ] <> headerFontStyle
      , linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , margin $ MarginTop 3
        , gravity CENTER_VERTICAL
        , visibility $ boolToVisibility detailVisibility
        ] [
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString if state.data.ticketType == "SingleJourney" then ONWORD_JOURNEY else ROUND_TRIP_STR
          , color Color.black800
          ] <> FontStyle.tags TypoGraphy
        , linearLayout [
            width $ V 4
          , height $ V 4
          , cornerRadius 2.0
          , margin $ MarginHorizontal 6 6
          , background Color.black500
          , gravity CENTER_VERTICAL
          ][]
        , textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ (show $ state.data.noOfTickets) <> " " <> (getString $ if state.data.noOfTickets > 0 then TICKETS else TICKET)
          , color Color.black800
          ] <> FontStyle.tags TypoGraphy
        ]
      ]
    ]

qrCodeView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
qrCodeView push state = 
  let 
    currentTicket = state.data.ticketsInfo !! state.props.currentTicketIndex
    qrString = case currentTicket of 
                Just ticket -> ticket.qrString
                Nothing -> ""
    ticketStr = " " <> (getString $ if state.data.noOfTickets > 0 then TICKETS else TICKET)
    headerText = (show $ state.props.currentTicketIndex + 1) 
                  <> if state.data.noOfTickets > 1 then  "/" <> (show $ length state.data.ticketsInfo) else "" 
                  <> ticketStr
    qrAplha = case currentTicket of 
      Just ticket -> if ticket.status == "EXPIRED" || ticket.status == "USED" then 0.25 else 1.0
      Nothing -> 1.0
  in 
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 20
    , orientation VERTICAL
    , gravity CENTER
    , visibility $ boolToVisibility $ isJust currentTicket
    ][
      textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ headerText
      , color Color.black800
      , gravity CENTER
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , alpha qrAplha
      ][
        imageView [
          width $ V 32
        , height $ V 32
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_grey"
        , onClick push $ const PrevTicketClick
        , visibility $  boolToInvisibility $ state.data.noOfTickets > 1 
        ]
      , linearLayout [
          height WRAP_CONTENT
        , weight 1.0
        ][]
      , PrestoAnim.animationSet [ Anim.fadeInWithDelay 50 true ] $ imageView [
          width $ V 218
        , height $ V 218
        , id $ getNewIDWithTag "metro_ticket_qr_code"
        , onAnimationEnd push (const (TicketQRRendered (getNewIDWithTag "metro_ticket_qr_code") qrString))
        ]
      , linearLayout [
          height WRAP_CONTENT
        , weight 1.0
        ][]
      , imageView [
          width $ V 32
        , height $ V 32
        , visibility $  boolToInvisibility $ state.data.noOfTickets > 1 
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_grey"
        , onClick push $ const NextTicketClick
        ]
      ]
    ]

ticketNumberAndValidView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
ticketNumberAndValidView push state = 
  let 
    currentTicket = state.data.ticketsInfo !! state.props.currentTicketIndex
    ticketNumber = case currentTicket of 
                    Just ticket -> ticket.ticketNumber
                    Nothing -> ""
    validUntil = case currentTicket of 
                    Just ticket -> ticket.validUntil
                    Nothing -> ""
  in 
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , margin $ MarginTop 20
    , orientation VERTICAL
    , gravity CENTER
    ][
      linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , padding $ Padding 12 8 12 8
      , background Color.blue600 
      , cornerRadius $ if os == "IOS" then 18.0 else 51.0
      , gravity CENTER
      ][
        textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ (getString TICKET_NUMBER) <> ": " <> ticketNumber
        , color Color.black800
        ] <> FontStyle.body20 TypoGraphy
      ]
    , linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , margin $ MarginTop 10
      , gravity CENTER
      ][
        imageView [
          width $ V 16
        , height $ V 16
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock"
        , margin $ MarginRight 4
        ] 
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ (getString VALID_UNTIL) <> " " <> validUntil
        ] <> FontStyle.tags TypoGraphy
      ]
    ]

originAndDestinationView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> Orientation -> (forall properties. (Array (Prop properties))) -> (forall properties. (Array (Prop properties))) -> Gravity -> PrestoDOM (Effect Unit) w
originAndDestinationView push state fieldOrientation fieldFontStyle valueFontStyle valueGravity = 
  let originStation = state.data.metroRoute !! 0
      destinationStation = state.data.metroRoute !! ((length state.data.metroRoute) - 1)
      originConfig = case originStation of
        Nothing -> {
          name : ""
        , line : ST.NoColorLine
        }
        Just station -> {
          name : station.name
        , line : station.line
        }
      destinationConfig = case destinationStation of
        Nothing -> {
          name : ""
        , line : ST.NoColorLine
        }
        Just station -> {
          name : station.name
        , line : station.line
        }
  in
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    , cornerRadius 8.0
    ][
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation fieldOrientation
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][
          linearLayout [
            width $ V 8
          , height $ V 8
          , cornerRadius 4.0
          , background Color.green900
          ][]
        , textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text "Origin"
          , color Color.black700
          , margin $ MarginLeft 6
          ] <> fieldFontStyle
        ]
      , linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity valueGravity
        ][
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text originConfig.name
          ] <> valueFontStyle
        -- , linePillView originConfig.line -- need to enabled once metro line is available
        ]
      ]
    , linearLayout [
        width MATCH_PARENT
      , height $ V 1
      , background $ Color.ghostWhite
      , margin $ MarginVertical 12 12
      ][]
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation fieldOrientation
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ][
          linearLayout [
            width $ V 8
          , height $ V 8
          , cornerRadius 4.0
          , background Color.red900
          ][]
        , textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ getString DESTINATION
          , color Color.black700
          , margin $ MarginLeft 6
          ] <> fieldFontStyle
        ]
      , linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , gravity valueGravity 
        ][
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text destinationConfig.name
          ] <> valueFontStyle
        -- , linePillView destinationConfig.line -- need to enabled once metro line is available
        ]
      ]
    ]

paymentInfoView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
paymentInfoView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 16 20 16 20
  , margin $ Margin 16 20 16 16
  , onClick push $ const ViewPaymentInfoClick
  , background Color.white900
  , cornerRadius 8.0
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ getString VIEW_ROUTE_INFO
    ] <> FontStyle.body1 TypoGraphy
  , linearLayout [
      height WRAP_CONTENT
    , weight 1.0
    ][]
  , imageView [
      width $ V 16
    , height $ V 16
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_down"
    ]
  ]

linePillView :: forall w . ST.MetroLine -> PrestoDOM (Effect Unit) w
linePillView line = 
  let 
    pillConfig = case line of 
      ST.GreenLine -> {text : getString GREEN_LINE, color : Color.green900, bg : Color.tealishGreen}
      ST.BlueLine -> {text :  getString BLUE_LINE, color : Color.blue900, bg : Color.blue600}
      ST.RedLine -> {text : getString RED_LINE, color : Color.red900, bg : Color.red600}
      _ -> {text : "", color : Color.black800, bg : Color.black600}
  in 
    linearLayout [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding $ Padding 8 2 8 2
    , background pillConfig.bg
    , cornerRadius 4.0 
    , margin $ MarginLeft 8
    ][
      textView $ [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , text pillConfig.text
      , color pillConfig.color
      , padding $ PaddingBottom 4
      ] <> FontStyle.body15 TypoGraphy
    ]


mapView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
mapView push state = 
  PrestoAnim.animationSet [Anim.fadeIn true] $  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity START 
  ][
    imageView [
      width MATCH_PARENT
    , height $ V (screenWidth unit)
    , margin $ MarginTop 24
    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chennai_metro_map"
    ]
  ]

routeDetailsView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketDetailsScreenState -> PrestoDOM (Effect Unit) w
routeDetailsView push state = 
  PrestoAnim.animationSet [Anim.fadeIn true] $ linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 24 16 0
  , padding $ Padding 16 16 16 16
  , background Color.white900 
  , cornerRadius 8.0
  , orientation VERTICAL
  ] $ mapWithIndex (\index route -> routeDetailsItemView push index route) state.data.metroRoute

routeDetailsItemView :: forall w . (Action -> Effect Unit) -> Int -> ST.MetroRoute -> PrestoDOM (Effect Unit) w
routeDetailsItemView push index routeDetails = 
  let 
    noOfStops = length routeDetails.stops
    pillText = case routeDetails.line of 
                ST.GreenLine -> getString GREEN_LINE
                ST.BlueLine -> getString BLUE_LINE
                ST.RedLine -> getString RED_LINE
                _ -> ""
  in
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL 
    , margin $ MarginVertical 4 4
    ][
      linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][
        linearLayout [
          width $ V 20
        , height $ V 20
        , background metroPrimaryColor
        , cornerRadius 10.0
        , gravity CENTER
        ][
          imageView [
            width $ V 9
          , height $ V 12
          , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_metro_logo_mini"
          ]
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ DS.take 40 routeDetails.name <> "..."
        , margin $ MarginLeft 8
        ] <> FontStyle.body1 TypoGraphy
      ]
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][
        linearLayout [
          width $ V 2
        , height MATCH_PARENT
        , margin $ Margin 9 8 9 0
        , background Color.grey900
        , visibility if noOfStops > 0 then VISIBLE else INVISIBLE
        ][]
      , linearLayout [
          width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ] $ [
          linearLayout [
            width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ MarginTop 8
          , visibility $ boolToVisibility $ noOfStops > 0
          ][
            linePillView routeDetails.line
          , linearLayout [
              height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding $ Padding 8 2 8 2
            , background pillBG
            , cornerRadius 4.0
            , margin $ MarginLeft 8 
            , onClick push $ const $ StopsBtnClick index
            , gravity CENTER
            ][
              textView $ [
                width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ (show noOfStops) <> " " <> (getString STOPS)
              , padding $ PaddingBottom 4
              , color metroPrimaryColor
              ] <> FontStyle.tags TypoGraphy
              , imageView [
                width $ V 20
              , height $ V 20
              , padding $ PaddingLeft 8
              , imageWithFallback $ fetchImage FF_COMMON_ASSET (if routeDetails.listExpanded then "ny_ic_chevron_up" else "ny_ic_chevron_down" )
              ]
            ]
          ]
        ] <> if routeDetails.listExpanded then [stopsListView] else [] 
      ]
    ]
  where 
    metroPrimaryColor = case routeDetails.line of 
                      ST.GreenLine -> Color.green900
                      ST.BlueLine -> Color.blue800 
                      ST.RedLine -> Color.red900
                      _ -> Color.black800

    pillBG = case routeDetails.line of 
                ST.GreenLine -> Color.tealishGreen
                ST.BlueLine -> Color.blue600 
                ST.RedLine -> Color.red600
                _ -> Color.black600

    stopsListView :: forall w . PrestoDOM (Effect Unit) w
    stopsListView = 
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginTop 8
      ] $ mapWithIndex (\index stop -> stopItemView index stop) routeDetails.stops
    
    stopItemView :: forall w . Int -> ST.MetroStop -> PrestoDOM (Effect Unit) w
    stopItemView index stop = 
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ MarginTop 8
      , gravity CENTER_VERTICAL
      ][
        linearLayout [
          width $ V 16
        , height $ V 16
        , cornerRadius 8.0
        , background pillBG
        , gravity CENTER
        ][ 
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ show $ index + 1
          , color metroPrimaryColor
          , padding $ PaddingBottom 4
          ] <> FontStyle.body16 TypoGraphy
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text stop.name
        , padding $ PaddingBottom 4
        , margin $ MarginLeft 8
        ] <> FontStyle.body1 TypoGraphy
      ]