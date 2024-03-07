{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.MetroMyTickets.View where

import Prelude
import Common.Types.App (LazyCheck(..))

import Data.Array 
import Presto.Core.Types.Language.Flow 
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
import Screens.TicketBookingFlow.MetroMyTickets.Controller
import Screens.Types as ST
import Styles.Colors as Color
import Data.Array as DA
import Font.Size as FontSize
import Mobility.Prelude
import Debug
import Helpers.Utils
import Animation as Anim
import Language.Strings
import Language.Types
import Services.Backend as Remote
import Engineering.Helpers.Commons as EHC
import Services.API as API
import Engineering.Helpers.Utils as EHU

screen :: ST.MetroMyTicketsScreenState -> Screen Action ST.MetroMyTicketsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroMyTickets"
  , globalEvents : [
      ( \push -> do
        void $ launchAff_ $ void $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do 
          void $ lift $ lift $ EHU.toggleLoader true
          (API.GetMetroBookingListResp resp)<- Remote.getMetroBookingStatusListBT
          lift $ lift $ doAff do liftEffect $ push $ MetroBookingListRespAC resp
          void $ lift $ lift $ EHU.toggleLoader false
        pure $ pure unit
      )
    ]
  , eval :
    \action state -> do
        let _ = spy "MetroMyTickets action " action
        let _ = spy "MetroMyTickets state " state
        eval action state
  }

view :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
view push state = 
  Anim.screenAnimation $ linearLayout[
    width MATCH_PARENT
  , height MATCH_PARENT
  , background Color.white900
  , clickable true
  , onBackPressed push $ const BackPressed
  , orientation VERTICAL
  , afterRender push $ const AfterRender
  ] [ headerView push state
        , scrollableView push state
        ]

shimmerView :: forall w . ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout[ 
    width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , visibility $ boolToVisibility state.props.showShimmer
  ][ 
    linearLayout [ 
      width MATCH_PARENT
    , height (V 235)
    , margin (Margin 16 15 16 0)
    , background Color.greyDark
    , cornerRadius 16.0
    ] []
  , linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin (MarginTop 258)
    ] (DA.mapWithIndex 
        (\index item ->
            linearLayout
              [ width MATCH_PARENT
              , height (V 60)
              , margin (Margin 16 16 16 0)
              , cornerRadius 12.0
              , background Color.greyDark
              ][]
        ) (1 .. 7)
      )
    ]


headerView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
headerView push state = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ PaddingBottom 1
  , background Color.grey900
  , margin $ MarginTop safeMarginTop
  ][
    linearLayout[
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 16 16 15
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
      , text $ getString MY_TICKETS
      , color Color.black800
      , margin $ MarginLeft 8
      ] <> FontStyle.subHeading1 TypoGraphy 
    ]
  ]

scrollableView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
scrollableView push state = 
  scrollView [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ] [
        activeTicketsListView push state
      , pastTicketsListView push state
    ]
  ]

activeTicketsListView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
activeTicketsListView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 24 16 0
  , orientation VERTICAL
  , visibility $ if not $ null state.data.activeTickets then VISIBLE else GONE
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ getString ACTIVE_TICKETS
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 13
    ] $ map (\ ticket -> activeTicketView push ticket) state.data.activeTickets
  ]

activeTicketView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketCardData -> PrestoDOM (Effect Unit) w
activeTicketView push ticketCard = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT 
  , padding $ Padding 1 1 1 1
  , cornerRadius 8.0
  , orientation VERTICAL
  , background Color.grey900
  , margin $ MarginBottom 16
  , onClick push $ const $ TicketPressed ticketCard.metroTicketStatusApiResp
  ][ 
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT 
    , padding $ Padding 16 16 16 16
    , cornerRadius 8.0
    , orientation VERTICAL
    , background Color.white900
    ][
      linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      ][
        linearLayout [
          width WRAP_CONTENT
        , height MATCH_PARENT
        , gravity CENTER_VERTICAL
        ][
          imageView [
            width $ V 41
          , height $ V 41
          , imageWithFallback $ fetchImage FF_COMMON_ASSET (getMetroLogoImage ticketCard)
          ]
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
          , text $ ticketCard.sourceName <> " to " <> ticketCard.destinationName
          , color Color.black800
          ] <> FontStyle.body1 TypoGraphy
        , linearLayout [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin $ MarginTop 3
          , gravity CENTER_VERTICAL
          ] [
            textView $ [
              width WRAP_CONTENT
            , height WRAP_CONTENT
            , text ticketCard.createdAt
            , color Color.black700
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
            , text $ (show ticketCard.noOfTickets) <> " " <> (getString TICKETS)
            , color Color.black700
            ] <> FontStyle.tags TypoGraphy
          ]
        ]
      ]
    , linearLayout [
        width MATCH_PARENT
      , height $ V 1
      , margin $ MarginVertical 16 16
      , background Color.grey900
      , visibility $ boolToVisibility $ ticketCard.status /= "PAYMENT_PENDING"
      ][]
    , linearLayout [
        width MATCH_PARENT
      , height WRAP_CONTENT
      , visibility $ boolToVisibility $ ticketCard.status /= "PAYMENT_PENDING"
      ][
        linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
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
          , text $ (getString VALID_UNTIL) <> " " <> ticketCard.validUntill
          ] <> FontStyle.tags TypoGraphy
        ]
      -- , linearLayout [
      --     height WRAP_CONTENT
      --   , weight 1.0
      --   ][]
      -- , linearLayout [
      --     width WRAP_CONTENT
      --   , height MATCH_PARENT
      --   , gravity CENTER_VERTICAL 
      --   ][
      --     imageView [
      --       width $ V 16
      --     , height $ V 16
      --     , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_share"
      --     ]
      --   , textView $ [
      --       width WRAP_CONTENT
      --     , height WRAP_CONTENT
      --     , text "Share"
      --     , color Color.blue900
      --     , margin $ MarginLeft 8
      --     ] <> FontStyle.tags TypoGraphy
      --   ]
      ]
    ]
  ]

pastTicketsListView :: forall w . (Action -> Effect Unit) -> ST.MetroMyTicketsScreenState -> PrestoDOM (Effect Unit) w
pastTicketsListView push state = 
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 32 16 0
  , orientation VERTICAL
  , visibility $ if not $ null state.data.pastTickets then VISIBLE else GONE
  ][
    textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , text $ getString PAST_TICKETS
    , color Color.black900
    ] <> FontStyle.subHeading1 TypoGraphy
  , linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 13
    , orientation VERTICAL
    ] $ map (\ticket ->  pastTicketView push ticket) state.data.pastTickets
  ]

pastTicketView :: forall w . (Action -> Effect Unit) -> ST.MetroTicketCardData -> PrestoDOM (Effect Unit) w
pastTicketView push ticketCard = 
  let status = case ticketCard.status of
        "PAYMENT_PENDING" -> getString PENDING_STR 
        "CONFIRMING" -> getString CONFIRMING_STR 
        "FAILED" -> getString FAILED_STR
        "CANCELLED" -> getString CANCELLED
        "CONFIRMED" -> getString CONFIRMED_STR
        "EXPIRED" -> "Exipired" -- getString EXPIRED_STR
        _ -> ""
      statusColor = case ticketCard.status of
        "PAYMENT_PENDING" -> Color.yellow900
        "CONFIRMING" -> Color.yellow900
        "FAILED" -> Color.red900
        "CANCELLED" -> Color.red900
        "CONFIRMED" -> Color.green900
        "EXPIRED" -> Color.black800 -- getString EXPIRED_STR
        _ -> Color.black900
      statusIcon = case ticketCard.status of
        "PAYMENT_PENDING" -> fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock"
        "CONFIRMING" -> fetchImage FF_COMMON_ASSET "ny_ic_yellow_clock"
        "FAILED" -> fetchImage FF_COMMON_ASSET "ny_ic_red_triangle_warning"
        "CANCELLED" -> fetchImage FF_COMMON_ASSET "ny_ic_cross"
        "CONFIRMED" -> fetchImage FF_COMMON_ASSET "ny_ic_green_tick"
        "EXPIRED" -> fetchImage FF_ASSET "ny_ic_info"
        _ ->  ""
  in
  linearLayout [
    width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 1 1 1 1
  , cornerRadius 8.0
  , margin $ MarginBottom 16
  , background Color.grey900
  , gravity CENTER
  , onClick push $ const $ TicketPressed ticketCard.metroTicketStatusApiResp
  ][
    linearLayout [
      width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ Padding 16 16 16 16
    , cornerRadius 8.0
    , background Color.white900
    ][
      linearLayout [
        width WRAP_CONTENT
      , height MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][
        imageView [
          width $ V 30
        , height $ V 30
        , imageWithFallback $ fetchImage FF_COMMON_ASSET (getMetroLogoImage ticketCard)
        ]
      ]
    , linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin $ MarginLeft 9
      , orientation VERTICAL
      ][
        textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ ticketCard.sourceName <> " to " <> ticketCard.destinationName
        , color Color.black800
        ] <> FontStyle.body1 TypoGraphy
      , linearLayout [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , margin $ MarginTop 3
        , gravity CENTER_VERTICAL
        ] [
          textView $ [
            width WRAP_CONTENT
          , height WRAP_CONTENT
          , text ticketCard.createdAt
          , color Color.black700
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
          , text $ (show ticketCard.noOfTickets) <> " " <> (getString TICKETS)
          , color Color.black700
          ] <> FontStyle.tags TypoGraphy
        ]
      , linearLayout [
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER_VERTICAL
      , margin $ MarginTop 6
      ][
        imageView [
          width $ V 16
        , height $ V 16
        , imageWithFallback statusIcon
        , margin $ MarginRight 4
        ]
      , textView $ [
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ status
        , color statusColor
        ] <> FontStyle.body3 TypoGraphy
      ]
    ]
  ]
]

getMetroLogoImage :: ST.MetroTicketCardData -> String
getMetroLogoImage ticketCard = 
  let
    (API.MetroTicketBookingStatus resp) = ticketCard.metroTicketStatusApiResp
    city = getCityNameFromCode $ Just resp.city
    config = getMetroConfigFromCity city
  in
    config.logoImage
 