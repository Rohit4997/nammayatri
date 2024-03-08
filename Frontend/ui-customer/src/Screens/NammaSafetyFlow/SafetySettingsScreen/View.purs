{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetySettingsScreen.View where

import Animation (fadeIn, screenAnimation)
import Prelude (Unit, bind, const, discard, not, pure, unit, void, ($), (&&), (/=), (<<<), (<>), (==), (<$>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, alignParentBottom, background, color, cornerRadius, gravity, height, imageUrl, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, singleLine, stroke, text, textFromHtml, textView, visibility, weight, width)
import Screens.NammaSafetyFlow.ComponentConfig (goToDrillButtonConfig, shareTripPopupConfig, startNSOnboardingButtonConfig, pastRideSOSConfirmationPopConfig)
import Screens.NammaSafetyFlow.Components.HelperViews as HV
import Common.Types.App (LazyCheck(..))
import Components.PopupWithCheckbox.View as PopupWithCheckbox
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (mapWithIndex, null)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude (boolToVisibility)
import Presto.Core.Types.Language.Flow (fork, await, doAff, Flow)
import PrestoDOM.Animation as PrestoAnim
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.SafetySettingsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (NammaSafetyScreenState, SafetySetupStage(..))
import Services.API (RideShareOptions(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState, GlobalState(..))
import Engineering.Helpers.Commons (liftFlow)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Helpers.Pooling (delay)
import Data.Time.Duration (Milliseconds(..))
import Components.PopUpModal as PopUpModal

screen :: NammaSafetyScreenState -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState =
  { initialState
  , view: view
  , name: "SafetySettingsScreen"
  , globalEvents:
      [ ( \push -> do
            void $ launchAff $ EHC.flowRunner defaultGlobalState $ checkAndStatus push initialState
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "SafetySettingsScreen action " action
        let
          _ = spy "SafetySettingsScreen state " state
        eval action state
  }

checkAndStatus :: forall st. (Action -> Effect Unit) -> NammaSafetyScreenState -> Flow GlobalState Unit
checkAndStatus push state = do
  mbRideListResp <- getLastRide state.props.checkPastRide
  eiResponse <- Remote.getEmergencySettings ""
  case eiResponse of
    Right response -> do
      case mbRideListResp of
        Nothing -> pure unit
        Just control -> do
          eiResp <- await control
          liftFlow $ handleLastRide eiResp
      liftFlow $ push $ UpdateEmergencySettings response
      liftFlow $ push $ DisableShimmer
    Left _ -> pure unit
  EHU.toggleLoader false
  where
  getLastRide checkPastRide =
    if checkPastRide then do
      listControl <- fork $ Remote.rideBookingListWithStatus "1" "0" "COMPLETED"
      pure $ Just $ listControl
    else
      pure $ Nothing

  handleLastRide eiResp = case eiResp of
    Right resp -> push $ CheckRideListResp resp
    Left err -> pure unit

view :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , onBackPressed push $ const BackPressed
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background Color.white900
            , padding padding'
            , onBackPressed push $ const BackPressed
            ]
            [ Header.view (push <<< SafetyHeaderAction) headerConfig
            , dashboardView state push
            , HV.shimmerView state
            ]
        , if state.props.showRideShareOptionsPopup then PopupWithCheckbox.view (push <<< ShareTripOptionPopup) $ shareTripPopupConfig state else HV.emptyTextView
        , if state.props.showPastRidePopUp then PopUpModal.view (push <<< PopUpModalAC) $ pastRideSOSConfirmationPopConfig state else HV.emptyTextView
        ]
  where
  padding' =
    if EHC.os == "IOS" then
      (Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 16 else EHC.safeMarginBottom))
    else
      (PaddingLeft 0)

  headerConfig = (Header.config Language) { showLearnMore = state.data.hasCompletedSafetySetup }

------------------------------------- dashboardView -----------------------------------
dashboardView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
dashboardView state push =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , visibility $ boolToVisibility $ not state.props.showShimmer
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.greySmoke
            ]
            []
        , nammaSafetyFeaturesView state push $ not state.data.hasCompletedSafetySetup
        , userSettingsView state push state.data.hasCompletedSafetySetup
        ]
    ]

-- ---------------------------------- nammaSafetyFeaturesView -----------------------------------
nammaSafetyFeaturesView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
nammaSafetyFeaturesView state push visibility' =
  PrestoAnim.animationSet
    [ fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , visibility $ boolToVisibility visibility'
        ]
        [ featuresView state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , background Color.white900
            , alignParentBottom "true,-1"
            ]
            [ PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
        ]

featuresView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
featuresView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.blue600
        , gravity CENTER
        , orientation VERTICAL
        , cornerRadius 12.0
        , margin $ Margin 16 20 16 6
        , stroke $ "1," <> Color.blue600
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_safety_shield"
            , width $ V 220
            , height $ V 114
            ]
        , textView
            $ [ text $ getString NAMMA_SAFETY_WILL_ENABLE_ACCESS
              , margin $ Margin 16 20 16 4
              , color Color.black800
              , width MATCH_PARENT
              ]
            <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ]
            [ imageWithTextView (getString AUTOMATIC_CALL_PLACED_TO_EMERGENCY_CONTACTS) true
            , imageWithTextView (getString $ EMERGENCY_CONTACTS_CAN_FOLLOW "EMERGENCY_CONTACTS_CAN_FOLLOW") true
            , imageWithTextView (getString GET_OPTIONS_TO_DIRECTLY_CALL_POLICE) true
            , imageWithTextView (getString $ ALERT_SAFETY_TEAM "ALERT_SAFETY_TEAM") true
            , imageWithTextView (getString OPTION_TO_REPORT_A_SAFETY_ISSUE) true
            ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 12 16 0
            , background Color.grey900
            ]
            []
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER
            , padding $ PaddingVertical 16 20
            , onClick push $ const $ GoToEducationView
            ]
            [ textView
                $ [ textFromHtml $ "<u>" <> getString LEARN_MORE <> "</u>"
                  , color Color.blue800
                  ]
                <> FontStyle.body1 TypoGraphy
            ]
        ]
    ]

imageWithTextView :: String -> Boolean -> forall w. PrestoDOM (Effect Unit) w
imageWithTextView text' isActive =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ PaddingHorizontal 16 16
    , margin $ MarginTop 12
    ]
    [ imageView
        [ imageWithFallback $ fetchImage FF_ASSET if isActive then "ny_ic_check" else "ny_ic_ellipse_outline_grey"
        , height $ V 20
        , width $ V 20
        , margin $ MarginRight 8
        ]
    , textView
        $ [ text text'
          , color Color.black800
          , weight 1.0
          , height WRAP_CONTENT
          , singleLine false
          ]
        <> FontStyle.tags TypoGraphy
    ]

userSettingsView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> Boolean -> forall w. PrestoDOM (Effect Unit) w
userSettingsView state push visibility' =
  PrestoAnim.animationSet
    [ fadeIn true
    ]
    $ relativeLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , visibility $ boolToVisibility visibility'
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation VERTICAL
            , padding $ PaddingTop 16
            ]
            [ toggleSwitchViewLayout (ToggleSwitch SetDefaultEmergencyContacts) state.data.shareToEmergencyContacts (getString EMERGENCY_SHARING_WITH_CONTACTS) push true 16
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER_VERTICAL
                , padding $ Padding 16 8 16 0
                ]
                [ textView
                    $ [ text $ getString SHARING_WITH
                      , color Color.black700
                      , margin $ MarginRight 8
                      , gravity CENTER
                      , visibility $ boolToVisibility $ not $ null state.data.emergencyContactsList
                      ]
                    <> FontStyle.body3 TypoGraphy
                , linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    ]
                    (mapWithIndex (\index item -> ContactCircle.view (ContactCircle.getContactConfig item index false) (push <<< ContactAction)) state.data.emergencyContactsList)
                , textView
                    $ [ text $ getString if null state.data.emergencyContactsList then ADD_CONTACTS else EDIT
                      , color Color.blue900
                      , margin $ MarginLeft 8
                      , gravity CENTER
                      , onClick push $ const $ EditEmergencyContacts
                      ]
                    <> FontStyle.body2 TypoGraphy
                ]
            , HV.separatorView Color.lightGreyShade $ Margin 16 16 16 16
            , toggleSwitchViewLayout (ToggleSwitch SetNightTimeSafetyAlert) state.data.nightSafetyChecks (getString NIGHT_TIME_SAFETY_CHECKS) push true 16
            , HV.separatorView Color.lightGreyShade $ Margin 16 16 16 16
            , linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                ]
                [ textView
                    $ [ text shareSettingsText
                      , color Color.black800
                      , padding $ Padding 16 16 0 16
                      , weight 1.0
                      ]
                    <> FontStyle.body1 TypoGraphy
                , textView
                    $ [ text $ getString EDIT
                      , color Color.blue900
                      , margin $ MarginLeft 8
                      , gravity CENTER
                      , padding $ Padding 16 16 16 16
                      , onClick push $ const ShowShareTripOptions
                      ]
                    <> FontStyle.body2 TypoGraphy
                ]
            , textView
                $ [ text $ getString $ WHO_CAN_TRACK_YOUR_RIDE "WHO_CAN_TRACK_YOUR_RIDE"
                  , color Color.black700
                  , margin $ Margin 16 16 16 16
                  , visibility $ boolToVisibility $ (not $ null state.data.emergencyContactsList) && state.data.shareTripWithEmergencyContactOption /= NEVER_SHARE
                  ]
                <> FontStyle.body1 TypoGraphy
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , visibility $ boolToVisibility $ (not $ null state.data.emergencyContactsList) && state.data.shareTripWithEmergencyContactOption /= NEVER_SHARE
                ]
                ( mapWithIndex
                    ( \index item ->
                        linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , margin $ Margin 16 16 0 0
                          , gravity CENTER_VERTICAL
                          ]
                          [ ContactCircle.view (ContactCircle.getContactConfig item index false) (push <<< ContactAction)
                          , toggleSwitchViewLayout (ChangeFollowing index) item.enableForFollowing item.name push true 12
                          ]
                    )
                    state.data.emergencyContactsList
                )
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            , visibility $ boolToVisibility $ getValueToLocalStore IS_SOS_ACTIVE /= "true" && state.data.shareToEmergencyContacts
            ]
            [ PrimaryButton.view (push <<< StartTestDrill) (goToDrillButtonConfig state) ]
        ]
  where
  shareSettingsText = case state.data.shareTripWithEmergencyContactOption of
    ALWAYS_SHARE -> getString ALWAYS_SHARE_DESC
    SHARE_WITH_TIME_CONSTRAINTS -> getString NIGHT_RIDES_DESC
    NEVER_SHARE -> getString NEVER_SHARE_DESC

toggleSwitchViewLayout :: Action -> Boolean -> String -> (Action -> Effect Unit) -> Boolean -> Int -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchViewLayout action isActive text' push visibility' marginLeft =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginHorizontal marginLeft 16
    ]
    [ textView
        $ [ text text'
          , weight 1.0
          , color Color.black800
          ]
        <> FontStyle.body2 TypoGraphy
    , toggleSwitchView isActive true action push
    ]

toggleSwitchView :: Boolean -> Boolean -> Action -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
toggleSwitchView isActive visibility' action push =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , onClick push $ const action --ToggleSwitch stage
    , visibility $ boolToVisibility visibility'
    ]
    [ imageView
        [ imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
        , width $ V 40
        , height $ V 24
        ]
    ]
