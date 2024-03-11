module Components.StepsHeaderModel.View where

import Data.Array (mapWithIndex)
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, (<>), bind, ($), pure, unit, show, (+), (>=), (&&), (>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), Visibility(..), Accessiblity(..), PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback, lottieAnimationView, id, afterRender, visibility, background, padding, accessibilityHint, accessibility, rippleColor, cornerRadius)
import Styles.Colors as Color
import Components.StepsHeaderModel.Controller (Action(..), StepsHeaderModelState)
import Data.Array as Array
import Data.Maybe as Maybe
import Common.Types.App (LazyCheck(..))
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Mobility.Prelude (boolToVisibility)

view :: forall w . (Action -> Effect Unit) -> StepsHeaderModelState -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background state.primaryBackground
    , padding state.padding
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin state.margin
        , padding state.padding
        , gravity CENTER_VERTICAL
        ][  imageView
            [ height $ V 35
            , width $ V 35
            , imageWithFallback $ fetchImage FF_ASSET "ny_ic_chevron_left_white"
            , onClick push $ const OnArrowClick
            , accessibilityHint "Back"
            , accessibility ENABLE
            , cornerRadius 20.0
            , rippleColor Color.rippleShade
            , padding $ Padding 7 7 7 7
            , visibility $ boolToVisibility state.backArrowVisibility
            ]
          , linearLayout
            [ width WRAP_CONTENT
            , height MATCH_PARENT
            , gravity CENTER_VERTICAL
            , margin $ MarginHorizontal 5 5
            , weight 1.0
            ](mapWithIndex (\index item -> 
              linearLayout
              [ height $ V 1
              , weight 1.0
              , background if state.activeIndex >= index then Color.white900 else Color.black800
              , margin $ MarginRight 15
              ][]) state.textArray)
          , textView $
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , accessibility ENABLE
            , accessibilityHint $ "Step " <> (show (state.activeIndex + 1)) <> " Of "<> (show (Array.length state.textArray))
            , text $ state.stepsTranslation <> " "<> (show (state.activeIndex + 1)) <> "/"<> (show (Array.length state.textArray))
            , color Color.white900
            ] <> FontStyle.body3 TypoGraphy

        ]
      , textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , accessibility ENABLE
        , accessibilityHint $ Maybe.fromMaybe "" (state.textArray Array.!! state.activeIndex)
        , text $ Maybe.fromMaybe "" (state.textArray Array.!! state.activeIndex)
        , color Color.white900
        , margin $ Margin 15 5 0 22
        ] <> FontStyle.h1 TypoGraphy
    ]