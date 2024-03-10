{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SearchLocationScreen.ScreenData where

import Screens.Types (SearchLocationScreenState, SearchLocationStage(..), SearchLocationTextField(..), SearchLocationActionType(..), LocationInfo, ZoneType(..))
import ConfigProvider
import Screens (ScreenName(..), getScreen)
import Data.Maybe (Maybe(..))
import Services.API (PlaceName(..), LatLong(..))
import Components.LocationListItem.Controller (locationListStateObj, dummyAddress)

initData :: SearchLocationScreenState 
initData = {
  data : { srcLoc : Nothing
         , destLoc : Nothing
         , currentLoc : Nothing 
         , locationList : []
         , fromScreen : getScreen HOME_SCREEN -- getScreen RENTAL_SCREEN
         , saveFavouriteCard : {
              address : ""
            , tag : ""
            , tagExists : false
            , selectedItem : locationListStateObj
            , isBtnActive : false
        }
        , latLonOnMap : dummyLocationInfo
        , defaultGate : ""
        , nearByGates : []
        , specialZoneCoordinates : ""
        , confirmLocCategory : NOZONE
        , metroStations : []
        , updatedMetroStations : []
  } ,
  props : {
    searchLocStage : PredictionsStage ,
    focussedTextField : Nothing , 
    actionType : AddingStopAction ,
    showSaveFavCard : false ,
    areBothLocMandatory : false,
    canSelectFromFav : true,
    showLoader : false,
    canClearText : false,
    locUnserviceable : false,
    isAutoComplete : false
  },
  appConfig : getAppConfig appConfig
}

dummyLocationName :: PlaceName
dummyLocationName = PlaceName {
  "formattedAddress" : "",
  "location" : LatLong{
    "lat" : 0.0,
    "lon" : 0.0
  },
  "plusCode" : Nothing,
  "addressComponents" : [],
  "placeId" : Nothing
}

dummyLocationInfo :: LocationInfo 
dummyLocationInfo = {
  lat : Nothing ,
  lon : Nothing ,
  placeId : Nothing,
  address : "",
  addressComponents : dummyAddress,
  city : Nothing ,
  stationCode : "",
  metroInfo : Nothing
}