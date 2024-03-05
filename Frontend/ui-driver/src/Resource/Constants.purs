{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Resource.Constants where

import Common.Types.App as Common
import Data.Array as DA
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, toLower)
import Data.String (trim)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude ((==), (&&), (<>), ($))
import Screens.Types as ST
import Services.API (LocationInfo(..), StopLocation(..), StopLocationAddress(..), TripCategory(..))

type Language =
    {
        name :: String,
        value :: String
    }

getLanguages :: Array Language
getLanguages = 
    [
        {name:"English",value:"EN_US"},
        {name:"ಕನ್ನಡ",value:"KN_IN"},
        {name:"हिन्दी",value :"HI_IN"},
        {name:"தமிழ்",value :"TA_IN"},
        {name:"తెలుగు", value : "TE_IN"}
    ]

decodeAddress :: LocationInfo -> Boolean -> String
decodeAddress ( LocationInfo address) fullAddress =
        if fullAddress then 
             if ( trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.street) == "" && trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "" ) then
                    ((fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.door) == "" && trim (fromMaybe "" address.building) == "") then
                    ((fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.door) == "") then
                    ((fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
                    else
                    ((fromMaybe "" address.door) <> ", " <> (fromMaybe "" address.building) <> ", " <> (fromMaybe "" address.street) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
        else 
            if ( trim (fromMaybe "" address.city) == "" && trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == ""  ) then
                    (trim (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.area) == "" && trim (fromMaybe "" address.street) == "" ) then
                    (trim (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
            else if ( trim (fromMaybe "" address.street) == "") then
                    (trim (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country))
                    else
                    (trim (fromMaybe "" address.street)) <> ", " <> (fromMaybe "" address.area) <> ", " <> (fromMaybe "" address.city) <> ", " <> (fromMaybe "" address.state) <> ", " <> (fromMaybe "" address.country)


tripDatesCount :: Int
tripDatesCount = 15

getPspIcon :: String -> String 
getPspIcon vpa = do
    let handleName = ((split (Pattern "@") (vpa)) DA.!! 1)
    case handleName of 
        Nothing -> "ny_ic_defaultpg"
        Just handle -> case handle of
            "ybl" -> "ny_ic_phonepe"
            "ibl" -> "ny_ic_phonepe"
            "axl" -> "ny_ic_phonepe"
            "okhdfcbank" -> "ny_ic_gpay"
            "okicici" -> "ny_ic_gpay"
            "oksbi" -> "ny_ic_gpay"
            "okaxis" -> "ny_ic_gpay"
            "paytm" -> "ny_ic_paytm"
            "apl" -> "ny_ic_amazonpay"
            "yapl" -> "ny_ic_amazonpay"
            "indus" -> "ny_ic_induspay"
            "upi" -> "ny_ic_bhim"
            _ -> "ny_ic_defaultpg"

waitTimeConstructor :: String -> ST.TimerStatus
waitTimeConstructor key = case key of
  "NoStatus" -> ST.NoStatus
  "Triggered" -> ST.Triggered
  "PostTriggered" -> ST.PostTriggered
  _ -> ST.NoStatus

rideTypeConstructor :: Maybe TripCategory -> ST.TripType
rideTypeConstructor ( tripCategory) = 
        case tripCategory of
            Nothing -> ST.OneWay
            Just (TripCategory tripCategory') ->
                case toLower tripCategory'.tag of
                        "oneway" -> ST.OneWay 
                        "roundtrip" -> ST.RoundTrip
                        "rental" -> ST.Rental
                        "intercity" -> ST.Intercity
                        "rideshare" -> ST.RideShare
                        _ -> ST.OneWay

constructLocationInfo :: Maybe Number -> Maybe Number -> Maybe LocationInfo
constructLocationInfo (latitude) (longitude) = 
    case latitude,longitude of
        Just latitude',Just longitude' -> 
                Just $ LocationInfo {
                        area :  Just "",
                        state :  Just "",
                        country :  Just "",
                        building :  Just "",
                        door : Just "",
                        street :  Just "",
                        lat : latitude',
                        city :  Just "",
                        areaCode :  Just "",
                        lon : longitude'
                }
        _,_ -> Nothing

getLocationInfoFromStopLocation :: StopLocationAddress -> Number -> Number -> LocationInfo
getLocationInfoFromStopLocation (StopLocationAddress {door, building, street, area, city, state, country, areaCode}) lat lon = 
     LocationInfo 
        {
                area :  area,
                state :  state,
                country :  country,
                building :  building,
                door : door,
                street :  street,
                lat : lat,
                city :  city,
                areaCode :  areaCode,
                lon : lon
        }

getHomeStageFromString :: String -> ST.HomeScreenStage
getHomeStageFromString localStage = 
  case localStage of
        "HomeScreen" -> ST.HomeScreen
        "RideRequested" -> ST.RideRequested
        "RideAccepted" -> ST.RideAccepted
        "RideStarted" -> ST.RideStarted
        "RideCompleted" -> ST.RideCompleted
        "ChatWithCustomer" -> ST.ChatWithCustomer
        _ -> ST.HomeScreen