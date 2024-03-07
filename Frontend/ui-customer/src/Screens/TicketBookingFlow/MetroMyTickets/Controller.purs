{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroMyTickets.Controller where


import Log 
import Prelude 
import PrestoDOM (Eval, continue, exit)
import Screens 
import Screens.Types 
import Helpers.Utils 
import Effect.Uncurried 
import Effect.Unsafe 
import Screens.Types 
import Common.Types.App as Common
import Language.Strings
import Language.Types
import PrestoDOM.Types.Core (class Loggable)
import Services.API
import Screens.TicketBookingFlow.MetroMyTickets.Transformer (metroTicketListApiToMyTicketsTransformer)


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = NoAction
            | BackPressed
            | AfterRender
            | TicketPressed MetroTicketBookingStatus
            | PastTicketPressed MetroTicketBookingStatus
            | MetroBookingListRespAC (Array MetroTicketBookingStatus)

data ScreenOutput = NoOutput
                  | GoToMetroTicketDetailsFlow String
                  | GoToMetroTicketStatusFlow MetroTicketBookingStatus
                  | GoToHomeScreen
                  | GoToMetroBooking


eval :: Action -> MetroMyTicketsScreenState -> Eval Action ScreenOutput MetroMyTicketsScreenState


eval (MetroBookingListRespAC bookingList) state = 
  continue $ metroTicketListApiToMyTicketsTransformer bookingList state

eval (TicketPressed (MetroTicketBookingStatus ticketApiResp)) state = do 
  exit $ GoToMetroTicketDetailsFlow ticketApiResp.bookingId

eval (PastTicketPressed ticketApiResp) state = exit $ GoToMetroTicketStatusFlow ticketApiResp

eval AfterRender state =
  continue state {
    props{
      showShimmer = false
    }
  }

eval BackPressed state = 
  case state.props.entryPoint of 
    HomeScreenToMetroMyTickets -> exit GoToHomeScreen
    MetroTicketBookingToMetroMyTickets -> exit GoToMetroBooking

eval _ state = continue state