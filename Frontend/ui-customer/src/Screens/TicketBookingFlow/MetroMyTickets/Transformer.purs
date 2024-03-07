{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroMyTickets.Transformer where


import Prelude 
import Screens.Types 
import Services.API
import Data.Array
import Data.Maybe
import Engineering.Helpers.Commons
import Data.Function.Uncurried as DFU
import JBridge as JB

metroTicketListApiToMyTicketsTransformer ::  (Array MetroTicketBookingStatus) -> MetroMyTicketsScreenState -> MetroMyTicketsScreenState 
metroTicketListApiToMyTicketsTransformer ticketList state = 
  let 
    activeTickets' = metroTicketCardTransformer $ filter (\ (MetroTicketBookingStatus bookingStatus) -> activeTicketEvaluator bookingStatus.status bookingStatus.tickets) ticketList
    pastTickets' = reverse $ metroTicketCardTransformer $ filter (\ (MetroTicketBookingStatus bookingStatus) -> pastTicketEvaluator bookingStatus.status bookingStatus.tickets) ticketList
  in
    state {
      data {
        activeTickets =  activeTickets'
      , pastTickets = pastTickets'
      }
    }
  where 
    activeTicketEvaluator :: String -> Array FRFSTicketAPI  -> Boolean
    activeTicketEvaluator status tickets = 
      let validTill = maybe "" (\(FRFSTicketAPI ticket) -> ticket.validTill) (head tickets)
      in (any (_ == status) ["CONFIRMED", "PAYMENT_PENDING"]) && (not $ isTicketExpired validTill)

    pastTicketEvaluator :: String -> Array FRFSTicketAPI -> Boolean
    pastTicketEvaluator status tickets = 
      let validTill =  maybe "" (\(FRFSTicketAPI ticket) -> ticket.validTill) (head tickets)
      in (any (_ == status) ["CANCELLED", "FAILED"]) || (isTicketExpired validTill)


metroTicketCardTransformer :: Array MetroTicketBookingStatus -> Array MetroTicketCardData
metroTicketCardTransformer  ticketList = map ticketItemTransformer ticketList

ticketItemTransformer :: MetroTicketBookingStatus -> MetroTicketCardData
ticketItemTransformer (MetroTicketBookingStatus bookingItem) = 
  let 
    sourceStationEnum = bookingItem.stations !! 0
    destinationStationEnum = bookingItem.stations !! ((length bookingItem.stations)- 1)
    utcValidTill =  maybe "" (\(FRFSTicketAPI ticket) -> ticket.validTill) (head bookingItem.tickets)
    ticketsValidTill = (convertUTCtoISC utcValidTill "hh:mm A") <> ", " <> (convertUTCtoISC utcValidTill "Do MMM YYYY")
    sourceName' = getStationName sourceStationEnum
    destinationName' = getStationName destinationStationEnum
    noOfTickets' = bookingItem.quantity
    createdAt' = (convertUTCtoISC (getCurrentUTC "") "Do MMM YYYY")
    metroTicketStatusApiResp' = (MetroTicketBookingStatus bookingItem)
    status' = bookingItem.status
    validUntill' = (convertUTCtoISC bookingItem.validTill "hh:mm A") <> ", " <> (convertUTCtoISC bookingItem.validTill "Do MMM YYYY") 
  in 
  {
      sourceName : sourceName'
    , destinationName : destinationName'
    , createdAt : createdAt'
    , noOfTickets : noOfTickets'
    , metroTicketStatusApiResp :  metroTicketStatusApiResp'
    , status : status'
    , validUntill : ticketsValidTill
  }

getStationName :: Maybe FRFSStationAPI -> String
getStationName Nothing = ""
getStationName (Just (FRFSStationAPI station)) = station.name 

isTicketExpired :: String -> Boolean
isTicketExpired validTill =  (DFU.runFn2 JB.differenceBetweenTwoUTC  validTill (getCurrentUTC "")) < 0