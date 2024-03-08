{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.MerchantServiceUsageConfig where

import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.External.AadhaarVerification.Types (AadhaarVerificationService)
import Kernel.External.Call (CallService)
import Kernel.External.Maps.Types (MapsService)
import Kernel.External.Notification.Types (NotificationService)
import Kernel.External.SMS.Types
import Kernel.External.Ticket.Types (IssueTicketService)
import Kernel.External.Verification.Types (DriverVerificationService, VerificationService)
import Kernel.External.Whatsapp.Types
import Kernel.Prelude
import Kernel.Types.Id

data MerchantServiceUsageConfigD (s :: UsageSafety) = MerchantServiceUsageConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    initiateCall :: CallService,
    getDistances :: MapsService,
    getEstimatedPickupDistances :: MapsService,
    getRoutes :: MapsService,
    getPickupRoutes :: MapsService,
    getTripRoutes :: MapsService,
    snapToRoad :: MapsService,
    getPlaceName :: MapsService,
    getPlaceDetails :: MapsService,
    autoComplete :: MapsService,
    getDistancesForCancelRide :: MapsService,
    smsProvidersPriorityList :: [SmsService],
    snapToRoadProvidersList :: [MapsService],
    whatsappProvidersPriorityList :: [WhatsappService],
    verificationService :: VerificationService,
    driverVerificationService :: DriverVerificationService,
    aadhaarVerificationService :: AadhaarVerificationService,
    faceVerificationService :: VerificationService,
    issueTicketService :: IssueTicketService,
    getExophone :: CallService,
    sendSearchRequestToDriver :: [NotificationService],
    updatedAt :: UTCTime,
    createdAt :: UTCTime,
    verificationProvidersPriorityList :: [VerificationService]
  }
  deriving (Generic, Show)

type MerchantServiceUsageConfig = MerchantServiceUsageConfigD 'Safe

instance FromJSON (MerchantServiceUsageConfigD 'Unsafe)

instance ToJSON (MerchantServiceUsageConfigD 'Unsafe)
