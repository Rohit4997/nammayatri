{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.AutoCompleteData where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data AutoCompleteData = AutoCompleteData
  { autocompleteInputs :: Kernel.Prelude.Text,
    customerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Prelude.Text,
    isLocationSelectedOnMap :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    searchRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest),
    searchType :: Kernel.Prelude.Text,
    sessionToken :: Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
