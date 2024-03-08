{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.DriverOffer where

import qualified Database.Beam as B
import qualified Domain.Types.DriverOffer as Domain
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data DriverOfferT f = DriverOfferT
  { id :: B.C f Text,
    estimateId :: B.C f Text,
    merchantId :: B.C f (Maybe Text),
    merchantOperatingCityId :: B.C f (Maybe Text),
    driverName :: B.C f Text,
    durationToPickup :: B.C f (Maybe Int),
    distanceToPickup :: B.C f (Maybe HighPrecMeters),
    validTill :: B.C f UTCTime,
    bppQuoteId :: B.C f Text,
    rating :: B.C f (Maybe Centesimal),
    status :: B.C f Domain.DriverOfferStatus,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverOfferT where
  data PrimaryKey DriverOfferT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverOffer = DriverOfferT Identity

$(enableKVPG ''DriverOfferT ['id] [['bppQuoteId], ['estimateId]])

$(mkTableInstances ''DriverOfferT "driver_offer")
