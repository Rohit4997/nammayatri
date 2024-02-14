{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FareProduct where

import qualified Database.Beam as B
import Domain.Types.Common
import qualified Domain.Types.FareProduct as Domain
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Tools.Beam.UtilsTH

data FareProductT f = FareProductT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    farePolicyId :: B.C f Text,
    vehicleVariant :: B.C f Variant.Variant,
    area :: B.C f Domain.Area,
    tripCategory :: B.C f TripCategory
  }
  deriving (Generic, B.Beamable)

instance B.Table FareProductT where
  data PrimaryKey FareProductT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FareProduct = FareProductT Identity

$(enableKVPG ''FareProductT ['id] [['merchantOperatingCityId, 'area]])

$(mkTableInstances ''FareProductT "fare_product")
