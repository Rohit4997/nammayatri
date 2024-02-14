{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.DriverOnboarding.AadhaarOtpVerify where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data AadhaarOtpVerifyT f = AadhaarOtpVerifyT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    requestId :: B.C f Text,
    statusCode :: B.C f Text,
    transactionId :: B.C f Text,
    requestMessage :: B.C f Text,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarOtpVerifyT where
  data PrimaryKey AadhaarOtpVerifyT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type AadhaarOtpVerify = AadhaarOtpVerifyT Identity

$(enableKVPG ''AadhaarOtpVerifyT ['id] [['driverId]])

$(mkTableInstances ''AadhaarOtpVerifyT "aadhaar_otp_verify")
