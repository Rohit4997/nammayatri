{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BapMetadata where

import qualified Data.Text
import qualified Database.Beam as B
import qualified Domain.Types.BapMetadata
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import Tools.Beam.UtilsTH

data BapMetadataT f = BapMetadataT
  { id :: B.C f Data.Text.Text,
    logoUrl :: B.C f Data.Text.Text,
    name :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BapMetadataT where
  data PrimaryKey BapMetadataT f = BapMetadataId (B.C f Data.Text.Text)
    deriving (Generic, B.Beamable)
  primaryKey = BapMetadataId . id

type BapMetadata = BapMetadataT Identity

$(enableKVPG ''BapMetadataT ['id] [])

$(mkTableInstances ''BapMetadataT "bap_metadata")
