{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Suspect where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import "lib-dashboard" Tools.Auth

data SuspectBulkUploadReq = SuspectBulkUploadReq
  { suspects :: [API.Types.UI.Suspect.SuspectUploadReq]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SuspectBulkUploadResp = SuspectBulkUploadResp
  { dlList :: [Kernel.Prelude.Maybe Data.Text.Text],
    message :: Data.Text.Text,
    voterIdList :: [Kernel.Prelude.Maybe Data.Text.Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SuspectFlagStatusChangeReq = SuspectFlagStatusChangeReq
  { dl :: Kernel.Prelude.Maybe Data.Text.Text,
    reasonToChange :: Data.Text.Text,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SuspectUploadReq = SuspectUploadReq
  { dl :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    flaggedCategory :: Data.Text.Text,
    flaggedReason :: Data.Text.Text,
    lastName :: Data.Text.Text,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
