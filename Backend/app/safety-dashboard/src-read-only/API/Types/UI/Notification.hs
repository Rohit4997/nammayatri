{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.Notification where

import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Notification
import qualified "lib-dashboard" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import "lib-dashboard" Tools.Auth

data NotificationList = NotificationList
  { list :: [Domain.Types.Notification.Notification],
    summary :: API.Types.UI.Notification.Summary
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data NotificationReadRequest = NotificationReadRequest
  { id :: Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Summary = Summary
  { count :: Kernel.Prelude.Int,
    totalCount :: Kernel.Prelude.Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
