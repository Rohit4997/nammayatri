{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SearchSuspect where

import qualified API.Types.UI.Notification
import Data.OpenApi (ToSchema)
import qualified Data.Text
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person
import qualified Domain.Types.Suspect
import qualified Domain.Types.SuspectFlagRequest
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Servant
import "lib-dashboard" Tools.Auth

data CheckSuspectStatusHistoryResp = CheckSuspectStatusHistoryResp
  { suspectStatusHistory :: [API.Types.UI.SearchSuspect.StatusHistory]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FlagHistoryResp = FlagHistoryResp
  { flagRequestHistory :: [API.Types.UI.SearchSuspect.FlagRequestHistory]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data FlagRequestHistory = FlagRequestHistory
  { adminApproval :: Kernel.Prelude.Maybe Domain.Types.SuspectFlagRequest.AdminApproval,
    approvedBy :: Kernel.Prelude.Maybe Data.Text.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Kernel.Prelude.Maybe Data.Text.Text,
    flaggedBy :: Kernel.Prelude.Maybe Data.Text.Text,
    flaggedCategory :: Kernel.Prelude.Maybe Data.Text.Text,
    flaggedReason :: Kernel.Prelude.Maybe Data.Text.Text,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    lastName :: Kernel.Prelude.Maybe Data.Text.Text,
    merchantShortId :: Kernel.Prelude.Maybe Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SearchSuspectReq = SearchSuspectReq
  { dl :: Kernel.Prelude.Maybe Data.Text.Text,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SearchSuspectReqList = SearchSuspectReqList
  { suspectReqList :: [API.Types.UI.SearchSuspect.SearchSuspectReq]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SearchSuspectResp = SearchSuspectResp
  { createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Data.Text.Text,
    flagUpdatedAt :: Kernel.Prelude.UTCTime,
    flaggedBy :: Kernel.Prelude.Maybe [Domain.Types.Suspect.FlaggedBy],
    flaggedCounter :: Kernel.Prelude.Int,
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    id :: Data.Text.Text,
    lastName :: Data.Text.Text,
    statusChangedReason :: Kernel.Prelude.Maybe Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data StatusHistory = StatusHistory
  { adminApproval :: Kernel.Prelude.Maybe Domain.Types.SuspectFlagRequest.AdminApproval,
    createdAt :: Kernel.Prelude.UTCTime,
    dl :: Kernel.Prelude.Maybe Data.Text.Text,
    firstName :: Kernel.Prelude.Maybe Data.Text.Text,
    flaggedBy :: Kernel.Prelude.Maybe [Domain.Types.Suspect.FlaggedBy],
    flaggedStatus :: Domain.Types.Suspect.FlaggedStatus,
    id :: Data.Text.Text,
    lastName :: Kernel.Prelude.Maybe Data.Text.Text,
    merchantShortId :: Kernel.Prelude.Maybe Data.Text.Text,
    statusChangedReason :: Kernel.Prelude.Maybe Data.Text.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    voterId :: Kernel.Prelude.Maybe Data.Text.Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SuspectsList = SuspectsList
  { summary :: API.Types.UI.Notification.Summary,
    suspects :: [API.Types.UI.SearchSuspect.SearchSuspectResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
