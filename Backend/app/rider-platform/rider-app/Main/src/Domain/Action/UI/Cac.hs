{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Cac where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInReplica)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Error.Throwing
import Kernel.Utils.Logging
import Servant
import SharedLogic.Cac
import qualified Storage.Queries.Person as QPerson
import Tools.Auth
import Tools.Error

getGetUiConfigs :: (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person), Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Int -> Environment.Flow Data.Aeson.Object
getGetUiConfigs (mbPersonId, _) toss =
  case mbPersonId of
    Just personId -> do
      person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      fromMaybe (Data.Aeson.KeyMap.empty) <$> getFrontendConfigs person (Just toss)
    Nothing -> do
      logError "PersonId is null, hence context of city cannot be determined. Returning empty object."
      return Data.Aeson.KeyMap.empty
