{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SuspectFlagRequest where

import qualified API.Types.UI.SuspectFlagRequest
import qualified Control.Lens
import qualified Domain.Action.UI.SuspectFlagRequest as Domain.Action.UI.SuspectFlagRequest
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person
import qualified Domain.Types.SuspectFlagRequest
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth

type API =
  DashboardAuth 'MERCHANT_MAKER :> "list" :> "suspectsFlag" :> QueryParam "limit" (Kernel.Prelude.Int) :> QueryParam "offset" (Kernel.Prelude.Int) :> MandatoryQueryParam "approvalStatus" (Domain.Types.SuspectFlagRequest.AdminApproval) :> Get '[JSON] API.Types.UI.SuspectFlagRequest.SuspectFlagRequestList
    :<|> DashboardAuth 'MERCHANT_ADMIN :> "process" :> "suspectFlagRequest" :> ReqBody '[JSON] API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList :> Post '[JSON] Kernel.Types.APISuccess.APISuccess

handler :: Environment.FlowServer API
handler =
  getListSuspectsFlag
    :<|> postProcessSuspectFlagRequest

getListSuspectsFlag :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Domain.Types.SuspectFlagRequest.AdminApproval -> Environment.FlowHandler API.Types.UI.SuspectFlagRequest.SuspectFlagRequestList
getListSuspectsFlag a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SuspectFlagRequest.getListSuspectsFlag a4 a3 a2 a1

postProcessSuspectFlagRequest :: TokenInfo -> API.Types.UI.SuspectFlagRequest.SuspectApprovalReqList -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
postProcessSuspectFlagRequest a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SuspectFlagRequest.postProcessSuspectFlagRequest a2 a1
