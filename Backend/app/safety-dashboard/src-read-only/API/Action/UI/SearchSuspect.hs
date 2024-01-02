{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SearchSuspect where

import qualified API.Types.UI.SearchSuspect
import qualified Control.Lens
import qualified Domain.Action.UI.SearchSuspect as Domain.Action.UI.SearchSuspect
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth
import Tools.Auth.Webhook

type API =
  DashboardAuth 'DASHBOARD_USER :> "search" :> "suspectList" :> ReqBody '[JSON] API.Types.UI.SearchSuspect.SearchSuspectReqList :> Post '[JSON] API.Types.UI.SearchSuspect.SuspectsList
    :<|> DashboardAuth 'DASHBOARD_ADMIN :> "check" :> "suspectStatusHistory" :> QueryParam "limit" (Kernel.Prelude.Int) :> QueryParam "offset" (Kernel.Prelude.Int) :> ReqBody '[JSON] API.Types.UI.SearchSuspect.SearchSuspectReq :> Post '[JSON] API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp
    :<|> DashboardAuth 'MERCHANT_ADMIN :> "merchant" :> "check" :> "suspectStatusHistory" :> ReqBody '[JSON] API.Types.UI.SearchSuspect.SearchSuspectReq :> Post '[JSON] API.Types.UI.SearchSuspect.FlagHistoryResp
    :<|> SafetyWebhookAuth 'MERCHANT_SERVER :> "partner" :> "search" :> "agent" :> ReqBody '[JSON] API.Types.UI.SearchSuspect.SearchSuspectReqList :> Post '[JSON] API.Types.UI.SearchSuspect.SuspectsList
    :<|> DashboardAuth 'DASHBOARD_ADMIN :> "suspect" :> "list" :> QueryParam "from" (Kernel.Prelude.UTCTime) :> QueryParam "limit" (Kernel.Prelude.Int) :> QueryParam "offset" (Kernel.Prelude.Int) :> QueryParam "to" (Kernel.Prelude.UTCTime) :> Get '[JSON] API.Types.UI.SearchSuspect.SuspectsList
    :<|> DashboardAuth 'MERCHANT_ADMIN :> "partner" :> "suspect" :> "list" :> QueryParam "from" (Kernel.Prelude.UTCTime) :> QueryParam "limit" (Kernel.Prelude.Int) :> QueryParam "offset" (Kernel.Prelude.Int) :> QueryParam "to" (Kernel.Prelude.UTCTime) :> Get '[JSON] API.Types.UI.SearchSuspect.SuspectsList

handler :: Environment.FlowServer API
handler =
  postSearchSuspectList
    :<|> postCheckSuspectStatusHistory
    :<|> postMerchantCheckSuspectStatusHistory
    :<|> postPartnerSearchAgent
    :<|> getSuspectList
    :<|> getPartnerSuspectList

postSearchSuspectList :: TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList
postSearchSuspectList a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postSearchSuspectList a2 a1

postCheckSuspectStatusHistory :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.FlowHandler API.Types.UI.SearchSuspect.CheckSuspectStatusHistoryResp
postCheckSuspectStatusHistory a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postCheckSuspectStatusHistory a4 a3 a2 a1

postMerchantCheckSuspectStatusHistory :: TokenInfo -> API.Types.UI.SearchSuspect.SearchSuspectReq -> Environment.FlowHandler API.Types.UI.SearchSuspect.FlagHistoryResp
postMerchantCheckSuspectStatusHistory a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postMerchantCheckSuspectStatusHistory a2 a1

postPartnerSearchAgent :: AuthToken -> API.Types.UI.SearchSuspect.SearchSuspectReqList -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList
postPartnerSearchAgent a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.postPartnerSearchAgent a2 a1

getSuspectList :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList
getSuspectList a5 a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.getSuspectList a5 a4 a3 a2 a1

getPartnerSuspectList :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.UI.SearchSuspect.SuspectsList
getPartnerSuspectList a5 a4 a3 a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.SearchSuspect.getPartnerSuspectList a5 a4 a3 a2 a1
