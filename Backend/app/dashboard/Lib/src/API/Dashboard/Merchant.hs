{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Dashboard.Merchant where

import qualified Domain.Action.Dashboard.Merchant as DMerchant
import qualified Domain.Action.Dashboard.Person as DPerson
import Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Servant
import Storage.Beam.BeamFlow
import Tools.Auth

type API =
  "admin"
    :> "merchant"
    :> ( "create"
           :> ( "withAdmin"
                  :> DashboardAuth 'DASHBOARD_ADMIN
                  :> ReqBody '[JSON] DMerchant.CreateMerchantWithAdminReq
                  :> Post '[JSON] DP.PersonAPIEntity
                  :<|> DashboardAuth 'DASHBOARD_ADMIN
                    :> ReqBody '[JSON] DMerchant.CreateMerchantReq
                    :> Post '[JSON] DMerchant.MerchantAPIEntity
              )
           :<|> DashboardAuth 'DASHBOARD_ADMIN
             :> "list"
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> Get '[JSON] DMerchant.ListMerchantResp
           :<|> DashboardAuth 'MERCHANT_ADMIN
             :> "create"
             :> "user"
             :> ReqBody '[JSON] DPerson.CreatePersonReq
             :> Post '[JSON] DPerson.CreatePersonRes
       )

handler :: BeamFlow' => FlowServer API
handler =
  ( createMerchantWithAdmin
      :<|> createMerchant
  )
    :<|> listMerchants
    :<|> createUserForMerchant

createMerchantWithAdmin :: BeamFlow' => TokenInfo -> DMerchant.CreateMerchantWithAdminReq -> FlowHandler DP.PersonAPIEntity
createMerchantWithAdmin tokenInfo =
  withFlowHandlerAPI' . DMerchant.createMerchantWithAdmin tokenInfo

createMerchant :: BeamFlow' => TokenInfo -> DMerchant.CreateMerchantReq -> FlowHandler DMerchant.MerchantAPIEntity
createMerchant tokenInfo =
  withFlowHandlerAPI' . DMerchant.createMerchant tokenInfo

listMerchants :: BeamFlow' => TokenInfo -> Maybe Int -> Maybe Int -> FlowHandler DMerchant.ListMerchantResp
listMerchants tokenInfo mbLimit mbOffset =
  withFlowHandlerAPI' $ DMerchant.listMerchants tokenInfo mbLimit mbOffset

createUserForMerchant :: BeamFlow' => TokenInfo -> DPerson.CreatePersonReq -> FlowHandler DPerson.CreatePersonRes
createUserForMerchant tokenInfo req =
  withFlowHandlerAPI' $ DMerchant.createUserForMerchant tokenInfo req
