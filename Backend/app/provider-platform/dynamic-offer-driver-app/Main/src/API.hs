{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API where

import qualified API.Beckn as Beckn
import qualified API.Cac as CacApi
import qualified API.Dashboard as Dashboard
import qualified API.Internal as Internal
import qualified API.UI as UI
import qualified Data.ByteString as BS
import Data.OpenApi
import qualified Domain.Action.UI.DriverOnboarding.IdfyWebhook as DriverOnboarding
import qualified Domain.Action.UI.Payment as Payment
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Plan as Plan
import Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Juspay.Webhook as Juspay
import qualified Kernel.External.Verification.Interface.Idfy as Idfy
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.BasicAuth ()
import Kernel.Utils.Servant.HTML
import Servant hiding (serveDirectoryWebApp)
import Servant.OpenApi
import Storage.Beam.SystemConfigs ()

type DriverOfferAPI =
  MainAPI
    :<|> Beckn.API -- TODO : Revert after 2.x release
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  UI.API
    -- :<|> Beckn.API -- TODO : Revert after 2.x release
    :<|> Idfy.IdfyWebhookAPI
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> Idfy.IdfyWebhookAPI
         )
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> Capture "city" Context.City
             :> Idfy.IdfyWebhookAPI
         )
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> Juspay.JuspayWebhookAPI
         )
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> QueryParam "city" Context.City
             :> QueryParam "serviceName" Plan.ServiceNames
             :> "v2"
             :> Juspay.JuspayWebhookAPI
         )
    :<|> Dashboard.API -- TODO :: Needs to be deprecated
    :<|> Dashboard.APIV2
    :<|> Internal.API
    :<|> CacAPI

type CacAPI = "cac" :> CacApi.API

driverOfferAPI :: Proxy DriverOfferAPI
driverOfferAPI = Proxy

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    -- :<|> Beckn.handler -- TODO : Revert after 2.x release
    :<|> oldIdfyWebhookHandler
    :<|> idfyWebhookHandler
    :<|> idfyWebhookV2Handler
    :<|> juspayWebhookHandler
    :<|> juspayWebhookHandlerV2
    :<|> Dashboard.handler
    :<|> Dashboard.handlerV2
    :<|> Internal.handler
    :<|> CacApi.handler

driverOfferServer :: FlowServer DriverOfferAPI
driverOfferServer =
  mainServer
    :<|> Beckn.handler -- TODO : Revert after 2.x release
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Namma Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI

idfyWebhookV2Handler ::
  ShortId DM.Merchant ->
  Context.City ->
  Maybe Text ->
  Value ->
  FlowHandler AckResponse
idfyWebhookV2Handler merchantShortId opCity secret =
  withFlowHandlerAPI . DriverOnboarding.idfyWebhookV2Handler merchantShortId opCity secret

idfyWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Text ->
  Value ->
  FlowHandler AckResponse
idfyWebhookHandler merchantShortId secret =
  withFlowHandlerAPI . DriverOnboarding.idfyWebhookHandler merchantShortId secret

oldIdfyWebhookHandler ::
  Maybe Text ->
  Value ->
  FlowHandler AckResponse
oldIdfyWebhookHandler secret =
  withFlowHandlerAPI . DriverOnboarding.oldIdfyWebhookHandler secret

juspayWebhookHandler ::
  ShortId DM.Merchant ->
  BasicAuthData ->
  Value ->
  FlowHandler AckResponse
juspayWebhookHandler merchantShortId secret value' =
  withFlowHandlerAPI $ Payment.juspayWebhookHandler merchantShortId Nothing Nothing secret value'

juspayWebhookHandlerV2 ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe Plan.ServiceNames ->
  BasicAuthData ->
  Value ->
  FlowHandler AckResponse
juspayWebhookHandlerV2 merchantShortId mbOpCity mbServiceName secret value' =
  withFlowHandlerAPI $ Payment.juspayWebhookHandler merchantShortId mbOpCity mbServiceName secret value'
