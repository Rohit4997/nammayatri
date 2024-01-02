{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Webhook where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import Domain.Types.MerchantConfigs as MC
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (elem, filter, id, length, map, mapM_, whenJust)
import Kernel.Prelude
import Kernel.Utils.Common
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager, tlsManagerSettings)
import Network.HTTP.Types (HeaderName)

sendWebHook :: [MC.MerchantConfigs] -> LBS.ByteString -> Environment.Flow ()
sendWebHook merchants webhookBody = do
  mapM_
    ( \receiver -> do
        let webHookHeaders = convertToHeaders receiver.webHookHeaders
        let request = parseRequest_ (T.unpack receiver.webHookUrl)
            request' =
              request
                { method = "POST",
                  requestBody = RequestBodyLBS $ webhookBody,
                  requestHeaders = webHookHeaders
                }
        manager <- liftIO getGlobalManager
        logDebug $ "Sending webhook to " <> show request'
        out <- liftIO $ httpLbs request' manager
        logDebug $ "Webhook response: " <> (show out)
    )
    merchants
  where
    convertToHeaders :: [MC.WebHookHeaders] -> [(HeaderName, ByteString)]
    convertToHeaders webHookHeaders =
      map (\header -> (CI.mk $ encodeUtf8 header.key, encodeUtf8 header.value)) webHookHeaders
