{-# LANGUAGE DataKinds,TypeOperators,OverloadedStrings,DuplicateRecordFields,RecordWildCards #-}

module Api.Stockfighter.Jailbreak.Api where

import Api.Stockfighter.Jailbreak.Config
import Api.Stockfighter.Jailbreak.Types

import Control.Exception (bracket)
import Control.Monad.Trans.Except
import Data.IORef
import Data.Proxy
import Network.HTTP.Client (newManager, closeManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

type JailbreakApi =
  Header "X-Starfighter-Authorization" ApiKey :> (
       "device/status" :> Get '[JSON] GetDeviceStatusResponse
  :<|> "vm/compile" :> ReqBody '[OctetStream] BSL.ByteString :> Post '[JSON] CompileResponse
  :<|> "level" :> Get '[JSON] GetCurrentLevelResponse
  )

type Response a = Manager -> BaseUrl -> ClientM a

data ApiClient = ApiClient {
  get_device_status :: Response GetDeviceStatusResponse,
  post_vm_compile :: BSL.ByteString -> Response CompileResponse,
  get_level :: Response GetCurrentLevelResponse
  }

mkApiClient :: ApiKey -> ApiClient
mkApiClient apiKey = ApiClient{..}
  where
    (get_device_status :<|> post_vm_compile :<|> get_level) = client jailbreakApi $ Just apiKey

jailbreakApi :: Proxy JailbreakApi
jailbreakApi = Proxy

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "www.stockfighter.io" 80 "/trainer"

invokeApi :: (ApiClient -> Response a) -> ClientM a
invokeApi action =
  ExceptT $ bracket (newManager tlsManagerSettings) closeManager $ \manager -> do
    config <- readIORef jailbreakConfig
    let apiClient = mkApiClient $ apiKey config
    runExceptT $ action apiClient manager baseUrl
  
unsafeInvokeApi :: (ApiClient -> Response a) -> IO a
unsafeInvokeApi action = do
  eResult <- runExceptT $ invokeApi action
  case eResult of
    Left err -> error $ show err
    Right result -> return result
