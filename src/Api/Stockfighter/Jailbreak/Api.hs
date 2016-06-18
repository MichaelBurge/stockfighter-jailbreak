{-# LANGUAGE DataKinds,TypeOperators,OverloadedStrings,DuplicateRecordFields,RecordWildCards,DeriveDataTypeable,MultiParamTypeClasses #-}

module Api.Stockfighter.Jailbreak.Api where

import Api.Stockfighter.Jailbreak.Config
import Api.Stockfighter.Jailbreak.Types

import Control.Exception (bracket)
import Control.Monad.Trans.Except
import Data.IORef
import Data.Proxy
import Data.Typeable
import Network.HTTP.Client (newManager, closeManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Servant.API.ContentTypes

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

data OctetStreamJSON deriving (Typeable)

instance Accept OctetStreamJSON where
  contentType _ = "application/json"
  
instance MimeRender OctetStreamJSON BSL.ByteString where
  mimeRender _ = id
  
instance MimeUnrender OctetStreamJSON BSL.ByteString where
  mimeUnrender _ = Right . id

type JailbreakApi =
  Header "X-Starfighter-Authorization" ApiKey :> (
       "device/status" :> Get '[JSON] GetDeviceStatusResponse
  :<|> "device/start" :> Post '[JSON] StartDeviceResponse
  :<|> "device/restart" :> Post '[JSON] RestartDeviceResponse
  :<|> "device/stdout" :> Capture "core" Int :> Capture "offset" Int :> Get '[JSON] GetStdoutResponse
  :<|> "device/stop"  :> Post '[JSON] StopDeviceResponse
  :<|> "vm/compile" :> ReqBody '[OctetStream] BSL.ByteString :> Post '[OctetStreamJSON] BSL.ByteString
  :<|> "vm/exec" :> Post '[JSON] ExecResponse
  :<|> "vm/load" :> Post '[JSON] LoadBytecodeResponse
  :<|> "vm/write" :> ReqBody '[OctetStream] BSL.ByteString :> Post '[JSON] WriteBytecodeResponse
  :<|> "level" :> Get '[JSON] GetCurrentLevelResponse
  )

type Response a = Manager -> BaseUrl -> ClientM a

data ApiClient = ApiClient {
  get_device_status :: Response GetDeviceStatusResponse,
  post_device_start :: Response StartDeviceResponse,
  post_device_restart :: Response RestartDeviceResponse,
  get_device_stdout :: Int -> Int -> Response GetStdoutResponse,
  post_device_stop :: Response StopDeviceResponse,
  post_vm_compile :: BSL.ByteString -> Response BSL.ByteString,
  post_vm_exec :: Response ExecResponse,
  post_vm_load :: Response LoadBytecodeResponse,
  get_vm_write :: BSL.ByteString -> Response WriteBytecodeResponse,
  get_level :: Response GetCurrentLevelResponse
  }

mkApiClient :: ApiKey -> ApiClient
mkApiClient apiKey = ApiClient{..}
  where
    (get_device_status :<|>
     post_device_start :<|>
     post_device_restart :<|>
     get_device_stdout :<|>
     post_device_stop :<|>
     post_vm_compile :<|>
     post_vm_exec :<|>
     post_vm_load :<|>
     get_vm_write :<|>
     get_level) = client jailbreakApi $ Just apiKey

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
    Left err -> Prelude.error $ show err
    Right result -> return result
