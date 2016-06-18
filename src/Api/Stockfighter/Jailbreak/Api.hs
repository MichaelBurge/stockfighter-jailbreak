{-# LANGUAGE DataKinds,TypeOperators,OverloadedStrings #-}

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

import qualified Data.Text as T

type JailbreakApi =
  Header "X-Starfighter-Authorization" ApiKey :> (
  "device/status" :> Get '[JSON] GetDeviceStatusResponse
  )

jailbreakApi :: Proxy JailbreakApi
jailbreakApi = Proxy

getDeviceStatus :: Maybe T.Text->Manager->BaseUrl->ClientM GetDeviceStatusResponse
getDeviceStatus = client jailbreakApi

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "www.stockfighter.io" 80 "/trainer"

invokeApi :: (Maybe T.Text -> Manager -> BaseUrl -> ClientM a) -> ClientM a
invokeApi action =
  ExceptT $ bracket (newManager tlsManagerSettings) closeManager $ \manager -> do
    config <- readIORef jailbreakConfig
    let key = apiKey config
    runExceptT $ action (Just key) manager baseUrl
  
unsafeInvokeApi :: (Maybe T.Text -> Manager -> BaseUrl -> ClientM a) -> IO a
unsafeInvokeApi action = do
  eResult <- runExceptT $ invokeApi action
  case eResult of
    Left err -> error $ show err
    Right result -> return result
