{-# LANGUAGE OverloadedStrings #-}

module Api.Stockfighter.Jailbreak.Config where

import Data.Configurator
import Data.IORef
import System.IO.Unsafe

import Api.Stockfighter.Jailbreak.Types

import qualified Data.Text as T

jailbreakConfig :: IORef JailbreakConfig
jailbreakConfig = unsafePerformIO $ newIORef =<< getJailbreakConfig

getJailbreakConfig :: IO JailbreakConfig
getJailbreakConfig = do
  (config, threadId) <- autoReload autoConfig [
    Required "config.txt"
    ]
  apiKey <- require config "stockfighter.api-key"
  return $ JailbreakConfig apiKey
