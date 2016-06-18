module Main where

import Api.Stockfighter.Jailbreak

main = do
  result <- unsafeInvokeApi getDeviceStatus
  putStrLn $ show result
  
