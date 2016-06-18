{-# LANGUAGE OverloadedStrings,DuplicateRecordFields #-}
module Main where

import Prelude as P

import Api.Stockfighter.Jailbreak
import qualified Api.Stockfighter.Jailbreak as JB

import Data.List (lookup)
import Data.Monoid ((<>))
import System.Environment

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Args = [ T.Text ]

data CommandTree = CommandTree [ (T.Text, CommandTree) ] | Command (Args -> IO ())

commandTree :: CommandTree
commandTree = CommandTree [
  ("device", CommandTree [
      ("status", Command $ const command_device_status),
      ("start", Command command_device_start),
      ("restart", Command command_device_restart),
      ("stop", Command command_device_stop)
      ]),
  ("vm", CommandTree [
      ("compile", Command command_vm_compile),
      ("write", Command command_vm_write)
      ]),
  ("level", Command command_level)
  ]

walkCommandTree :: Args -> CommandTree -> Either T.Text (IO ())
walkCommandTree args tree = case tree of
  Command f -> Right $ f args
  CommandTree xs ->
    let choicesError = "Your choices are:\n" <> T.unlines (map fst xs)
    in case args of
      [] -> Left $ "Expected an additional argument. " <> choicesError
      (a:as) -> do
        let err = "Unknown command '" <> a <> "'. " <> choicesError
        child <- maybe (Left err) Right $ lookup a xs
        walkCommandTree as child

command_device_status = do
  result <- unsafeInvokeApi get_device_status
  putStrLn $ show result

command_device_start _ = do
  result <- unsafeInvokeApi post_device_start
  putStrLn $ show result
  
command_device_restart _ = do
  result <- unsafeInvokeApi post_device_restart
  putStrLn $ show result
  
command_device_stop _ = do
  result <- unsafeInvokeApi post_device_stop
  putStrLn $ show result

command_level :: Args -> IO ()
command_level _ = do
  result <- unsafeInvokeApi get_level
  T.putStrLn $ "Level" <> T.pack (show $ level result) <> " - " <> name (result :: JB.GetCurrentLevelResponse)

command_vm_compile :: Args -> IO ()
command_vm_compile xs = case xs of
  [] -> P.error $ "Expected a filename"
  
  [x] -> do
    program <- BSL.readFile $ T.unpack x
    result <- unsafeInvokeApi $ flip post_vm_compile program
    putStrLn $ show result
  _ -> P.error $ "Too many arguments"  
  
command_vm_write :: Args -> IO ()
command_vm_write _ = do
  result <- unsafeInvokeApi $ get_vm_write
  putStrLn $ show result

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  let eResult = walkCommandTree args commandTree
  case eResult of
    Left err -> T.putStrLn err
    Right result -> result