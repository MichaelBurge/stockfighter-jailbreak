{-# LANGUAGE OverloadedStrings,DuplicateRecordFields #-}
module Main where

import Api.Stockfighter.Jailbreak

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
      ("status", Command $ const command_device_status)
      ]),
  ("vm", CommandTree [
      ("compile", Command command_vm_compile)
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

command_level :: Args -> IO ()
command_level _ = do
  result <- unsafeInvokeApi get_level
  T.putStrLn $ "Level" <> T.pack (show $ level result) <> " - " <> name (result :: GetCurrentLevelResponse)

command_vm_compile :: Args -> IO ()
command_vm_compile xs = case xs of
  [] -> error $ "Expected a filename"
  
  [x] -> do
    program <- BSL.readFile $ T.unpack x
    result <- unsafeInvokeApi $ flip post_vm_compile program
    putStrLn $ show result
  _ -> error $ "Too many arguments"  
  

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  let eResult = walkCommandTree args commandTree
  case eResult of
    Left err -> T.putStrLn err
    Right result -> result
