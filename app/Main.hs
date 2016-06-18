{-# LANGUAGE OverloadedStrings #-}
module Main where

import Api.Stockfighter.Jailbreak

import Data.List (lookup)
import Data.Monoid ((<>))
import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Args = [ T.Text ]

data CommandTree = CommandTree [ (T.Text, CommandTree) ] | Command (Args -> IO ())

commandTree :: CommandTree
commandTree = CommandTree [
  ("get-status", Command $ const getStatus)
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
        walkCommandTree args child

getStatus = do
  result <- unsafeInvokeApi getDeviceStatus
  putStrLn $ show result

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  let eResult = walkCommandTree args commandTree
  case eResult of
    Left err -> T.putStrLn err
    Right result -> result
