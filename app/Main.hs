{-# LANGUAGE OverloadedStrings,DuplicateRecordFields,RecordWildCards #-}
module Main where

import Prelude as P

import Api.Stockfighter.Jailbreak
import Api.Stockfighter.Jailbreak.Decompiler
import qualified Api.Stockfighter.Jailbreak as JB

import Control.Monad
import Control.Monad.Trans.Reader
import Data.Aeson (decode,encode,eitherDecode)
import Data.List (lookup)
import Data.Monoid ((<>))
import System.Environment
import Text.PrettyPrint (render)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T

type Args = [ T.Text ]

data CommandTree = CommandTree [ (T.Text, CommandTree) ] | Command (Args -> IO ())

showt :: Show a => a -> T.Text
showt = T.pack . show

commandTree :: CommandTree
commandTree = CommandTree [
  ("device", CommandTree [
      ("status", Command $ const command_device_status),
      ("program", CommandTree [
          ("fetch", Command command_device_program_fetch),
          ("disassemble", Command command_device_program_disassemble),
          ("decompile", Command command_device_program_decompile)
          ]),
      ("start", Command command_device_start),
      ("restart", Command command_device_restart),
      ("stdout", Command command_device_stdout),
      ("stop", Command command_device_stop)
      ]),
  ("vm", CommandTree [
      ("compile", Command command_vm_compile),
      ("exec", Command command_vm_exec),
      ("load", Command command_vm_load),
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

command_device_program_fetch args = case args of
  [] -> act 0
  [x] -> act $ int x
  where
    act core = do
      result <- unsafeInvokeApi $ flip get_device_program core
      BSL.putStr result

withAssemblyFile :: Args -> ([Instruction] -> IO ()) -> IO ()
withAssemblyFile args action = case args of
  [] -> P.error "Expected exactly 1 argument(filename); found 0"
  [x] -> do
    contents <- BSL.readFile $ T.unpack x
    let eInstructions = eitherDecode contents
    case eInstructions of
      Left err -> putStrLn $ "Error decoding: " ++ err
      Right instructions -> do
        action instructions

command_device_program_disassemble args = withAssemblyFile args $ \instructions -> do
  putStrLn $ "Number of Instructions: " ++ show (length (instructions :: [Instruction]))
  forM_ instructions $ putStrLn . render . flip runReader mempty . printNode

command_device_program_decompile args = withAssemblyFile args $ \instructions -> do
  (statements, context) <- decompile instructions
  print_ast statements
  
command_device_start _ = do
  result <- unsafeInvokeApi post_device_start
  putStrLn $ show result
  
command_device_restart _ = do
  result <- unsafeInvokeApi post_device_restart
  putStrLn $ show result

command_device_stdout xs = case xs of
  [] -> act 0 0
  [x] -> act (int x) 0
  [x,y] -> act (int x) (int y)
  where
    act cores offset = do
      result <- unsafeInvokeApi $ \apiKey -> get_device_stdout apiKey cores offset
      let (Base64Bytes bs) = base64bytes $ iov result
      BS.putStrLn bs
  
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
    BSL.putStrLn result
  _ -> P.error $ "Too many arguments"  

command_vm_exec :: Args -> IO ()
command_vm_exec _ = do
  result <- unsafeInvokeApi post_vm_exec
  putStrLn $ show result

command_vm_load :: Args -> IO ()
command_vm_load _ = do
  result <- unsafeInvokeApi post_vm_load
  putStrLn $ show result
  
command_vm_write :: Args -> IO ()
command_vm_write [x] = do
  bytecode <- BSL.readFile $ T.unpack x
  result <- unsafeInvokeApi $ flip get_vm_write bytecode
  putStrLn $ show result

int x = read $ T.unpack x :: Int

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  let eResult = walkCommandTree args commandTree
  case eResult of
    Left err -> T.putStrLn err
    Right result -> result
