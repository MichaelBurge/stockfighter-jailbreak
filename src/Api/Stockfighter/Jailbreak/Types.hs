{-# LANGUAGE DeriveGeneric,DuplicateRecordFields,DeriveDataTypeable #-}

module Api.Stockfighter.Jailbreak.Types where

import Data.Aeson
import GHC.Generics

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

type ApiKey = T.Text
newtype HexString = HexString T.Text deriving (Generic, Show)

instance ToJSON HexString where
instance FromJSON HexString where

newtype Base64Bytes = Base64Bytes BS.ByteString deriving (Show)

instance ToJSON Base64Bytes where
  toJSON (Base64Bytes bs) = toJSON (T.decodeUtf8 $ Base64.encode bs :: T.Text)

instance FromJSON Base64Bytes where
  parseJSON json = do
    eResult <- Base64.decode <$> T.encodeUtf8 <$> parseJSON json
    case eResult of
      Left e -> fail e
      Right x -> return $ Base64Bytes x
  
data JailbreakConfig = JailbreakConfig {
  apiKey :: ApiKey
  } deriving (Generic, Show)

data GetDeviceStatusResponse = GetDeviceStatusResponse {
  ok :: Bool,
  address :: T.Text,
  runcount :: Int,
  apu_state :: ApuState
  } deriving (Generic, Show)

instance ToJSON GetDeviceStatusResponse where
instance FromJSON GetDeviceStatusResponse where

data StartDeviceResponse = StartDeviceResponse {
  ok :: Bool,
  error :: T.Text
  } deriving (Generic, Show)

instance ToJSON StartDeviceResponse where
instance FromJSON StartDeviceResponse where

data RestartDeviceResponse = RestartDeviceResponse {
  ok :: Bool,
  error :: T.Text
  } deriving (Generic, Show)

instance ToJSON RestartDeviceResponse where
instance FromJSON RestartDeviceResponse where

data GetStdoutResponse = GetStdoutResponse {
  ok :: Bool,
  runcount :: Int,
  iov :: Iov
  } deriving (Generic, Show)

instance ToJSON GetStdoutResponse where
instance FromJSON GetStdoutResponse where

data Iov = Iov {
  offset :: Int,
  base64bytes :: Base64Bytes
  } deriving (Generic, Show)

instance ToJSON Iov where
instance FromJSON Iov

                         
data StopDeviceResponse = StopDeviceResponse {
  ok :: Bool,
  error :: T.Text
  } deriving (Generic, Show)

instance ToJSON StopDeviceResponse where
instance FromJSON StopDeviceResponse where
                          
data GetCurrentLevelResponse = GetCurrentLevelResponse {
  ok :: Bool,
  level :: Int,
  name :: T.Text
  } deriving (Generic, Show)

instance ToJSON GetCurrentLevelResponse where
instance FromJSON GetCurrentLevelResponse where

data LoadBytecodeResponse = LoadBytecodeResponse {
  ok :: Bool
  } deriving (Generic, Show)

instance ToJSON LoadBytecodeResponse where
instance FromJSON LoadBytecodeResponse where

data ExecResponse = ExecResponse {
  ok :: Bool,
  error :: T.Text
  } deriving (Generic, Show)
instance ToJSON ExecResponse where
instance FromJSON ExecResponse where
  
  
-- | Response from compiling a C program
data CompileResponse = CompileResponse {
  ok :: Bool,
  bss :: Maybe T.Text,
  po :: Maybe Int,
  eov :: Maybe Int,
  raw :: Maybe T.Text,
  ep :: Maybe Int,
  row :: Int,
  text :: T.Text,
  token :: Maybe T.Text,
  functions :: Maybe [Function]
  }
  deriving (Generic, Show)

instance ToJSON CompileResponse where
instance FromJSON CompileResponse where

data WriteBytecodeResponse = WriteBytecodeResponse {
  ok :: Bool,
  error :: Maybe T.Text
  } deriving (Generic, Show)

instance ToJSON WriteBytecodeResponse where
instance FromJSON WriteBytecodeResponse where
                             
data Function = Function {
  offset :: Int,
  name :: T.Text
  } deriving (Generic, Show)

instance ToJSON Function where
instance FromJSON Function where
                
data ApuState = ApuState {
  -- | Instruction pointer
  pc :: Int,
  -- | Stack pointer
  sp :: HexString,
  -- | Flags register
  sr :: Int,
  -- | Flags register(string)
  sr_string :: T.Text,
  cycles :: Int,
  current_insn :: T.Text,
  registers :: [ HexString ]
  } deriving (Generic, Show)

instance ToJSON ApuState where
instance FromJSON ApuState where

data Instruction = Instruction {
  ok :: Bool,
  raw64 :: T.Text,
  mnem :: T.Text,
  code :: Int,
  dest :: Int,
  src :: Int,
  k :: Int,
  s :: Int,
  b :: Int,
  offset :: Int,
  symbol :: Maybe T.Text,
  dump :: T.Text
  } deriving (Generic, Show)

instance ToJSON Instruction where
instance FromJSON Instruction where
