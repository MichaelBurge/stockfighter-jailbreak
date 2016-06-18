{-# LANGUAGE DeriveGeneric,DuplicateRecordFields #-}

module Api.Stockfighter.Jailbreak.Types where

import Data.Aeson
import GHC.Generics

import qualified Data.Text as T

type ApiKey = T.Text
newtype HexString = HexString T.Text deriving (Generic, Show)

instance ToJSON HexString where
instance FromJSON HexString where

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
  ok :: Bool
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
