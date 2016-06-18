{-# LANGUAGE DeriveGeneric #-}

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

data CompileCProgramResponse = CompileCProgramResponse {
  } deriving (Generic, Show)
  
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
