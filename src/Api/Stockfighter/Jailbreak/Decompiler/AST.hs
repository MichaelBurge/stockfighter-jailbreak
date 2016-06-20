{-# LANGUAGE GADTs,StandaloneDeriving,DataKinds,TemplateHaskell #-}

module Api.Stockfighter.Jailbreak.Decompiler.AST where

import Api.Stockfighter.Jailbreak.Types

import qualified Data.IntMap.Strict as M
import qualified Data.Text as T

import Control.Lens.TH
import Control.Monad.Trans.Reader
import Data.Monoid
import Text.PrettyPrint hiding ((<>))

newtype FunctionId = FunctionId Int deriving (Show)
newtype VariableId = VariableId Int deriving (Show)

data Symbol = Symbol {
  _sym_symbol :: T.Text,
  _sym_offset :: Int
  } deriving (Show)

data Table a = Table {
  _elements :: M.IntMap a,
  _nextId :: Int
  } deriving (Show)

instance Monoid (Table a) where
  mempty = Table mempty 0
  mappend (Table a1 b1) (Table a2 b2) = Table (a1 <> a2) (b1 + b2)

data Context = Context {
  _ctx_functions :: Table Symbol,
  _ctx_variables :: Table Statement,
  _ctx_assembly :: [ Instruction ]
  } deriving (Show)

instance Monoid Context where
  mempty = Context mempty mempty mempty
  mappend (Context a1 b1 c1) (Context a2 b2 c2) = Context (a1 `mappend` a2) (b1 `mappend` b2) (c1 `mappend` c2)

class PrintAst a where
  printNode :: a -> Reader Context Doc

data Unop = Negate
          deriving (Show)

data Binop = Plus
           | Subtract
           | Multiply
           | Divide
           | Mod
             deriving (Show)

data Expression where
  ELiteral :: Int -> Expression
  EUnop :: Unop -> Expression -> Expression
  EBinop :: Binop -> Expression -> Expression -> Expression
  ECall :: Symbol -> [ Expression ] -> Expression

deriving instance Show Expression 

data Type where
  TVoid    :: Type
  TInt     :: Type
  TIntPtr  :: Type
  TChar    :: Type
  TCharPtr :: Type

deriving instance Show Type

data Statement where
  SAssign :: VariableId -> Expression -> Statement
  SGoto :: VariableId -> Statement
  SAsm :: [ Instruction ] -> Statement
  SVariable :: VariableId -> Type -> Maybe Expression -> Statement
  SFunction :: Type -> Symbol -> [ Statement ] -> [ Statement ] -> Statement

deriving instance Show Statement

makeLenses ''Symbol
makeLenses ''Table
makeLenses ''Context
makePrisms ''Unop
makeLenses ''Statement
makePrisms ''Binop
makeLenses ''Type
makeLenses ''Expression
