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
newtype Register = RRegister Int
                 deriving (Show)

regJunk = RRegister 0
regZero = RRegister 1

data Reg16 = RX
           | RY
           | RZ
           deriving (Show)

regPairs :: Reg16 -> (Register, Register)
regPairs RX = (RRegister 26, RRegister 27)
regPairs RY = (RRegister 28, RRegister 29)
regPairs RZ = (RRegister 30, RRegister 31)

data RegOp = RegOpUnchanged
           | RegOpInc
           | RegOpDec
           | RegOpPlus Int
           | RegOpMinus Int
           deriving (Show)
                    
data Reg16Ex = Reg16Ex {
  reg16ex_op    :: RegOp,
  reg16ex_reg16 :: Reg16
  } deriving (Show)

newtype AbsPtr = AbsPtr Int deriving (Show)
newtype RelPtr = RelPtr Int deriving (Show)
newtype Imm32 = Imm32 Int deriving (Show)
newtype Imm16 = Imm16 Int deriving (Show)
newtype Imm8 = Imm8 Int deriving (Show)
newtype BitIdx = BitIdx Int deriving (Show)

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

data AstInstruction = Mov Register Register
                    | Cli -- Clear interrupts
                    | Sei -- Set interrupts
                      -- Clear flags
                    | Clc
                    | Clh
                    | Cln
                    | Cls
                    | Clv
                    | Clz
                    | Clt
                      -- Set flags
                    | Sec
                    | Seh
                    | Sen
                    | Ses
                    | Sev
                    | Sez
                    | Set
                      -- Jumps
                    | Jmp AbsPtr
                    | Ijmp
                    | Rjmp RelPtr
                      -- Add/Subtract
                    | Add Register Register
                    | Adc Register Register
                    | Sub Register Register
                    | Sbc Register Register
                      -- Math with immediates
                    | Andi Register Imm32
                    | Ldi Register Imm32
                    | Cpi Register Imm32
                    | Ori Register Imm32
                    | Subi Register Imm32
                    | Sbci Register Imm32
                      -- IO load/store
                    | In Register Imm32
                    | Out Imm32 Register
                    | Sbi Register BitIdx
                    | Cbi Register BitIdx
                      -- Conditional Skip
                    | Sbic Register BitIdx
                    | Sbis Register BitIdx
                    | Sbrc Register BitIdx
                    | Sbrcs Register BitIdx
                    | Cpse Register Register
                      -- T Flag
                    | Bld Register BitIdx
                    | Bst Register BitIdx
                      -- Register Pairs
                    | Adiw Register Register
                    | Sbiw Register Register
                    | Movw Register Register
                      -- Stack
                    | Push Register
                    | Pop Register
                      -- Branches
                    | Brcc RelPtr
                    | Brcs RelPtr
                    | Brtc RelPtr
                    | Brts RelPtr
                    | Breq RelPtr
                    | Brne RelPtr
                    | Brlt RelPtr
                    | Brge RelPtr
                    | Brpl RelPtr
                    | Brmi RelPtr
                      -- Comparisons
                    | Cp Register Register
                    | Cpc Register Register
                      -- inc/dec
                    | Inc Register
                    | Dec Register
                      -- Loads
                    | Ld Register Reg16Ex
                    | Lds Register Imm16
                    | Ldd Register Reg16Ex
                      -- Stores
                    | St Reg16Ex Register
                    | Sts Imm16 Register
                    | Std Reg16Ex Register
                      -- Data/Program Transfer
                    | Lpm Register Reg16
                    | Spm Reg16 Register
                      -- Function calls
                    | Call Imm32
                    | Rcall RelPtr
                    | Icall
                    | Ret
                      -- Math
                    | Com Register
                    | Neg Register
                    | And Register Register
                    | Eor Register Register
                    | Or Register Register
                    | Ror  Register
                    | Rol Register
                    | Lsr Register
                    | Lsl Register
                    | Asr Register
                    | Swap Register
                    deriving (Show)

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
