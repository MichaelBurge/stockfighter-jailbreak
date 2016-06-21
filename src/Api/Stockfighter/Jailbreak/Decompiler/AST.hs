{-# LANGUAGE GADTs,StandaloneDeriving,DataKinds,TemplateHaskell,DeriveDataTypeable #-}

module Api.Stockfighter.Jailbreak.Decompiler.AST where

import Api.Stockfighter.Jailbreak.Types

import qualified Data.IntMap.Strict as M
import qualified Data.Text as T

import Control.Lens.TH
import Control.Monad.Trans.Reader
import Data.Data
import Data.Monoid
import Data.Typeable
import Numeric
import Text.PrettyPrint hiding ((<>))
import Text.Printf

newtype FunctionId = FunctionId Int deriving (Show, Eq, Data, Typeable)
newtype LabelId = LabelId Int deriving (Show, Eq, Data, Typeable)
newtype VariableId = VariableId Int deriving (Eq, Show, Data, Typeable)
newtype Register = Register Int deriving (Eq, Data, Typeable)
instance Show Register where
  show (Register id) = "r" ++ show id

regJunk = Register 0
regZero = Register 1

data Reg16 = R16 Int
           | RX
           | RY
           | RZ
           deriving (Eq, Data, Typeable)

instance Show Reg16 where
  show (R16 i) = "R" ++ show i
  show RX = "X"
  show RY = "Y"
  show RZ = "Z"

mkReg16 :: Int -> Reg16
mkReg16 x = case x of
  26 -> RX
  28 -> RY
  30 -> RZ
  _  -> R16 x

getRegisterPair :: Register -> Register -> Maybe Reg16
getRegisterPair r1 r2 = case (r1,r2) of
  (Register 26, Register 27) -> Just RX
  (Register 28, Register 29) -> Just RY
  (Register 30, Register 31) -> Just RZ
  (Register x, Register y) | x == (y-1) -> Just $ R16 x
  _ -> Nothing

regPairs :: Reg16 -> (Register, Register)
regPairs (R16 x) = (Register x, Register $ x+1)
regPairs RX = (Register 26, Register 27)
regPairs RY = (Register 28, Register 29)
regPairs RZ = (Register 30, Register 31)

data RegOp = RegOpUnchanged
           | RegOpInc
           | RegOpDec
           | RegOpPlus Int
           | RegOpMinus Int
           deriving (Eq, Show, Data, Typeable)

newtype AbsPtr = AbsPtr Int deriving (Eq, Data, Typeable)
instance Show AbsPtr where
  show (AbsPtr id) = printf "0x%08x" id

newtype RelPtr = RelPtr Int deriving (Eq, Data, Typeable)
instance Show RelPtr where
  show (RelPtr id) = "." ++ show id

mkRelptr :: Int -> RelPtr
mkRelptr id = RelPtr $
  if id > 32767
  then id - 65536
  else id

newtype Imm32 = Imm32 Int deriving (Eq, Data, Typeable)
instance Show Imm32 where
  show (Imm32 id) = printf "0x%08x" id

newtype Imm16 = Imm16 Int deriving (Eq, Data, Typeable)
instance Show Imm16 where
  show (Imm16 id) = printf "0x%04x" id

newtype Imm8 = Imm8 Int deriving (Eq, Data, Typeable)
instance Show Imm8 where
  show (Imm8 id) = printf "0x%02x" id

newtype BitIdx = BitIdx Int deriving (Eq, Data, Typeable)
instance Show BitIdx where
  show (BitIdx id) = show id

data Symbol = Symbol {
  _sym_symbol :: T.Text,
  _sym_offset :: Int
  } deriving (Show, Eq, Data, Typeable)

data Table a = Table {
  _elements :: M.IntMap a,
  _nextId :: Int
  } deriving (Show)

instance Monoid (Table a) where
  mempty = Table mempty 0
  mappend (Table a1 b1) (Table a2 b2) = Table (a1 <> a2) (b1 + b2)

data Context = Context {
  _ctx_functions  :: Table Symbol,
  _ctx_variables  :: Table Statement,
  _ctx_statements :: [ Statement ]
  } deriving (Show)

instance Monoid Context where
  mempty = Context mempty mempty mempty
  mappend (Context a1 b1 c1) (Context a2 b2 c2) = Context (a1 `mappend` a2) (b1 `mappend` b2) (c1 `mappend` c2)

class PrintAst a where
  printNode :: a -> Reader Context Doc

data Unop = Negate
          | Not
          | Dereference
          | PostIncrement
          | PreIncrement
          | BitComplement
          deriving (Show, Eq, Data, Typeable)

data Binop = Plus
           | Subtract
           | Multiply
           | Divide
           | Mod
           | BitAnd
           | BitOr
           | BitXor
           | BitShiftRight
           | BitShiftLeft
           | Assign
           | AssignPlus
           | AssignMinus
           | AssignMultiply
           | AssignDivide  
           | AssignBitXor
           | AssignBitAnd
           | AssignBitOr
           | AssignBitShiftRight
           | AssignBitShiftLeft
           deriving (Eq, Show, Data, Typeable)

data Literal = LImm8 Imm8
             | LImm16 Imm16
             | LImm32 Imm32
             deriving (Eq, Show, Data, Typeable)

data Expression where
  ELiteral :: Literal -> Expression
  EUnop :: Unop -> Expression -> Expression
  EBinop :: Binop -> Expression -> Expression -> Expression
  ECall :: Symbol -> [ Expression ] -> Expression
  EReg8 :: Register -> Expression
  EReg16 :: Reg16 -> Expression

deriving instance Show Expression
deriving instance Eq Expression 
deriving instance Data Expression
deriving instance Typeable Expression

data Type where
  TVoid    :: Type
  TInt     :: Type
  TIntPtr  :: Type
  TChar    :: Type
  TCharPtr :: Type

deriving instance Show Type
deriving instance Eq Type
deriving instance Data Type
deriving instance Typeable Type

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
                    | Andi Register Imm8
                    | Ldi Register Imm8
                    | Cpi Register Imm8
                    | Ori Register Imm8
                    | Subi Register Imm8
                    | Sbci Register Imm8
                      -- IO load/store
                    | In Register Imm8
                    | Out Imm8 Register
                    | Sbi Register BitIdx
                    | Cbi Register BitIdx
                      -- Conditional Skip
                    | Sbic Register BitIdx
                    | Sbis Register BitIdx
                    | Sbrc Register BitIdx
                    | Sbrs Register BitIdx
                    | Cpse Register Register
                      -- T Flag
                    | Bld Register BitIdx
                    | Bst Register BitIdx
                      -- Register Pairs
                    | Adiw Reg16 Imm8
                    | Sbiw Reg16 Imm8
                    | Movw Reg16 Reg16
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
                    | Ldx Register
                    | Ldxp Register
                    | Ldxm Register
                    | Ldy Register
                    | Ldyp Register
                    | Ldym Register
                    | Ldz Register
                    | Ldzp Register
                    | Ldzm Register
                    | Lds Register Imm16
                    | Lddx Register Imm8
                    | Lddy Register Imm8
                    | Lddz Register Imm8
                      -- Stores
                    | Stx Register
                    | Sty Register
                    | Stz Register
                    | Stxp Register
                    | Stxm Register
                    | Styp Imm8 Register
                    | Stzp Register
                    | Sts Imm16 Register
                    | Stdz Imm8 Register
                      -- Data/Program Transfer
                    | Lpmz Register
                    | Lpmzp Register
                    | Spm
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
                    | Ror Register
                    | Rol Register
                    | Lsr Register
                    | Lsl Register
                    | Asr Register
                    | Swap Register
                      -- Undocumented
                    | Mul Register Register
                    | Break
                    | Reti
                    | Unknown
                    | Nop
                    deriving (Eq, Show, Data, Typeable)

data InstructionEx = InstructionEx {
  iex_i    :: Instruction,
  iex_astI :: AstInstruction
  } deriving (Eq, Show, Data, Typeable)

data StatementAnnotation = StatementAnnotation {
  stmtAnno_offset :: Int,
  stmtAnno_is     :: [ InstructionEx ]
  } deriving (Eq, Show, Data, Typeable)

instance Ord StatementAnnotation where
  compare (StatementAnnotation{stmtAnno_offset = offset1}) (StatementAnnotation{stmtAnno_offset = offset2}) = offset1 `compare` offset2

data StatementEx a where
  SExpression :: (Eq a, Show a) => a -> Expression -> StatementEx a
  SLabel      :: (Eq a, Show a) => a -> LabelId -> StatementEx a
  SGoto       :: (Eq a, Show a) => a -> LabelId -> StatementEx a
  SAsm        :: (Eq a, Show a) => a -> InstructionEx -> StatementEx a
  SBlock      :: (Eq a, Show a) => a -> [ StatementEx a ] -> StatementEx a
  SVariable   :: (Eq a, Show a) => a -> VariableId -> Type -> Maybe Expression -> StatementEx a
  SFunction   :: (Eq a, Show a) => a -> Type -> Symbol -> [ StatementEx a] -> StatementEx a -> StatementEx a
  SIfElse     :: (Eq a, Show a) => a -> Expression -> StatementEx a -> StatementEx a -> StatementEx a
  SReturn     :: (Eq a, Show a) => a -> Maybe Expression -> StatementEx a


deriving instance Show (StatementEx a)
deriving instance Eq (StatementEx a)
-- deriving instance Data (StatementEx a)
-- deriving instance Typeable (StatementEx a)

type Statement = StatementEx StatementAnnotation

makeLenses ''Symbol
makeLenses ''Table
makeLenses ''Context
makePrisms ''Unop
makeLenses ''StatementEx
makePrisms ''Binop
makeLenses ''Type
makeLenses ''Expression
