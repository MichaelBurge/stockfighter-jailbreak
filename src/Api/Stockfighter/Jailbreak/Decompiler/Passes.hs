{-# LANGUAGE OverloadedStrings #-}

module Api.Stockfighter.Jailbreak.Decompiler.Passes where

import Api.Stockfighter.Jailbreak.Decompiler.AST
import Api.Stockfighter.Jailbreak.Types

import Control.Monad.Trans.State.Strict
import Control.Lens hiding (Context)
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Text.Read as T

type PassM a = StateT Context IO a
type Pass = PassM [ Statement ]

mapi :: (a -> Int -> b) -> [a] -> [b]
mapi f xs = zipWith f xs [0..]

parseSymbol :: T.Text -> Symbol
parseSymbol symbolS =
  let name = T.takeWhile (/= '>') $ T.tail $ T.dropWhile (/= '<') $ symbolS
      offset = fromIntegral $ fst $ either undefined id $ T.decimal $ T.takeWhile (/= ':') symbolS
  in Symbol {
    _sym_symbol = name,
    _sym_offset = offset
  }

parseInstruction :: Instruction -> AstInstruction
parseInstruction instruction =
  let arg_dest = RRegister $ dest instruction
      arg_src = RRegister $ src instruction
      arg_imm8 = Imm8 $ fromJust $ a instruction
      arg_imm16 = Imm16 $ k instruction
      arg_imm32 = Imm32 $ k instruction
      arg_relptr = RelPtr $ k instruction
      arg_absptr = AbsPtr $ k instruction
      arg_bitidx = BitIdx $ b instruction
      arg_op = Imm8 $ fromJust $ q instruction
      absptr :: (AbsPtr -> AstInstruction) -> AstInstruction
      absptr f = f arg_absptr
      relptr :: (RelPtr -> AstInstruction) -> AstInstruction
      relptr f = f arg_relptr
      reg_reg :: (Register -> Register -> AstInstruction) -> AstInstruction
      reg_reg f = f arg_dest arg_src
      reg_imm32 :: (Register -> Imm32 -> AstInstruction) -> AstInstruction
      reg_imm32 f = f arg_dest arg_imm32
      imm8_reg :: (Imm8 -> Register -> AstInstruction) -> AstInstruction
      imm8_reg f = f arg_imm8 arg_src
      imm16_reg f = f arg_imm16 arg_src
      reg_imm16 f = f arg_dest arg_imm16
      reg_bit :: (Register -> BitIdx -> AstInstruction) -> AstInstruction
      reg_bit f = f arg_src arg_bitidx
      reg_op :: (Register -> Imm8 -> AstInstruction) -> AstInstruction
      reg_op f = f arg_dest arg_op
      op_reg f = f arg_op arg_src
      imm32 :: (Imm32 -> AstInstruction) -> AstInstruction
      imm32 f = f arg_imm32
    
      table :: [ (T.Text, AstInstruction) ]
      table = [
        ("mov",   reg_reg Mov),
        ("cli",   Cli),
        ("sei",   Sei),
        ("clc",   Clc),
        ("clh",   Clh),
        ("cln",   Cln),
        ("cls",   Cls),
        ("clv",   Clv),
        ("clz",   Clz),
        ("clt",   Clt),
        ("sec",   Sec),
        ("seh",   Seh),
        ("sen",   Sen),
        ("ses",   Ses),
        ("sev",   Sev),
        ("sez",   Sez),
        ("set",   Set),
        ("jmp",   absptr Jmp),
        ("ijmp",  Ijmp),
        ("rjmp",  relptr Rjmp),
        ("add",   reg_reg Add),
        ("adc",   reg_reg Adc),
        ("sub",   reg_reg Sub),
        ("sbc",   reg_reg Sbc),
        ("andi",  reg_imm32 Andi),
        ("ldi",   reg_imm32 Ldi),
        ("cpi",   reg_imm32 Cpi),
        ("ori",   reg_imm32 Ori),
        ("subi",  reg_imm32 Subi),
        ("sbci",  reg_imm32 Sbci),
        ("in",    reg_imm32 In),
        ("out",   imm8_reg Out),
        ("sbi",   reg_bit Sbi),
        ("cbi",   reg_bit Cbi),
        ("sbic",  reg_bit Sbic),
        ("sbis",  reg_bit Sbis),
        ("sbrc",  reg_bit Sbrc),
        ("sbrcs", reg_bit Sbrcs),
        ("cpse",  reg_reg Cpse),
        ("bld",   reg_bit Bld),
        ("bst",   reg_bit Bst),
        ("adiw",  reg_reg Adiw),
        ("sbiw",  reg_reg Sbiw),
        ("movw",  reg_reg Movw),
        ("push",  Push arg_src),
        ("pop",   Pop arg_dest),
        ("brcc",  relptr Brcc),
        ("brcs",  relptr Brcs),
        ("brtc",  relptr Brtc),
        ("brts",  relptr Brts),
        ("breq",  relptr Breq),
        ("brne",  relptr Brne),
        ("brlt",  relptr Brlt),
        ("brge",  relptr Brge),
        ("brpl",  relptr Brpl),
        ("brmi",  relptr Brmi),
        ("cp",    reg_reg Cp),
        ("cpc",   reg_reg Cpc),
        ("inc",   Inc arg_dest),
        ("dec",   Dec arg_dest),
        ("ldx",   Ldx arg_dest),
        ("ldxp",  Ldxp arg_dest),
        ("ldy",   Ldy arg_dest),
        ("ldyp",  Ldyp arg_dest),
        ("ldz",   Ldz arg_dest),
        ("ldzp",  Ldzp arg_dest),
        ("lds",   reg_imm16 Lds),
        ("lddx",  reg_op Lddx),
        ("lddy",  reg_op Lddy),
        ("lddz",  reg_op Lddz),
        ("stx",   Stx arg_src),
        ("stxp",  Stxp arg_src),
        ("sty",   Sty arg_src),
        ("sts",   imm16_reg Sts),
        ("sty",   Sty arg_src),
        ("styp",  op_reg Styp),
        ("stz",   Stz arg_src),
        ("stzp",  Stzp arg_src),
        ("stdz",  op_reg Stdz),
        ("lpmz",  Lpmz arg_dest),
        ("lpmzp", Lpmzp arg_dest),
        ("spm",   Spm),
        ("call",  imm32 Call),
        ("rcall", relptr Rcall),
        ("icall", Icall),
        ("ret",   Ret),
        ("com",   Com arg_dest),
        ("neg",   Neg arg_dest),
        ("and",   reg_reg And),
        ("eor",   reg_reg Eor),
        ("or",    reg_reg Or),
        ("ror",   Ror arg_dest),
        ("rol",   Rol arg_dest),
        ("lsr",   Lsr arg_dest),
        ("asr",   Asr arg_dest),
        ("swap",  Swap arg_dest)
        ]
      tableMap = Map.fromList table
  in fromJust $ Map.lookup (mnem instruction) tableMap

-- | Groups instructions by their symbol attribute, adding entries in the global symbol table
groupBySymbol :: Pass
groupBySymbol = do
  is <- _ctx_assembly <$> get
  newIndexBase <- _nextId <$> _ctx_functions <$> get
  let groups = groupBy (\a b -> not $ isJust $ symbol b) is
  let split_groups = flip mapi groups $ \disassembly i ->
        let (symbolI : instructions) = disassembly
            symbolS = fromJust $ symbol symbolI
            sym = parseSymbol symbolS
            symbolIStripped = symbolI { symbol = Nothing }
        in (newIndexBase + i, sym, symbolIStripped : instructions)
  let newFunctions = M.fromList $ map (\(a,b,c) -> (a,b)) split_groups
  let statements = flip map split_groups $ \(idx, sym, instructions) ->
        SFunction TVoid sym [] [ SAsm instructions ] 
  ctx_functions <>= Table newFunctions (M.size newFunctions)
  ctx_assembly .= [] -- No more free-floating assembly
  return statements
