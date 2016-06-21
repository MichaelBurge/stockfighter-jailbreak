{-# LANGUAGE OverloadedStrings,DuplicateRecordFields,OverloadedLabels,InstanceSigs,RecordWildCards #-}

module Api.Stockfighter.Jailbreak.Decompiler.Passes where

import Api.Stockfighter.Jailbreak.Decompiler.AST
import Api.Stockfighter.Jailbreak.Types

import Control.Monad.Trans.State.Strict
import Control.Lens hiding (Context)
import Data.Data
import Data.Function (on)
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Text.Read as T

type PassM a = StateT Context IO a
type Pass = PassM ()

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
  let arg_dest = Register $ dest instruction
      arg_src = Register $ src instruction
      arg_imm8a = Imm8 $ fromMaybe (Prelude.error $ show instruction) $ a instruction
      arg_imm8 = Imm8 $ k instruction
      arg_imm16 = Imm16 $ k instruction
      arg_imm32 = Imm32 $ k instruction
      arg_relptr = mkRelptr $ k instruction
      arg_absptr = AbsPtr $ k instruction
      arg_bitidx = BitIdx $ b instruction
      arg_reg16 = case dest instruction of
        24 -> R24
        26 -> RX
        28 -> RY
        30 -> RZ
      arg_op = Imm8 $ case q instruction of Just x -> x
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
      reg16_imm8 f = f arg_reg16 arg_imm8
      reg_imm16 f = f arg_dest arg_imm16
      reg_imm8 f = f arg_dest arg_imm8
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
        ("andi",  reg_imm8 Andi),
        ("ldi",   reg_imm8 Ldi),
        ("cpi",   reg_imm8 Cpi),
        ("ori",   reg_imm8 Ori),
        ("subi",  reg_imm8 Subi),
        ("sbci",  reg_imm8 Sbci),
        ("in",    reg_imm8 In),
        ("out",   imm8_reg Out),
        ("sbi",   reg_bit Sbi),
        ("cbi",   reg_bit Cbi),
        ("sbic",  reg_bit Sbic),
        ("sbis",  reg_bit Sbis),
        ("sbrc",  reg_bit Sbrc),
        ("sbrs",  reg_bit Sbrs),
        ("cpse",  reg_reg Cpse),
        ("bld",   reg_bit Bld),
        ("bst",   reg_bit Bst),
        ("adiw",  reg16_imm8 Adiw),
        ("sbiw",  reg16_imm8 Sbiw),
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
        ("ldxm",  Ldxm arg_dest),
        ("ldy",   Ldy arg_dest),
        ("ldyp",  Ldyp arg_dest),
        ("ldym",  Ldym arg_dest),
        ("ldz",   Ldz arg_dest),
        ("ldzp",  Ldzp arg_dest),
        ("ldzm",  Ldzm arg_dest),
        ("lds",   reg_imm16 Lds),
        ("lddx",  reg_op Lddx),
        ("lddy",  reg_op Lddy),
        ("lddz",  reg_op Lddz),
        ("stx",   Stx arg_src),
        ("stxp",  Stxp arg_src),
        ("stxm",  Stxm arg_src),
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
        ("swap",  Swap arg_dest),
        ("mul",   reg_reg Mul),
        ("break", Break),
        ("reti",  Reti),
        ("unknown", Unknown),
        ("nop",     Nop)
        ]
      tableMap = Map.fromList table
      mnemonic = mnem instruction
  in case Map.lookup mnemonic tableMap of
    Nothing -> Prelude.error $ "Unknown mnemonic: " ++ T.unpack mnemonic
    Just x -> x

iex_asm :: InstructionEx -> Statement
iex_asm x@(InstructionEx{ iex_i = Instruction{ offset = o }}) = SAsm (StatementAnnotation o [ x ]) x

-- | Groups instructions by their symbol attribute, adding entries in the global symbol table
pass_groupBySymbol :: Pass
pass_groupBySymbol = do
  statements <- _ctx_statements <$> get
  let iexs = catMaybes $ map (\x -> case x of { SAsm _ y -> Just y ; _ -> Nothing }) statements
      is = map iex_i iexs
  newIndexBase <- _nextId <$> _ctx_functions <$> get
  let groups = groupBy (\a b -> not $ isJust $ symbol b) is
  let split_groups = flip mapi groups $ \disassembly i ->
        let (symbolI : instructions) = disassembly
            symbolS = fromJust $ symbol symbolI
            sym = parseSymbol symbolS
            symbolIStripped = symbolI { symbol = Nothing }
        in (newIndexBase + i, sym, symbolIStripped : instructions)
  let newFunctions = M.fromList $ map (\(a,b,c) -> (a,b)) split_groups
      mkInstructionEx i = InstructionEx i (parseInstruction i)
  let statements = flip map split_groups $ \(idx, sym, instructions) ->
        let anno = (StatementAnnotation (sym ^. sym_offset) iexs)
        in SFunction anno TVoid sym [] $
           SBlock anno $ map iex_asm $ map mkInstructionEx instructions
  ctx_functions <>= Table newFunctions (M.size newFunctions)
  ctx_statements .= statements

-- | Looks at a function's assembly to figure out parameters and adds them to the argument list.
-- 1. The lowest uninitialized register in r24-r8 determines the number of arguments.
-- 2. A register that is used uninitialized might not count if it's only being stored(via a 'Push' and later 'Pop').
-- 3. Types are deduced by seeing what kinds of instructions use them. adiw for example takes register pairs.
deduceFunctionParameters :: Function -> Function
deduceFunctionParameters function = undefined
                    
spliceListPoints :: [(Int, a)] -> [Int] -> [[(Int, a)]]
spliceListPoints elems splicePoints = case splicePoints of
  [] -> [ elems ]
  (sp : sps) ->
    let (chunk, remainder) = span (\x -> fst x < sp) elems
    in chunk : spliceListPoints remainder sps

-- replaceLocalJumpsWithGotos :: [ InstructionEx ] -> [ Statement ]
-- replaceLocalJumpsWithGotos [] = []
-- replaceLocalJumpsWithGotos instructions@(firstInstruction:_) =
--   let iex_offset (InstructionEx{iex_i = Instruction{offset = offset}}) = offset
--       initialOffset = iex_offset firstInstruction
--       relativeOffset i = iex_offset i - initialOffset
--       relativeOffsets = flip map instructions $ \i -> (relativeOffset i, i)
--       getSplicePoint instruction = case instructionEx_astInstruction instruction of
--         Rjmp (RelPtr x) ->
--           let labelId = LabelId (relativeOffset instruction)
--           in [(relativeOffset instruction, SGoto labelId),
--               (relativeOffset instruction + x, SLabel labelId)]
--         _ -> []
--       splicePoints = concatMap getSplicePoint instructions
--       chunks = spliceListPoints relativeOffsets $ map fst splicePoints
--       statements = flip map chunks $ \chunk ->
--         let wasSpliced x = case instructionEx_astInstruction x of
--               Rjmp _ -> True
--               _ -> False
--             asm = SAsm $ filter (not . wasSpliced) $ map snd chunk
--         in case chunk of
--           [] -> (initialOffset, asm)
--           (i:_) -> (fst i, asm)
--   in map snd $ sortBy (compare `on` fst) $ splicePoints ++ statements

-- pass_replaceLocalJumpsWithGotos :: Pass
-- pass_replaceLocalJumpsWithGotos = do
--   ctx <- get
--   let processOneStatement s = case s of
--         f@(SFunction a b c d body) -> SFunction a b c d $ flip concatMap body $ \s -> case s of
--           SAsm d iexs -> replaceLocalJumpsWithGotos iexs
--           x -> [ x ]
--         x -> x
--   ctx_statements %= map processOneStatement
--   return ()

pass_replaceLocalJumpsWithGotos2 :: Pass
pass_replaceLocalJumpsWithGotos2 = do
  ctx <- get
  ctx_statements %= map (replaceSubtree replaceRelativeJumpsWithGotos2)
  return ()

annotation :: Lens' Statement StatementAnnotation
annotation =
  let getter = \case
        (SAssign a _ _) -> a
        (SLabel a _) -> a
        (SGoto a _) -> a
        (SAsm a _) -> a
        (SBlock a _) -> a
        (SVariable a _ _ _) -> a
        (SFunction a _ _ _ _) -> a
        (SIfElse a _ _ _) -> a
      setter a x = case a of
        (SAssign a b c) -> SAssign x b c
        (SLabel a b) -> SLabel x b
        (SGoto a b) -> SGoto x b
        (SAsm a b) -> SAsm x b
        (SBlock a b) -> SBlock x b
        (SVariable a b c d) -> SVariable x b c d
        (SFunction a b c d e) -> SFunction x b c d e
        (SIfElse a b c d) -> SIfElse x b c d
  in lens getter setter

-- instance Functor StatementEx where
--   fmap f statement =
--     let mapChildren xs = map (fmap f)
--     in case statement of
--       SAssign a b c -> SAssign (f a) b c
--       SLabel a b -> SLabel (f a) b
--       SGoto a b -> SGoto (f a) b
--       SAsm a b -> SAsm (f a) b
--       SBlock a xs -> SBlock (f a) $ mapChildren xs
--       SVariable a b c d -> SVariable (f a) b c d
--       SFunction a b c d e -> SFunction (f a) b c (mapChildren d) (fmap f e)
--       SIfElse a b c d -> SIfElse (f a) b (fmap f c) (fmap f d)

-- instance Foldable StatementEx where
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--   foldMap f statement =
--     let fm = foldMap f
--         mapChildren = map fm
--     in case statement of
--       SAssign a b c -> f a
--       SLabel a b -> f a
--       SGoto a b -> f a
--       SBlock a xs -> f a <> mconcat (mapChildren xs)
--       SVariable a _ _ _ -> f a
--       SFunction a _ _ cs d -> f a <> mconcat (mapChildren cs) <> d
--       SIfElse a _ c d -> f a <> fm c <> fm d

-- instance Traversable StatementEx where
--   traverse :: Applicative f => (a -> f b) -> StatementEx a -> f (StatementEx b)
--   traverse f statement = case statement of
--     SAssign a b c -> SAssign <$> f a <*> b <*> c
--     SLabel a b -> SLabel <$> f a <*> b
--     SGoto a b -> SGoto <$> f a <*> b
--     SBlock a xs -> SBlock <$> f a <*> traverse (traverse f) xs
--     SVariable a b c d -> SVariable <$> f a <*> b <*> c <*> d
--     SFunction a b c ds e -> SFunction <$> f a <*> b <*> c <*> traverse (traverse f) d <*> traverse f e
--     SIfElse a b c d -> SIfElse <$> f a <*> b <*> traverse f c <*> traverse f d

-- liftStat :: Iso' Statement (StatementEx Statement)
-- liftStat =
--   let forward :: Statement -> StatementEx Statement
--       forward stat = case stat of
--         SAssign a b c -> SAssign stat b c
--         SLabel a b -> SLabel stat b
--         SGoto a b -> SGoto stat b
--         SBlock a xs -> SBlock stat xs
--         SVariable a b c d -> SVariable stat b c d
--         SFunction a b c ds e -> SFunction stat b c ds e
--         SIfElse a b c d -> SIfElse stat b c d
--       backward :: StatementEx Statement -> Statement
--       backward stat = stat ^. annotation
--   in iso forward backward

-- replaceSubtree :: (Statement -> [ Statement ]) -> Statement -> [ Statement ]
-- replaceSubtree f stat = flip traverse stat $ \anno ->
  
--   (sort . f) stat

normalizeStatement :: Statement -> Statement
normalizeStatement stat =
  let expandChild stmt = case stmt of
        SBlock _ children -> children
        _ -> [ stmt ]
  in case stat of
    SAssign _ _ _ -> stat
    SLabel _ _ -> stat
    SGoto _ _ -> stat
    SAsm _ _ -> stat
    SBlock offset children -> case children of
      [ x ] -> normalizeStatement x
      _ -> SBlock offset $ sortBy (compare `on` (^. annotation)) $ concatMap expandChild children
    SVariable _ _ _ _ -> stat
    SFunction a b c ds e -> SFunction a b c (map normalizeStatement ds) (normalizeStatement e)
    SIfElse a b c d -> SIfElse a b (normalizeStatement c) (normalizeStatement d)

replaceSubtree :: (Statement -> [ Statement ] ) -> Statement -> Statement
replaceSubtree f stat =
  let rewrite = replaceSubtree f
  in normalizeStatement $ SBlock (stat ^. annotation) $ f $
     case stat of
       SAssign _ _ _ -> stat
       SLabel _ _ -> stat
       SGoto _ _ -> stat
       SAsm _ _ -> stat
       SBlock offset children -> SBlock offset $ map rewrite children
       SVariable _ _ _ _ -> stat
       SFunction a b c ds e -> SFunction a b c (map rewrite ds) (rewrite e)
       SIfElse a b c d -> SIfElse a b (rewrite c) (rewrite d)

replaceRelativeJumpsWithGotos2 :: Statement -> [ Statement ]
replaceRelativeJumpsWithGotos2 stat = case stat of
  SAsm anno instr@(InstructionEx { iex_astI = Rjmp (RelPtr o) }) ->
    let initOff = stmtAnno_offset anno
        labelId = LabelId $ initOff + o
    in [ (SLabel (StatementAnnotation (initOff + o) [ ]) labelId),
         (SGoto (StatementAnnotation initOff [ instr ]) labelId)]
  _ -> [ stat ]

getRegisterPair :: Register -> Register -> Maybe Reg16
getRegisterPair r1 r2 = case (r1,r2) of
  (Register 24, Register 25) -> Just R24
  (Register 26, Register 27) -> Just RX
  (Register 28, Register 29) -> Just RY
  (Register 30, Register 31) -> Just RZ
  _ -> Nothing
  
-- pass_pruneEmptyAssemblyBlocks :: Pass
-- pass_pruneEmptyAssemblyBlocks = do
--   replaceInstructionPatternWithStatements 2 $ \case
--     [ InstructionEx { iex_astI = Or r1 r2 },
--       i2@InstructionEx { iex_astI = Breq relptr } ] -> case getRegisterPair r1 r2 of
--       Nothing -> Nothing
--       Just r16 -> Just [
--         SIfElse (EUnop Dereference $ EReg16 r16) ( SAsm $ i2 { iex_astI = Rjmp relptr } ) (SBlock [])
--         ]
--     _ -> Nothing
