{-# LANGUAGE OverloadedStrings,DuplicateRecordFields,OverloadedLabels,InstanceSigs,RecordWildCards,ViewPatterns #-}

module Api.Stockfighter.Jailbreak.Decompiler.Passes where

import Api.Stockfighter.Jailbreak.Decompiler.AST
import Api.Stockfighter.Jailbreak.Types

import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Lens hiding (Context, elements)
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
      reader = case T.find (=='<') symbolS of
        Nothing -> T.decimal
        Just _ -> T.hexadecimal
      offset = fromIntegral $ fst $ either undefined id $ reader $ T.takeWhile (/= ':') symbolS
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
      arg_reg16 = mkReg16 $ dest instruction
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
      imm8a_reg f = f arg_imm8a arg_src
      imm16_reg f = f arg_imm16 arg_src
      reg16_imm8 f = f arg_reg16 arg_imm8
      reg_imm16 f = f arg_dest arg_imm16
      reg_imm8 f = f arg_dest arg_imm8
      reg_imm8a f = f arg_dest arg_imm8a
      reg16_reg16 f = f (mkReg16 $ dest instruction) (mkReg16 $ src instruction)
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
        ("in",    reg_imm8a In),
        ("out",   imm8a_reg Out),
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
        ("movw",  reg16_reg16 Movw),
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
iex_asm x@(InstructionEx{ iex_i = Instruction{ offset = o }}) = SAsm (StatementAnnotation o Nothing [ x ]) x

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
        let anno = (StatementAnnotation (sym ^. sym_offset) Nothing iexs)
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

set_expr :: Setter' Statement Expression
set_expr = setting f
  where
    f g = replaceSubtree (replaceExprs g)
    replaceExprs g stmt = case stmt of
      SExpression a e -> pure $ SExpression a $ g e
      SIfElse a cond s1 s2 -> pure $ SIfElse a (g cond) s1 s2
      SWhile a cond s -> pure $ SWhile a (g cond) s
      _ -> [stmt]
  
spliceListPoints :: [(Int, a)] -> [Int] -> [[(Int, a)]]
spliceListPoints elems splicePoints = case splicePoints of
  [] -> [ elems ]
  (sp : sps) ->
    let (chunk, remainder) = span (\x -> fst x < sp) elems
    in chunk : spliceListPoints remainder sps

pass_replaceLocalJumpsWithGotos :: Pass
pass_replaceLocalJumpsWithGotos = do
  ctx <- get
  ctx_statements %= map (replaceSubtree replaceLocalJumpsWithGotos)
  return ()

pass_replaceBranchesWithJumps :: Pass
pass_replaceBranchesWithJumps = do
  ctx <- get
  ctx_statements %= map (replaceGroup 2 replaceBranchesWithJumps)
  return ()

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

normalizeStatement :: Statement -> Statement
normalizeStatement stat =
  let expandChild stmt = case stmt of
        SBlock _ children -> children
        _ -> [ stmt ]
  in case stat of
    SExpression _ _ -> stat
    SLabel _ _ -> stat
    SGoto _ _ -> stat
    SAsm _ _ -> stat
    SBlock offset children -> case children of
      [ x ] -> normalizeStatement x
      _ -> SBlock offset $ sortBy (compare `on` (^. annotation)) $ concatMap expandChild children
    SVariable _ _ _ _ -> stat
    SFunction a b c ds e -> SFunction a b c (map normalizeStatement ds) (normalizeStatement e)
    SIfElse a b c d -> SIfElse a b (normalizeStatement c) (normalizeStatement d)
    SReturn _ _ -> stat
    SWhile a b stmt -> SWhile a b (normalizeStatement stmt)
    SContinue _ -> stat

replaceSubtree :: (Statement -> [ Statement ] ) -> Statement -> Statement
replaceSubtree f stat =
  let rewrite = replaceSubtree f
  in normalizeStatement $ SBlock (stat ^. annotation) $ f $
     case stat of
       SExpression _ _ -> stat
       SLabel _ _ -> stat
       SGoto _ _ -> stat
       SAsm _ _ -> stat
       SBlock offset children -> SBlock offset $ map rewrite children
       SVariable _ _ _ _ -> stat
       SFunction a b c ds e -> SFunction a b c (map rewrite ds) (rewrite e)
       SIfElse a b c d -> SIfElse a b (rewrite c) (rewrite d)
       SReturn _ _ -> stat
       SWhile a b c -> SWhile a b (rewrite c)
       SContinue _ -> stat

replace :: Int -> ([a] -> Maybe [a]) -> [a] -> [a]
replace i f [] = []
replace i f xs | length xs < i = xs
replace i f xs = case f $ take i xs of
  Nothing -> (head xs) : replace i f (tail xs)
  Just ys -> ys ++ replace i f (drop i xs)

replaceGroup :: Int -> ([Statement] -> Maybe [Statement]) -> Statement -> Statement
replaceGroup i f stat =
  let rewrite = replaceGroup i f
  in normalizeStatement $ case stat of
    SExpression _ _ -> stat
    SLabel _ _ -> stat
    SGoto _ _ -> stat
    SAsm _ _ -> stat
    SBlock anno children -> SBlock anno $ replace i f $ map rewrite children
    SVariable _ _ _ _ -> stat
    SFunction a b c ds e -> SFunction a b c (replace i f $ map rewrite ds) (rewrite e)
    SIfElse a b c d -> SIfElse a b (rewrite c) (rewrite d)
    SReturn _ _ -> stat


labelAndGotoForRelptr :: StatementAnnotation -> RelPtr -> (Statement, Statement)
labelAndGotoForRelptr anno (RelPtr o) =
  let initOff = anno ^. stmtAnno_offset
      labelId = LabelId $ initOff + o
  in ((SLabel (StatementAnnotation (initOff + o) (Just labelId) []) labelId),
      (SGoto anno labelId))

eAssign = EBinop Assign
eAssignPlus = EBinop AssignPlus
eAssignXor = EBinop AssignBitXor
eAssignAnd = EBinop AssignBitAnd
eAssignOr = EBinop AssignBitOr
eAssignShiftRight = EBinop AssignBitShiftRight
eNot = EUnop Not
eAssignShiftLeft = EBinop AssignBitShiftLeft
ePlus = EBinop Plus
eMinus = EBinop Subtract
eSub = EBinop Subtract
eMultiply = EBinop Multiply
eDivide = EBinop Divide
eXor = EBinop BitXor
eOr = EBinop BitOr
eAnd = EBinop BitAnd
eShiftRight = EBinop BitShiftRight
eShiftLeft = EBinop BitShiftLeft
eDereference = EUnop Dereference

eImm8 = ELiteral . LImm8
eImm16 = ELiteral . LImm16
eImm32 = ELiteral . LImm32

pImm8 (ELiteral (LImm8 x)) = Just x
pImm8 _ = Nothing
pImm16 (ELiteral (LImm16 x)) = Just x
pImm16 _ = Nothing
pImm32 (ELiteral (LImm32 x)) = Just x
pImm32 _ = Nothing

replaceSingleInstructions :: Statement -> [ Statement ]
replaceSingleInstructions stmt =
  let anyLdReg16 (Ldxp r) = Just (RX, r)
      anyLdReg16 (Ldyp r) = Just (RY, r)
      anyLdReg16 (Ldzp r) = Just (RZ, r)
      anyLdReg16 _ = Nothing
  in case stmt of
  SAsm anno (iex_astI -> Eor r1 r2) | r1 == r2 -> [ SExpression anno $ eAssign (EReg8 r1) (eImm8 $ Imm8 0) ]
  SAsm anno (iex_astI -> Eor r1 r2) -> [ SExpression anno $ eAssignXor (EReg8 r1) (EReg8 r2) ]
  SAsm anno (iex_astI -> Andi r imm) -> [ SExpression anno $ eAssignAnd (EReg8 r) (eImm8 imm) ]
  SAsm anno (iex_astI -> Or r1 r2) -> [ SExpression anno $ eAssignOr (EReg8 r1) (EReg8 r2) ]
  SAsm anno (iex_astI -> Ori r imm) -> [ SExpression anno $ eAssignOr (EReg8 r) (eImm8 imm) ]
  SAsm anno (iex_astI -> Lsr r) -> [ SExpression anno $ eAssignShiftRight (EReg8 r) (eImm8 $ Imm8 1) ]
  SAsm anno (iex_astI -> Lsl r) -> [ SExpression anno $ eAssignShiftLeft (EReg8 r) (eImm8 $ Imm8 1) ]
  SAsm anno (iex_astI -> Mov r1 r2) -> [ SExpression anno $ eAssign (EReg8 r1) (EReg8 r2) ]
  SAsm anno (iex_astI -> Add r1 r2) -> [ SExpression anno $ eAssignPlus (EReg8 r1) (EReg8 r2) ]
  SAsm anno (iex_astI -> Lds r imm) -> [ SExpression anno $ EBinop Assign (EReg8 r) (EUnop Dereference $ ELiteral $ LImm16 imm) ]
  SAsm anno (iex_astI -> Lddx r imm) -> [SExpression anno $ EBinop Assign (EReg8 r) (EUnop Dereference $ EBinop Plus (EReg16 RX) (ELiteral $ LImm8 imm))]
  SAsm anno (iex_astI -> Lddy r imm) -> [SExpression anno $ EBinop Assign (EReg8 r) (EUnop Dereference $ EBinop Plus (EReg16 RY) (ELiteral $ LImm8 imm))]
  SAsm anno (iex_astI -> Lddz r imm) -> [SExpression anno $ EBinop Assign (EReg8 r) (EUnop Dereference $ EBinop Plus (EReg16 RZ) (ELiteral $ LImm8 imm))]
  SAsm anno (iex_astI -> Ldi r imm) -> [ SExpression anno $ EBinop Assign (EReg8 r) (ELiteral $ LImm8 imm) ]
  SAsm anno (iex_astI -> Adiw r1 imm) -> [ SExpression anno $ EBinop AssignPlus (EReg16 r1) (ELiteral $ LImm8 imm) ]
  SAsm anno (iex_astI -> Sbiw r1 imm) -> [ SExpression anno $ EBinop AssignMinus (EReg16 r1) (ELiteral $ LImm8 imm) ]
  SAsm anno (iex_astI -> Movw r1 r2) -> [ SExpression anno $ EBinop Assign (EReg16 r1) (EReg16 r2) ]
  SAsm anno (iex_astI -> (anyLdReg16 -> Just (r16, r8))) -> [ SExpression anno $ EBinop Assign (EReg8 r8) (EUnop Dereference (EReg16 r16)) ]
  SAsm anno (iex_astI -> Ldxp r) -> [ SExpression anno $ EBinop Assign (EReg8 r) (EUnop PostIncrement (EUnop Dereference (EReg16 RX))) ]
  SAsm anno (iex_astI -> Ldyp r) -> [ SExpression anno $ EBinop Assign (EReg8 r) (EUnop PostIncrement (EUnop Dereference (EReg16 RY))) ]
  SAsm anno (iex_astI -> Ldyp r) -> [ SExpression anno $ EBinop Assign (EReg8 r) (EUnop PostIncrement (EUnop Dereference (EReg16 RY))) ]
  SAsm anno (iex_astI -> Stdz imm r) -> [ SExpression anno $ EBinop Assign (EUnop Dereference (EBinop Plus (EReg16 RZ) (ELiteral $ LImm8 imm))) (EReg8 r)]
  SAsm anno (iex_astI -> Stz r) -> [ SExpression anno $ eAssign (EReg8 r) (eDereference $ EReg16 RZ) ]
  SAsm anno (iex_astI -> Styp q r) -> [ SExpression anno $ eAssign (EReg8 r) (eDereference $ ePlus (EReg16 RZ) (eImm8 q)) ]
  SAsm anno (iex_astI -> Icall) -> [ SExpression anno $ ECall (EReg16 RZ) [] ]
  SAsm anno (iex_astI -> Ret) -> [ SReturn anno Nothing ]
  SAsm anno (iex_astI -> Swap r) ->
    let higher = eShiftLeft (eAnd (EReg8 r) (eImm8 $ Imm8 0x07)) (eImm8 $ Imm8 4)
        lower = eShiftRight (eAnd (EReg8 r) (eImm8 $ Imm8 0x70)) (eImm8 $ Imm8 4)
    in [ SExpression anno $ eAssign (EReg8 r) (eOr higher lower) ]
  _ -> [ stmt ]

pass_replaceSingleInstructions :: Pass
pass_replaceSingleInstructions = do
  ctx <- get
  ctx_statements %= map (replaceSubtree replaceSingleInstructions)
  return ()

replaceLocalJumpsWithGotos :: Statement -> [ Statement ]
replaceLocalJumpsWithGotos stat =
  let emitForRelptr anno relptr = let (a,b) = labelAndGotoForRelptr anno relptr in [ a, b]
  in case stat of
    SAsm anno instr@(iex_astI -> Rjmp relptr) -> emitForRelptr anno relptr
    _ -> [ stat ]


withPair r1 r2 f = case getRegisterPair r1 r2 of
  Nothing -> Nothing
  Just x -> f x

withPairEither :: Register -> Register -> (Reg16 -> a) -> Maybe a
withPairEither r1 r2 f = case getRegisterPair r1 r2 of
  Nothing -> f <$> getRegisterPair r2 r1
  Just x -> Just $ f x

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

anyCompare :: Binop -> Bool
anyCompare Equal = True
anyCompare GreaterOrEqual = True
anyCompare Greater = True
anyCompare LessOrEqual = True
anyCompare Less = True
anyCompare _ = False

expr_simplify :: Expression -> Maybe Expression
expr_simplify = \case
  EBinop AssignPlus x1 x2 | x1 == x2 -> Just $ EBinop AssignMultiply x1 (eImm8 $ Imm8 2)
  EBinop binop@(anyCompare -> True) (EBinop Subtract e1 e2) (ELiteral (anyLiteral -> 0)) -> Just $ EBinop binop e1 e2
  EUnop Not (EUnop Not x) -> Just x
  x -> Nothing

pass_simplify :: Pass
pass_simplify = do
  ctx_statements %= map (converge (==) . iterate (replaceSubtree simplify))
  ctx_statements %= map (replaceSubtree deleteEmptyBlocks)
  return ()
  where
    simplify stmt = pure $ stmt & set_expr %~ \e -> fromMaybe e (expr_simplify e)
    deleteEmptyBlocks stmt = case stmt of
      (SBlock anno1 []) -> []
      _ -> [ stmt ]

expandAssignment :: Expression -> Expression
expandAssignment x =
  let table binop = case binop of
        AssignPlus -> Just ePlus
        AssignMinus -> Just eMinus
        AssignMultiply -> Just eMultiply
        AssignDivide -> Just eDivide
        AssignBitXor -> Just eXor
        AssignBitOr -> Just eOr
        AssignBitAnd -> Just eAnd
        AssignBitShiftRight -> Just eShiftRight
        AssignBitShiftLeft -> Just eShiftLeft
        _ -> Nothing
  in case x of
    EBinop (table -> Just binop) a b -> eAssign a (binop a b)
    _ -> x

anyLiteral :: Literal -> Int
anyLiteral (LImm8 (Imm8 x)) = x
anyLiteral (LImm16 (Imm16 x)) = x
anyLiteral (LImm32 (Imm32 x)) = x

pass_fuse2Statements :: Pass
pass_fuse2Statements = do
  ctx_statements %= map (converge (==) . iterate (replaceGroup 2 fuse2Statements))
  return ()
  where
    fuse2Statements stmts = case stmts of
      (SExpression anno1 (EBinop AssignPlus x1 x2): SExpression anno2 (EBinop AssignPlus x3 x4):[]) | x1 == x3 ->
        Just [SExpression anno1 (EBinop AssignPlus x1 (EBinop Plus x2 x4))]
      (SExpression anno1 (EBinop AssignMultiply x1 x2): SExpression anno2 (EBinop AssignMultiply x3 x4):[]) | x1 == x3 ->
        Just [SExpression anno1 (EBinop AssignMultiply x1 (EBinop Multiply x2 x4))]
      (SExpression anno1 (EBinop AssignBitShiftRight x1 x2): SExpression anno2 (EBinop AssignBitShiftRight x3 x4):[]) | x1 == x3 ->
        Just [SExpression anno1 (EBinop AssignBitShiftRight x1 (EBinop Plus x2 x4))]
      (SExpression anno1 (EBinop Assign x1 x2): SExpression anno2 (expandAssignment -> EBinop Assign x3 (EBinop binop x4 x5)):[]) | x1 == x3 && x3 == x4 -> Just [ SExpression anno1 (EBinop Assign x1 (EBinop binop x2 x5)) ]
      (SExpression anno1 e1: SExpression anno2 e2:[]) -> (pure . SExpression anno1) <$> fuse2Expressions [e1,e2]
      _ -> Nothing

    withExprPair :: Expression -> Expression -> (Expression -> Maybe Expression) -> Maybe Expression
    withExprPair x1 x2 f = case (x1, x2) of
      (EUnop Dereference e1, EUnop Dereference e2) -> withExprPair e1 e2 (f . EUnop Dereference)
      (EBinop Plus e1 e2, EBinop Plus e3 e4) | e2 == e4 -> withExprPair e1 e3 (f . (\e -> EBinop Plus e e2))
      (EBinop Plus e1 e2, EBinop Plus e3 e4) | e1 == e3 -> withExprPair e2 e4 (f . (\e -> EBinop Plus e1 e))
      (e1@(ELiteral (anyLiteral -> i1)), ELiteral (anyLiteral -> i2)) | i1 + 1 == i2 -> f e1
      (ELiteral (anyLiteral -> i1), e2@(ELiteral (anyLiteral -> i2))) | i1 == i2 + 1 -> f e2
      (pImm8 -> Just (Imm8 imm8_1), pImm8 -> Just (Imm8 imm8_2)) -> f $ eImm16 $ Imm16 $ imm8_1 + 256 * imm8_2
      (EReg16 r16_1, EBinop Plus (EReg16 r16_2) (ELiteral (anyLiteral -> 1)))| r16_1 == r16_2 -> f $ EReg16 r16_1
      (EReg8 r1, EReg8 r2) -> f =<< withPairEither r1 r2 EReg16
      _ -> Nothing

    fuse2Expressions :: [Expression] -> Maybe Expression
    fuse2Expressions exprs = case exprs of
      (EBinop Assign (EReg8 r1) x1: EBinop Assign (EReg8 r2) x2:[]) ->
        join  $ withPairEither r1 r2 $ \r16 ->
        withExprPair x1 x2 $ \exp ->
        Just $ eAssign (EReg16 r16) exp
      _ -> Nothing

fuseMultibytePtrs :: [ Statement ] -> Maybe [ Statement ]
fuseMultibytePtrs stmts =
  let lift2_reg16 anno r1 r2 r3 r4 binop = withPair r1 r2 $ \r16_1 -> withPair r3 r4 $ \r16_2 ->
        Just $ SExpression anno (EBinop binop (EReg16 r16_1) (EReg16 r16_2))
  in case stmts of
    (SAsm anno1 (iex_astI -> Add r1 r3): SAsm anno2 (iex_astI -> Adc r2 r4):[]) -> pure <$> lift2_reg16 anno1 r1 r2 r3 r4 AssignPlus
    (SAsm anno1 (iex_astI -> Mov r1 r3): SAsm anno2 (iex_astI -> Or r2 r4):[]) -> pure <$> lift2_reg16 anno1 r1 r2 r3 r4 Assign
    (SAsm anno1 (iex_astI -> Eor r1 r3): SAsm anno2 (iex_astI -> Eor r2 r4):[]) -> pure <$> lift2_reg16 anno1 r1 r2 r3 r4 AssignBitXor
    (SAsm anno1 (iex_astI -> Ldxp r1): SAsm anno2 (iex_astI -> Ldx r2):[]) -> withPair r1 r2 $ \r16 ->
      Just [ SExpression anno1 (EBinop Assign (EReg16 r16) (EUnop Dereference (EUnop PostIncrement (EReg16 RX))))]
    (SAsm anno1 (iex_astI -> Ldxp r1): SAsm anno2 (iex_astI -> Ldxp r2):[]) -> withPair r1 r2 $ \r16 ->
      Just [ SExpression anno1 (EBinop Assign (EReg16 r16) (EUnop Dereference (EUnop PostIncrement (EReg16 RX)))),
             SExpression anno1 (EUnop PostIncrement (EReg16 RX))]
    (SAsm anno1 (iex_astI -> Ldyp r1): SAsm anno2 (iex_astI -> Ldy r2):[]) -> withPair r1 r2 $ \r16 ->
      Just [ SExpression anno1 (EBinop Assign (EReg16 r16) (EUnop Dereference (EUnop PostIncrement (EReg16 RY))))]
    (SAsm anno1 (iex_astI -> Ldy r1): SAsm anno2 (iex_astI -> Lddy r2 (Imm8 1)):[]) -> withPair r1 r2 $ \r16 ->
      Just [ SExpression anno1 (EBinop Assign (EReg16 r16) (EUnop Dereference (EReg16 r16))) ]

    _ -> Nothing

anyBranch :: AstInstruction -> Maybe (RelPtr, Expression -> Expression)
anyBranch (Brne relptr) = Just (relptr, id)
anyBranch (Breq relptr) = Just (relptr, EUnop Not)
anyBranch (Brge relptr) = Just (relptr, \e -> EBinop GreaterOrEqual e (eImm8 $ Imm8 0))
anyBranch _ = Nothing

fuse3Instrs :: [ Statement ] -> Maybe [ Statement ]
fuse3Instrs stmts =
  let compare e1 e2 e3 e4 =
        let sub1 = eSub e1 e3
            sub2 = eSub e2 e4
            condition = ePlus sub1 (eShiftLeft sub2 (eImm8 $ Imm8 8))
        in condition
      anyBranchI = anyBranch . iex_astI
      genIfElse annoIf annoBlock relptr condMod condition =
        let (label,goto) = labelAndGotoForRelptr annoBlock relptr
        in Just [ label, SIfElse annoIf (condMod condition) goto (SBlock annoBlock []) ]
  in case stmts of
    (SAsm anno1 (iex_astI -> Cpi r1 imm): SAsm anno2 (iex_astI -> Cpc r2 r3): SAsm anno3 (anyBranchI -> Just (relptr, condMod)):[]) ->
      withPair r1 r2 $ \r16 -> genIfElse anno1 anno3 relptr condMod $ compare (EReg8 r1) (EReg8 r2) (eImm8 imm) (EReg8 r3)
    (SAsm anno1 (iex_astI -> Cp r1 r2): SAsm anno2 (iex_astI -> Cpc r3 r4):SAsm anno3 (anyBranchI -> Just (relptr, condMod)):[]) ->
      withPair r1 r3 $ \r16_1 -> withPair r2 r4 $ \r16_2 ->
      genIfElse anno1 anno3 relptr condMod $ eSub (EReg16 r16_1) (EReg16 r16_2)
    _ -> Nothing
      
fuseRedundantLabels :: [ Statement ] -> Maybe [ Statement ]
fuseRedundantLabels stmts = case stmts of
  (i@(SLabel ann1 (LabelId id1)): SLabel ann2 (LabelId id2): []) | id1 == id2 -> Just [ i ]
  _ -> Nothing

pass_fuseMultibytePtrs :: Pass
pass_fuseMultibytePtrs = do
  ctx_statements %= map (replaceGroup 2 fuseMultibytePtrs)
  return ()

pass_fuseRedundantLabels :: Pass
pass_fuseRedundantLabels = do
  ctx_statements %= map (replaceGroup 2 fuseRedundantLabels)
  return ()

pass_fuse3Instrs :: Pass
pass_fuse3Instrs =do
  ctx_statements %= map (replaceGroup 3 fuse3Instrs)
  return ()

replaceBranchesWithJumps :: [Statement] -> Maybe [ Statement ]
replaceBranchesWithJumps stmts =
  case stmts of
    (SAsm x (iex_astI -> Or r1 r2): SAsm y (iex_astI -> Breq relptr):[]) -> withPair r1 r2 $ \r16 ->
      let (label, goto) = labelAndGotoForRelptr y relptr
      in Just [ label, SIfElse x (EUnop Not $ EReg16 r16) goto (SBlock y []) ]
    (SAsm x (iex_astI -> Or r1 r2): SAsm y (iex_astI -> Brne relptr):[]) -> withPair r1 r2 $ \r16 ->
      let (label, goto) = labelAndGotoForRelptr y relptr
      in Just [ label, SIfElse x (EReg16 r16) goto (SBlock y []) ]
    (SAsm anno instr@(iex_astI -> Sbrs r (BitIdx idx)) : s2 :[]) -> Just [
      SIfElse anno (EUnop Not (EBinop BitAnd (EReg8 r) (ELiteral $ LImm8 $ Imm8 $ 2^idx))) s2 (SBlock anno [])]
    (SAsm anno1 (iex_astI -> And r1 r2): SAsm anno2 (iex_astI -> Breq relptr):[]) | r1 == r2 ->
      let (label, goto) = labelAndGotoForRelptr anno2 relptr
      in Just [ label, SIfElse anno1 (EUnop Not (EReg8 r1)) goto (SBlock anno2 [])]

    _ -> Nothing

extractSubsequence :: (a -> Maybe b) -> (b -> a -> Maybe c) -> [a] -> ([a], (Maybe b, Maybe c, [a]),[a])
extractSubsequence fstart fstop xs =
  let g start middle end mStartS mStopS [] = (reverse start, (mStartS, mStopS, reverse middle), reverse end)
      g start middle end mStartS mStopS (x:xs) =
        case (mStartS, mStopS) of
          (Nothing, Nothing) -> case fstart x of
            Nothing -> g (x:start) middle end mStartS mStopS xs
            Just s -> g start (x:middle) end (Just s) mStopS xs
          (Just startS, Nothing) -> case fstop startS x of
            Nothing -> g start (x:middle) end mStartS mStopS xs
            _ | any isJust $ map (fstop startS) xs -> g start (x:middle) end mStartS mStopS xs
            Just s -> g start (x:middle) end mStartS (Just s) xs
          (Just startS, Just endS) -> g start middle (x:end) mStartS (Just endS) xs
          (Nothing, Just _) -> Prelude.error "extractSubsequence: Impossible case"
  in g [] [] [] Nothing Nothing xs

-- | Given functions that mark the start and end of a subsequence, and a function to replace the subsequence, executes a replace of the entire sequence that changes only the subsequence.
replaceSubsequence::(Statement-> Maybe a)->
                    (a-> Statement-> Maybe b) ->
                    (a -> Maybe b -> [Statement] -> Maybe [Statement]) ->
                    [ Statement ] ->
                    Maybe [Statement]
replaceSubsequence fstart fend ffinalize stmts =
  let (start, (ms_start, ms_end, middle), end) = extractSubsequence fstart fend stmts
  in case ms_start of
    Nothing -> Nothing
    Just s_start -> case ffinalize s_start ms_end middle of
      Nothing -> Nothing
      Just newMiddle -> Just $ start ++ newMiddle ++ end

replaceBlockSubsequence ::(Statement->Maybe a)->(a->Statement->Maybe b)->(a->Maybe b->[Statement]->Maybe [Statement])->[Statement]->[Statement]
replaceBlockSubsequence fstart fend ffinalize stmts = map (replaceSubtree f) stmts
  where
    f stmt = case stmt of
      SBlock _ children -> case replaceSubsequence fstart fend ffinalize children of
        Nothing -> [stmt]
        Just stmts -> stmts
      _ -> [ stmt ]

pass_fuseLabelsAndGotosIntoWhileLoops :: Pass
pass_fuseLabelsAndGotosIntoWhileLoops = do
  ctx_statements %= (replaceBlockSubsequence markLabel markGoto wrapWhile)
  return ()
  where
    markLabel stmt = case stmt of
      SLabel _ labelId -> Just labelId
      _ -> Nothing
    markGoto labelId stmt = case stmt of
      SIfElse anno cond (SGoto _ gotoLabelId) (SBlock _ []) | labelId == gotoLabelId -> Just (anno, cond)
      SGoto anno gotoLabelId | labelId == gotoLabelId -> Just (anno, eImm8 $ Imm8 1)
      _ -> Nothing
    replaceGotosWithContinue labelId stmts = flip replaceSubtree stmts $ \case
      SGoto anno labelId2 | labelId == labelId2 -> [ SContinue (anno & stmtAnno_label .~ Just labelId) ]
      SLabel anno labelId2 | labelId == labelId2 -> []
      stmt -> [ stmt ]
    wrapWhile labelId mEnd stmts = case mEnd of
      Nothing -> Nothing
      Just (anno, cond) -> 
        let newStmts = map (replaceGotosWithContinue labelId) $ init stmts
        in Just [ SWhile anno cond (SBlock anno newStmts)]

pass_fuseLabelsGotosIfsIntoIfBlocks :: Pass
pass_fuseLabelsGotosIfsIntoIfBlocks = do
  ctx_statements %= (replaceBlockSubsequence markIf markLabel wrapIf)
  return ()
  where
    markIf stmt = case stmt of
      SIfElse anno cond (SGoto _ labelId) (SBlock _ []) -> Just (anno, cond, labelId)
      _ -> Nothing
    markLabel (_, _, gotoLabelId) stmt = case stmt of
      label@(SLabel _ labelId) | gotoLabelId == labelId -> Just label
      _ -> Nothing
    wrapIf (anno, cond, labelId) mLabel stmts = case mLabel of
      Nothing -> Nothing
      Just label -> Just [ SIfElse anno (eNot cond) (SBlock anno $ init stmts) (SBlock (label ^. annotation) []), label ]

offsetSymbolTable :: Table Symbol -> M.IntMap Symbol
offsetSymbolTable symbolTable = M.fromList $ map (\(k, sym@(Symbol _ offset)) -> (offset, sym)) $ M.assocs $ symbolTable ^. elements
  

pass_rewriteCallInstructionsToCallSymbols :: Pass
pass_rewriteCallInstructionsToCallSymbols = do
  ctx <- get
  let offsetTable = offsetSymbolTable $ ctx ^. ctx_functions
  ctx_statements %= map (replaceSubtree (rewriteCalls offsetTable))
  return ()
  where
    rewriteCalls offsetTable stmt = case stmt of
      SAsm anno (iex_astI -> Call (Imm32 offset)) -> case M.lookup offset offsetTable of
        Nothing -> [ stmt ] 
        Just sym -> [ SExpression anno $ ECall (ESymbol sym) [] ]
      _ -> [ stmt ]
