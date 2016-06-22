{-# LANGUAGE RecordWildCards,DeriveDataTypeable #-}

module Api.Stockfighter.Jailbreak.Decompiler.Print where

import Api.Stockfighter.Jailbreak.Types
import Api.Stockfighter.Jailbreak.Decompiler.AST

import Control.Lens hiding (elements)
import Control.Monad.Trans.Reader
import Data.Char
import Text.PrettyPrint as PP

import qualified Data.Text as T

block :: [Doc] -> Doc
block inners = lbrace $+$ hang empty 4 (sep $ punctuate semi inners) $+$ rbrace

instance PrintAst Symbol where
  printNode x = return $ PP.text $ T.unpack $ x ^. sym_symbol 

instance PrintAst VariableId where
  printNode (VariableId varId) = do
    context <- ask
    printNode $ context ^. ctx_variables ^. elements ^. at varId

instance PrintAst LabelId where
  printNode (LabelId id) = return $ PP.text "label" <> PP.int id

instance PrintAst Imm8 where
  printNode (Imm8 x) = return $ PP.int x
  
instance PrintAst Imm16 where
  printNode (Imm16 x) = return $ PP.int x

instance PrintAst Imm32 where
  printNode (Imm32 x) = return $ PP.int x

instance PrintAst Unop where
  printNode x = case x of
    Negate -> return $ PP.text "-"
    Not -> return $ PP.text "!"
    Dereference -> return $ PP.text "*"
    PostIncrement -> return $ PP.text "++"
    PreIncrement -> return $ PP.text "++"

instance PrintAst Binop where
  printNode x = return $ PP.text $ case x of
    Plus -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    Mod -> "%"
    BitAnd -> "&"
    BitOr -> "|"
    BitXor -> "^"
    BitShiftRight -> ">>"
    BitShiftLeft -> "<<"
    Assign -> "="
    AssignPlus -> "+="
    AssignMinus -> "-="
    AssignMultiply -> "*="
    AssignDivide -> "/="
    AssignBitXor -> "^="
    AssignBitAnd -> "&="
    AssignBitOr -> "|="
    AssignBitShiftRight -> ">>="
    AssignBitShiftLeft -> "<<="


instance PrintAst Register where
  printNode (Register x) = return $ PP.text $ "r" ++ show x

instance PrintAst Reg16 where
  printNode x = return $ PP.text $ show x

instance PrintAst Literal where
  printNode x = case x of
    LImm8 x -> printNode x
    LImm16 x -> printNode x
    LImm32 x -> printNode x

isAssignment Assign = True
isAssignment AssignPlus = True
isAssignment AssignMinus = True
isAssignment AssignBitXor = True
isAssignment AssignBitAnd = True
isAssignment AssignBitOr = True
isAssignment AssignBitShiftRight = True
isAssignment AssignBitShiftLeft = True
isAssignment _ = False

shouldParenthesize :: Expression -> Bool
shouldParenthesize (EUnop _ (EBinop _ _ _)) = True
shouldParenthesize (EBinop x _ _) | isAssignment x = False
shouldParenthesize (EBinop _ (EBinop _ _ _) _) = True
shouldParenthesize (EBinop _ _ (EBinop _ _ _)) = True
shouldParenthesize _ = False

instance PrintAst Expression where
  printNode x =
    let maybeParenthesize e =
          if shouldParenthesize x
          then parens e
          else e
        result = case x of
          ELiteral y -> printNode y
          EUnop unop exp -> do
            a <- printNode unop
            b <- printNode exp
            case unop of
              PostIncrement -> return $ maybeParenthesize b <> a
              _ -> return $ a <> maybeParenthesize b
          EBinop binop exp1 exp2 -> do
            a <- printNode exp1
            b <- printNode binop
            c <- printNode exp2
            return $ maybeParenthesize a <+> b <+> maybeParenthesize c
          ECall symbol args -> do
            a <- printNode symbol
            xs <- mapM printNode args
            return $ a <> parens ( mconcat $ punctuate comma xs )
          EReg8 r8 -> printNode r8
          EReg16 r16 -> printNode r16
          ESymbol (Symbol x _) -> return $ PP.text $ T.unpack x
    in result

instance PrintAst Instruction where
  printNode = \Instruction{..} -> do
    let showI = PP.text $ show offset ++ ":\t\t" ++ T.unpack dump
    case symbol of
      Nothing -> return showI
      Just x -> do
        return $ (PP.text $ T.unpack x) $+$ showI
      
instance PrintAst (StatementEx a) where
  printNode x = case x of
    SExpression _ x -> do
      a <- printNode x
      return $ a
    SLabel _ x -> printNode x
    SGoto _ varId -> do
      a <- printNode varId
      return $ PP.text "goto" <+> a
    SAsm _ iex -> do
      a <- printNode iex
      return $ PP.text "asm" <+> a
    SVariable _ varId ty mInitializer -> do
      a <- printNode ty
      b <- printNode varId
      c <- case mInitializer of
        Nothing -> return empty
        Just x -> do
          xn <- printNode x
          return $ PP.text "=" <+> xn
      return $ a <+> b <+> c
    SBlock _ xs -> do
      as <- mapM printNode xs
      return $ block as
    SFunction _ ty sym args body -> do
      a <- printNode ty
      b <- printNode sym
      cs <- mapM printNode args
      d <- printNode body
      return $ sep [ a <+> b <> parens (mconcat $ punctuate comma cs),  d ]
    SIfElse _ cond x y -> do
      a <- printNode cond
      b <- printNode x
      let ifPart = PP.text "if" <+> parens a $+$ hang empty 4 b
      case y of
        SBlock _ [] -> do
          return ifPart
        _ -> do
          c <- printNode y
          return $ ifPart <+> PP.text "else" <+> c
    SReturn _ x -> do
      case x of
        Nothing -> return $ PP.text "return"
        Just x -> do
          a <- printNode x
          return $ PP.text "return" <+> a
    SWhile _ cond stmt -> do
      a <- printNode cond
      b <- printNode stmt
      return $ PP.text "while" <+> parens a $+$ hang empty 4 b
    SContinue _ -> return $ PP.text "continue"

instance PrintAst a => PrintAst (Maybe a) where
  printNode Nothing = return empty
  printNode (Just x) = printNode x

instance PrintAst Type where
  printNode x = return $ PP.text $ case x of
    TVoid -> "void"
    TInt -> "int"
    TIntPtr -> "int *"
    TChar -> "char"
    TCharPtr -> "char *"

instance PrintAst AstInstruction where
  printNode x = return $ PP.text $ (\(cons, rest) -> map toLower cons ++ rest) $ span (/= ' ') $ show x

instance PrintAst InstructionEx where
  printNode (InstructionEx _ x) = printNode x
