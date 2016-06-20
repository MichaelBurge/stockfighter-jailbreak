{-# LANGUAGE RecordWildCards,DeriveDataTypeable #-}

module Api.Stockfighter.Jailbreak.Decompiler.Print where

import Api.Stockfighter.Jailbreak.Types
import Api.Stockfighter.Jailbreak.Decompiler.AST

import Control.Lens hiding (elements)
import Control.Monad.Trans.Reader
import Data.Char
import Text.PrettyPrint as PP

import qualified Data.Text as T

block :: Doc -> Doc -> Doc
block intro inner = hang (intro <+> lbrace) 4 inner $+$ rbrace

instance PrintAst Symbol where
  printNode x = return $ PP.text $ T.unpack $ x ^. sym_symbol 

instance PrintAst VariableId where
  printNode (VariableId varId) = do
    context <- ask
    printNode $ context ^. ctx_variables ^. elements ^. at varId

instance PrintAst Unop where
  printNode x = case x of
    Negate -> return $ PP.text "-"

instance PrintAst Binop where
  printNode x = return $ PP.text $ case x of
    Plus -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide -> "/"
    Mod -> "%"

instance PrintAst Expression where
  printNode x = case x of
    ELiteral y -> return $ int y
    EUnop unop exp -> do
      a <- printNode unop
      b <- printNode exp
      return $ parens $ a <> b
    EBinop binop exp1 exp2 -> do
      a <- printNode exp1
      b <- printNode binop
      c <- printNode exp2
      return $ parens $ a <> b <> c
    ECall symbol args -> do
      a <- printNode symbol
      xs <- mapM printNode args
      return $ a <> parens ( mconcat $ punctuate comma xs )

instance PrintAst Instruction where
  printNode = \Instruction{..} -> do
    case symbol of
      Nothing -> do
        return $ PP.text $ show offset ++ ":\t\t" ++ T.unpack dump
      Just x -> return $ PP.text $ T.unpack x
      
instance PrintAst Statement where
  printNode x = case x of
    SAssign varId initializer -> do
      a <- printNode varId
      b <- printNode initializer
      return $ a <+> PP.text "=" <+> b
    SGoto varId -> do
      a <- printNode varId
      return $ PP.text "goto" <+> a
    SAsm xs -> do
      asms <- mapM printNode xs
      return $ block (PP.text "asm") $ sep (punctuate semi asms)
    SVariable varId ty mInitializer -> do
      a <- printNode ty
      b <- printNode varId
      c <- case mInitializer of
        Nothing -> return empty
        Just x -> do
          xn <- printNode x
          return $ PP.text "=" <+> xn
      return $ a <+> b <+> c
    SFunction ty sym args body -> do
      a <- printNode ty
      b <- printNode sym
      cs <- mapM printNode args
      ds <- mapM printNode body
      return $ block (a <+> b <> parens (mconcat $ punctuate comma cs)) $ sep (punctuate semi ds)

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
  printNode x = return $ PP.text $ map toLower $ show x
