module Api.Stockfighter.Jailbreak.Decompiler (
  module Api.Stockfighter.Jailbreak.Decompiler,
  module Api.Stockfighter.Jailbreak.Decompiler.AST,
  module Api.Stockfighter.Jailbreak.Decompiler.Passes,
  module Api.Stockfighter.Jailbreak.Decompiler.Print
  ) where

import Api.Stockfighter.Jailbreak.Decompiler.AST
import Api.Stockfighter.Jailbreak.Decompiler.Passes
import Api.Stockfighter.Jailbreak.Decompiler.Print
import Api.Stockfighter.Jailbreak.Types

import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Text.PrettyPrint as PP

decompile :: [ Instruction ] -> IO Context
decompile instructions =
  let iexs = map (\i -> InstructionEx i (parseInstruction i)) instructions
      context = mempty { _ctx_statements = map iex_asm iexs }
  in flip execStateT context $ do
    -- Instruction-level passes
    pass_groupBySymbol
    
    fixPass $ do
      pass_rewriteCallInstructionsToCallSymbols
      pass_fuse3Instrs
      pass_replaceLocalJumpsWithGotos
      pass_fuseMultibytePtrs
      pass_replaceBranchesWithJumps
      pass_replaceSingleInstructions

    -- Statement-level passes
    fixPass $ do
      pass_simplify
      pass_fuseRedundantLabels
      pass_fuse2Statements

    -- Block-level passes
    fixPass $ do
      pass_convertPushesAndPopsIntoVariables
      pass_fuseLabelsAndGotosIntoWhileLoops
      pass_fuseLabelsGotosIfsIntoIfBlocks
      pass_simplify

print_ast :: [ Statement ] -> IO ()
print_ast statements = do
  let result = flip runReader mempty $ sep <$> forM statements printNode
  putStrLn $ render result
  
