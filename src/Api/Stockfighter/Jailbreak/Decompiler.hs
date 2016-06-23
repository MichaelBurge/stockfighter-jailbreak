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
import Control.Monad.IO.Class
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
    
    fixPass "ilevel" $ do
      pass_rewriteCallInstructionsToCallSymbols
      pass_fuse3Instrs
      pass_replaceLocalJumpsWithGotos
      pass_fuseMultibytePtrs
      pass_replaceBranchesWithJumps
      pass_replaceSingleInstructions

    -- Statement-level passes
    fixPass "slevel" $ do
      pass_simplify
      pass_fuseRedundantLabels
      pass_fuse2Statements
      pass_reorderStatements
      -- stmts <- _ctx_statements <$> get
      -- liftIO $ print_ast stmts

    -- Block-level passes
    fixPass "blevel" $ do
      fixPass "pushpop" pass_convertPushesAndPopsIntoVariables
      fixPass "while" pass_fuseLabelsAndGotosIntoWhileLoops
      fixPass "ifblocks" pass_fuseLabelsGotosIfsIntoIfBlocks
      fixPass "simplify" pass_simplify

print_ast :: [ Statement ] -> IO ()
print_ast statements = do
  let result = flip runReader mempty $ sep <$> forM statements printNode
  putStrLn $ render result
  
