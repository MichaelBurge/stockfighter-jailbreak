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
    pass_groupBySymbol
    pass_replaceLocalJumpsWithGotos
    pass_replaceBranchesWithJumps
    pass_replaceSingleInstructions

print_ast :: [ Statement ] -> IO ()
print_ast statements = do
  let result = flip runReader mempty $ sep <$> forM statements printNode
  putStrLn $ render result
  
