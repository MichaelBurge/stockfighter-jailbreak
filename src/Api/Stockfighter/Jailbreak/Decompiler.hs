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

decompile :: [ Instruction ] -> IO ([ Statement ], Context)
decompile instructions = flip runStateT (mempty { _ctx_assembly = instructions }) $ do
  groupBySymbol

print_ast :: [ Statement ] -> IO ()
print_ast statements = do
  let result = flip runReader mempty $ sep <$> forM statements printNode
  putStrLn $ render result
  
