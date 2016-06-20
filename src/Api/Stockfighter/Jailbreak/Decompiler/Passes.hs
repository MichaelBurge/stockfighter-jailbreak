module Api.Stockfighter.Jailbreak.Decompiler.Passes where

import Api.Stockfighter.Jailbreak.Decompiler.AST
import Api.Stockfighter.Jailbreak.Types

import Control.Monad.Trans.State.Strict
import Control.Lens hiding (Context)
import Data.List
import Data.Maybe

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
        in (newIndexBase + i, sym, instructions)
  let newFunctions = M.fromList $ map (\(a,b,c) -> (a,b)) split_groups
  let statements = flip map split_groups $ \(idx, sym, instructions) ->
        SFunction TVoid sym [] [ SAsm instructions ] 
  ctx_functions <>= Table newFunctions (M.size newFunctions)
  ctx_assembly .= [] -- No more free-floating assembly
  return statements
