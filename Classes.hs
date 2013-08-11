{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleContexts #-}

module Classes ( Symbols (..)
               , IntermediateStates (..)
               , ImplicitEquations (..)
               , getAllSyms
               , getAllSymbolNames
               , emptySymbols
               , symbolElement
               ) where

import Control.Lens ( (^.) )
import Control.Lens.Type ( Lens' )
import Control.Monad.Error ( MonadError )
import Control.Monad.State ( MonadState, get )
import Control.Monad.Writer ( MonadWriter )
import qualified Data.Map as M
import qualified Data.Set as S

import Expr ( Expr )
import LogsAndErrors ( ErrorMessage, LogMessage, impossible )

class (Ord b, Enum b) => Symbols a b | a -> b where
  symbols :: a -> Lens' a (M.Map b (M.Map String Expr))

class IntermediateStates a where
  intermediateStates :: a -> Lens' a (M.Map String Expr)

class ImplicitEquations a where
  implicitEquations :: a -> Lens' a [(Expr, Expr)]

getAllSyms :: (MonadState s (m s), Symbols s a) => m s (M.Map a (M.Map String Expr))
getAllSyms = do
  b <- get
  return (b ^. (symbols b))

getAllSymbolNames :: (MonadState s (m s), Symbols s a) =>
                     m s (S.Set String)
getAllSymbolNames = do
  allSyms <- getAllSyms
  return $ ((S.unions . (map M.keysSet) . M.elems)) allSyms

emptySymbols :: Symbols s a => s -> M.Map a (M.Map String Expr)
emptySymbols _ = M.fromList $ map (\x -> (x,M.empty)) $ enumFrom (toEnum 0)

symbolElement :: (Show a, MonadError ErrorMessage m, MonadWriter [LogMessage] m,
                  MonadState t m, Symbols t a) =>
                 a -> Expr -> m Bool
symbolElement overMe sym = do
  x <- get
  let syms = x ^. (symbols x)
  case M.lookup overMe syms of
    Nothing -> impossible $ "lookupSymbol: " ++ show overMe ++ " not in symbol map"
    Just m -> if sym `elem` (M.elems m) then return True else return False
