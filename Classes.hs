{-# OPTIONS_GHC -Wall #-}
{-# Language Rank2Types #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}

module Classes ( Symbols (..)
               , IntermediateStates (..)
               , ImplicitEquations (..)
               , getAllSyms
               , getAllSymbolNames
               , emptySymbols
               ) where

import Control.Lens ( (^.) )
import Control.Lens.Type ( Lens' )
import qualified Data.Map as M
import Control.Monad.State ( MonadState, get )
import qualified Data.Set as S

import Expr ( Expr )

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
