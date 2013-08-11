{-# OPTIONS_GHC -Wall #-}

module Expr ( Expr
            , newSym
            ) where

import qualified Dvda

type Expr = Dvda.Expr Double

newSym :: String -> Expr
newSym = Dvda.sym
