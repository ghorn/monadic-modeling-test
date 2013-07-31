{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module Expr ( Expr (..)
            , Dim (..)
            ) where

import GHC.Generics ( Generic )
import Data.Hashable

data Dim = Dim Int Int deriving (Eq, Generic, Show)
instance Hashable Dim

data Expr = Sym Dim String
          | Add Expr Expr
          | Mul Expr Expr
          | Neg Expr
          | Abs Expr
          | Integer Int
          deriving (Eq, Generic, Show)

instance Num Expr where
  (+) = Add
  (*) = Mul
  abs = Abs
  negate = Neg
  fromInteger = Integer . fromIntegral
  signum = error "signum not supported, yo"

instance Hashable Expr
