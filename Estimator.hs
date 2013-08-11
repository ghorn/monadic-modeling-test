{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}

module Estimator ( Estimator
                 , state
                 , stateV3
                 , process
                 , processV3
                 , processNoise
                 , processNoiseV3
                 , constant
                 , setNext
                 , setNextV3
                 ) where

import Linear
import Builder
import LogsAndErrors ( err )

data EstimatorSymbols = State | Process | ProcessNoise | Constant deriving (Eq,Enum,Ord,Show)

type Estimator a = Builder EstimatorSymbols a

setNext :: Expr -> Expr -> Estimator ()
setNext _ _ = err "setNext is not implemented"

setNextV3 :: V3 Expr -> V3 Expr -> Estimator ()
setNextV3 (V3 x0 y0 z0) (V3 x1 y1 z1) = do
  setNext x0 x1
  setNext y0 y1
  setNext z0 z1

state :: String -> Estimator Expr
state = addSym State

process :: String -> Estimator Expr
process = addSym Process

processNoise :: String -> Estimator Expr
processNoise = addSym ProcessNoise

constant :: String -> Estimator Expr
constant = addSym Constant

vecSym :: (String -> Estimator Expr) -> String -> Estimator (V3 Expr)
vecSym getter name = do
  x <- getter $ name ++ "_x"
  y <- getter $ name ++ "_y"
  z <- getter $ name ++ "_z"
  return (V3 x y z)

stateV3,processV3,processNoiseV3 :: String -> Estimator (V3 Expr)
stateV3 = vecSym state
processV3 = vecSym process
processNoiseV3 = vecSym processNoise
