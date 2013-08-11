{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}

module Estimator ( Estimator
                   -- * estimator symbols
                 , state
                 , stateV3
                 , process
                 , processV3
                 , processNoise
                 , processNoiseV3
                 , constant
                 , (@=)
                   -- * propogate
                 , setNext
                 , setNextV3
                   -- * building
                 , buildEstimator
                 , estimatorSummary
                 ) where

import Control.Lens ( (^.), makeLenses )
import qualified Data.Map as M

import Classes
import Linear ( V3(..) )
import Builder
import LogsAndErrors

data EstimatorState = EstimatorState { _eSymbols :: M.Map EstimatorSymbols (M.Map String Expr)
                                     , _eIntermediateState :: M.Map String Expr
                                     }
data EstimatorSymbols = State | Process | ProcessNoise | Constant deriving (Eq,Enum,Ord,Show)

makeLenses ''EstimatorState

instance Symbols EstimatorState EstimatorSymbols where
  symbols _ = eSymbols

instance IntermediateStates EstimatorState where
  intermediateStates _ = eIntermediateState

type Estimator a = Builder EstimatorState a

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

emptyEstimator :: EstimatorState
emptyEstimator = EstimatorState { _eSymbols = emptySymbols (emptyEstimator :: EstimatorState)
                                , _eIntermediateState = M.empty
                                }

buildEstimator :: Estimator a -> (Either ErrorMessage a, [LogMessage], EstimatorState)
buildEstimator = build emptyEstimator

estimatorSummary :: Estimator a -> IO ()
estimatorSummary est = do
  let (result, messages, x) = buildEstimator est
  showLog messages
  putStr "\nresult: "
  case result of
    Left (ErrorMessage msg) -> putStrLn $ "Failure: " ++ msg
    Right _ -> do
      putStrLn "Success!"
      putStrLn ""
      showSymbols (x ^. (symbols x))
      putStrLn ""
      showIntermediateStates (x ^. (intermediateStates x))

