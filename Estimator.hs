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

import Control.Lens ( (^.), makeLenses, over )
import Control.Monad ( unless, when )
import Control.Monad.State ( get, put )
import Data.Function ( on )
import Data.List ( sortBy )
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as HM

import qualified Dvda.Expr ( Expr (ESym) )
import Classes
import Linear ( V3(..) )
import Builder
import LogsAndErrors
import Utils ( withEllipse )

data EstimatorState = EstimatorState { _eSymbols :: M.Map EstimatorSymbols (M.Map String Expr)
                                     , _eIntermediateState :: M.Map String Expr
                                     , _eNext :: HM.HashMap Expr Expr
                                     }
data EstimatorSymbols = State | Process | ProcessNoise | Constant deriving (Eq,Enum,Ord,Show)

makeLenses ''EstimatorState

instance Symbols EstimatorState EstimatorSymbols where
  symbols _ = eSymbols

instance IntermediateStates EstimatorState where
  intermediateStates _ = eIntermediateState

type Estimator a = Builder EstimatorState a

setNext :: Expr -> Expr -> Estimator ()
setNext sym@(Dvda.Expr.ESym _) val = do
  iselem <- symbolElement State sym
  unless iselem $ err $ "setNext: "++show sym++" is not a " ++ show State
  x <- get
  when (HM.member sym (x ^. eNext)) $
    err $ "setNext: you tried to set " ++ show sym ++ " twice"
  put $ over eNext (HM.insert sym val) x
setNext other _ =
  err $ "setNext tried to set something which is not a symbol: " ++ show other

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
                                , _eNext = HM.empty
                                }

buildEstimator :: Estimator a -> (Either ErrorMessage a, [LogMessage], EstimatorState)
buildEstimator = build emptyEstimator

showNext :: HM.HashMap Expr Expr -> IO ()
showNext hm = do
  let blahs = sortBy (compare `on` show) $ HM.toList hm
  putStrLn "next value: "
  mapM_ (\(key, val) -> putStrLn $ show key ++ ": " ++ withEllipse 40 (show val)) blahs

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
      showSymbols (x ^. symbols x)
      putStrLn ""
      showIntermediateStates (x ^. intermediateStates x)
      putStrLn ""
      showNext (x ^. eNext)
