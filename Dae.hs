{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}

module Dae ( Dae
             -- * dae symbols
           , diffState, algVar, control, parameter, constant
           , (===)
           , (@=)
           , buildDae
           , daeSummary
           ) where

import Control.Lens ( (^.), makeLenses )
import qualified Data.Map as M

import Builder
import Classes
import LogsAndErrors

data DaeSymbols = DifferentialState
                | DifferentialStateDeriv
                | Control
                | AlgebraicVar
                | Parameter
                | Constant
                deriving ( Show, Ord, Eq, Enum )

data DaeState = DaeState { _daeSymbols :: M.Map DaeSymbols (M.Map String Expr)
                         , _daeIntermediateState :: M.Map String Expr
                         , _daeImplicitEquations :: [(Expr,Expr)]
                         }
makeLenses ''DaeState
instance Symbols DaeState DaeSymbols where
  symbols _ = daeSymbols
instance IntermediateStates DaeState where
  intermediateStates _ = daeIntermediateState
instance ImplicitEquations DaeState where
  implicitEquations _ = daeImplicitEquations

type Dae a = Builder DaeState a

diffState, algVar, control, parameter, constant :: String -> Dae Expr
diffState = addSym DifferentialState
algVar    = addSym AlgebraicVar
control   = addSym Control
parameter = addSym Parameter
constant  = addSym Constant

--ddt :: String -> Dae Expr
--ddt name = addSym DifferentialStateDeriv "ddt(" ++ name ++ ")"

emptyDae :: DaeState
emptyDae = DaeState (emptySymbols emptyDae) M.empty []

buildDae :: Dae a -> (Either ErrorMessage a, [LogMessage], DaeState)
buildDae = build emptyDae

daeSummary :: Dae a -> IO ()
daeSummary dae = do
  let (result, messages, x) = buildDae dae
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
      putStrLn ""
      showImplicitEquations (x ^. (implicitEquations x))
