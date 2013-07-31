{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}

module DaeBuilder ( DaeBuilder (..)
                  , (===)
                  , buildDae
                    -- * dae symbols
                  , diffState, algVar, control, parameter, constant
                    -- * example:
                  , someDae
                  , summary
                  ) where

import Control.Lens ( makeLenses, over, (^.) )
import Control.Lens.Setter ( Setting )
import Control.Monad ( when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState, get, put )
import Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ratio ( numerator, denominator )

import LogsAndErrors

--- symbolics for the DaeBuilder ---
data DaeExpr = DaeExprSym String
             | DaeExprDdt String
             | DaeExprInt Integer
             | DaeExprPlus DaeExpr DaeExpr
             | DaeExprMinus DaeExpr DaeExpr
             | DaeExprTimes DaeExpr DaeExpr
             | DaeExprNegate DaeExpr
             | DaeExprDiv DaeExpr DaeExpr
             deriving Show
instance Num DaeExpr where
  (+) = DaeExprPlus
  (-) = DaeExprMinus
  (*) = DaeExprTimes
  abs = error "absolute value not supported"
  signum = error "signum not supported"
  fromInteger = DaeExprInt
  negate = DaeExprNegate
instance Fractional DaeExpr where
  (/) = DaeExprDiv
  fromRational x = DaeExprInt (numerator x) / DaeExprInt (denominator x)

-- | the monad for building up a Dae
data DaeBuilderState = DaeBuilderState
                       { _dbX    :: M.Map String DaeExpr
                       , _dbU    :: M.Map String DaeExpr
                       , _dbZ    :: M.Map String DaeExpr
                       , _dbP    :: M.Map String DaeExpr
                       , _dbC    :: M.Map String DaeExpr
                       , _dbImplicitEq :: [(DaeExpr,DaeExpr)]
                       } deriving Show
makeLenses ''DaeBuilderState

emptyDaeBuilder :: DaeBuilderState
emptyDaeBuilder = DaeBuilderState M.empty M.empty M.empty M.empty M.empty []

newtype DaeBuilder a =
  DaeBuilder
  { runDaeBuilder :: ErrorT ErrorMessage (WriterT [LogMessage] (State DaeBuilderState)) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState DaeBuilderState
             , MonadWriter [LogMessage]
             )

buildDae :: DaeBuilder a -> (Either ErrorMessage a, [LogMessage], DaeBuilderState)
buildDae daeBuilder = (result, logs, state)
  where
    ((result,logs),state) =
      flip runState emptyDaeBuilder . runWriterT . runErrorT . runDaeBuilder $ daeBuilder

daeSummary :: String -> DaeBuilderState -> IO ()
daeSummary prefix dae = do
  let blah name overMe = putStrLn $ prefix ++ show (length keys) ++ " " ++ name ++ " " ++ show keys
        where
          keys = M.keys (dae ^. overMe)
  blah "diff states" dbX
  blah "alg vars   " dbZ
  blah "controls   " dbU
  blah "parameters " dbP
  blah "constants  " dbC
  let ieqs = dae ^. dbImplicitEq
  putStrLn $ prefix ++ show (length ieqs) ++ " implicit equations: " ++ withEllipse 70 (show ieqs)
  
summary :: DaeBuilder a -> IO ()
summary daeBuilder = do
  let (result, messages, dae) = buildDae daeBuilder
      (n0,n1,n2,n3) = countLogs messages
  putStrLn $ "log: (" ++ show n0 ++ " messages, "++show n1++" warnings, "++show n2++" errors, "++show n3++" \"impossible\" errors)"
  mapM_ (putStrLn . ("  " ++) . show) messages
  putStr "\nresult: "
  case result of Left (ErrorMessage x) -> putStrLn $ "Failure: " ++ x
                 Right _ -> putStrLn "Success!" >> daeSummary "  " dae

withEllipse :: Int -> String -> String
withEllipse n blah
  | length blah <= n = blah
  | otherwise = take n blah ++ "..."

infix 4 ===
(===) :: DaeExpr -> DaeExpr -> DaeBuilder ()
(===) lhs rhs = do
  debug $ "adding implicit equation: " ++
    withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ over dbImplicitEq ((lhs,rhs):) state0

getAllNames :: DaeBuilder (S.Set String)
getAllNames = do
  state <- get
  return $ S.unions [ M.keysSet $ _dbX state
                    , M.keysSet $ _dbZ state
                    , M.keysSet $ _dbU state
                    , M.keysSet $ _dbP state
                    , M.keysSet $ _dbC state
                    ]

assertUniqueName :: String -> DaeBuilder ()
assertUniqueName name = do
  allNames <- getAllNames
  when (S.member name allNames) (err $ "in DaeBuilder, \"" ++ name ++ "\" is not a unique name")

addDaeSym :: Control.Lens.Setter.Setting (->) DaeBuilderState DaeBuilderState
             (M.Map String DaeExpr) (M.Map String DaeExpr)
          -> String -> String -> DaeBuilder DaeExpr
addDaeSym overMe descriptor name = do
  debug $ "adding "++descriptor++": "++name
  assertUniqueName name
  state0 <- get
  let sym = DaeExprSym name
  put $ over overMe (\m0 -> M.insert name sym m0) state0
  return sym
  
diffState, algVar, control, parameter, constant :: String -> DaeBuilder DaeExpr
diffState = addDaeSym dbX "differential state"
algVar    = addDaeSym dbZ "algebraic variable"
control   = addDaeSym dbU "control"
parameter = addDaeSym dbP "parameter"
constant  = addDaeSym dbC "constant"

ddt :: String -> DaeExpr
ddt = DaeExprDdt

someDae :: DaeBuilder ()
someDae = do
  _    <- diffState "pos"
  vel  <- diffState "vel"
  mass <- diffState "mass"

  thrust <- control "thrust"

  k <- constant "viscous damping"
  -- parameter "pos" -- causes an error

  let acceleration = thrust/mass - vel*k

  ddt "pos" === vel
  ddt "vel" === acceleration
  ddt "mass" === -0.8*thrust*thrust

go :: IO ()
go = summary someDae
