{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Builder ( Builder
               , Expr
               , (===)
               , (@=)
               , addSym
               , build
                 -- * summaries
               , showLog
               , showSymbols
               , showIntermediateStates
               , showImplicitEquations
               ) where

import Control.Lens ( (^.), over )
import Control.Monad ( when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState, get, put )
import Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Map as M
import qualified Data.Set as S

import Classes
import Expr
import LogsAndErrors
import Utils ( withEllipse )

newtype Builder s a =
  Builder
  { runBuilder :: ErrorT ErrorMessage (WriterT [LogMessage] (State s)) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState s
             , MonadWriter [LogMessage]
             )

build :: s -> Builder s a -> (Either ErrorMessage a, [LogMessage], s)
build emptyBuilder builder = (result, logs, state)
  where
    ((result,logs),state) =
      flip runState emptyBuilder . runWriterT . runErrorT . runBuilder $ builder

assertUniqueIntermediateStateName :: IntermediateStates s => String -> Builder s ()
assertUniqueIntermediateStateName name = do
  s <- get
  when (M.member name (s ^. intermediateStates s)) $
    err $ name ++ " is not a unique intermediate state name"

assertUniqueName :: (IntermediateStates s, Symbols s a) => String -> Builder s ()
assertUniqueName name = do
  assertUniqueSymbolName name
  assertUniqueIntermediateStateName name

assertUniqueSymbolName :: Symbols s a => String -> Builder s ()
assertUniqueSymbolName name = do
  allSymbolNames <- getAllSymbolNames
  when (S.member name allSymbolNames) $
    err $ "in Builder, \"" ++ name ++ "\" is not a unique symbol name"

infix 4 @=
(@=) :: (Symbols s a, IntermediateStates s) => Expr -> String -> Builder s ()
(@=) expr name = do
  debug $ "setting intermediate state: " ++ name ++ " = " ++
    withEllipse 30 (show expr)
  assertUniqueName name
  b <- get
  put $ over (intermediateStates b) (M.insert name expr) b

infix 4 ===
(===) :: ImplicitEquations s => Expr -> Expr -> Builder s ()
(===) lhs rhs = do
  debug $ "adding implicit equation: " ++
    withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ over (implicitEquations state0) ((lhs,rhs):) state0

addSym :: (Show a, Symbols s a) => a -> String -> Builder s Expr
addSym overMe name = do
  let descriptor = show overMe
  debug $ "adding "++descriptor++": "++name
  assertUniqueSymbolName name
  b <- get
  let sym = newSym name
      map0 = b ^. symbols b
  when (M.notMember overMe map0) $ impossible $ descriptor ++ " not in symbol map"
  put $ over (symbols b) (M.adjust (M.insert name sym) overMe) b
  return sym

showSymbols :: Show s => M.Map s (M.Map String Expr) -> IO ()
showSymbols blahs = do
  putStrLn "symbols: "
  mapM_ (\(key,vals) -> putStrLn $ "  " ++ show key ++ ": " ++ show (M.keys vals)) $ M.toList blahs

showIntermediateStates :: M.Map String Expr -> IO ()
showIntermediateStates is = do
  putStrLn $ "intermediate states: (" ++ show (M.size is) ++ ")"
  mapM_ (\(key,val) -> putStrLn $ "  " ++ key ++ ": " ++ withEllipse 40 (show val)) $ M.toList is

showImplicitEquations :: [(Expr,Expr)] -> IO ()
showImplicitEquations ieqs = do
  putStrLn $ "implicit equations: (" ++ show (length ieqs) ++ ")"
  mapM_ (\(lhs,rhs) -> putStrLn ("  " ++ withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs))) ieqs

showLog :: [LogMessage] -> IO ()
showLog messages = do
  let (n0,n1,n2,n3) = countLogs messages
  putStrLn $ "log: (" ++ show n0 ++ " messages, "++show n1++" warnings, "++show n2++" errors, "++show n3++" \"impossible\" errors)"
  mapM_ (putStrLn . ("  " ++) . show) messages
