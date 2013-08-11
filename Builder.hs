{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Builder ( Builder
               , Expr
               , (===)
               , (@=)
               , addSym
               , build
               , summary
               ) where

import Control.Lens ( (^.), makeLenses, over )
import Control.Monad ( when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState, get, put )
import Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Dvda

import LogsAndErrors
import Utils ( withEllipse )

type Expr = Dvda.Expr Double

data BuilderState syms = BuilderState { _bSyms :: M.Map syms (M.Map String Expr)
                                      , _bIntermediateStates :: M.Map String Expr
                                      , _bImplicitEqs :: [(Expr,Expr)]
                                      }
makeLenses ''BuilderState

newtype Builder syms a =
  Builder
  { runBuilder :: ErrorT ErrorMessage (WriterT [LogMessage] (State (BuilderState syms))) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState (BuilderState syms)
             , MonadWriter [LogMessage]
             )

build :: (Enum s, Ord s) => Builder s a -> (Either ErrorMessage a, [LogMessage], BuilderState s)
build builder = (result, logs, state)
  where
    emptySyms = M.fromList $ map (\x -> (x,M.empty)) $ enumFrom (toEnum 0)
    emptyBuilder = BuilderState emptySyms M.empty []
    ((result,logs),state) =
      flip runState emptyBuilder . runWriterT . runErrorT . runBuilder $ builder

getAllSyms :: Builder s (M.Map s (M.Map String Expr))
getAllSyms = do
  b <- get
  return (b ^. bSyms)

getAllNames :: Builder s (S.Set String)
getAllNames = do
  allSyms <- getAllSyms
  b <- get
  let symNames = ((S.unions . (map M.keysSet) . M.elems)) allSyms
      isNames = M.keysSet (b ^. bIntermediateStates)
  return $ S.union symNames isNames

assertUniqueName :: String -> Builder s ()
assertUniqueName name = do
  allNames <- getAllNames
  when (S.member name allNames) (err $ "in Builder, \"" ++ name ++ "\" is not a unique name")

infix 4 @=
(@=) :: Expr -> String -> Builder s ()
(@=) expr name = do
  debug $ "setting intermediate state: " ++ name ++ " = " ++
    withEllipse 30 (show expr)
  assertUniqueName name
  b <- get
  put $ over bIntermediateStates (M.insert name expr) b

infix 4 ===
(===) :: Expr -> Expr -> Builder s ()
(===) lhs rhs = do
  debug $ "adding implicit equation: " ++
    withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
  state0 <- get
  put $ over bImplicitEqs ((lhs,rhs):) state0

addSym :: (Ord s, Show s) => s -> String -> Builder s Expr
addSym overMe name = do
  let descriptor = show overMe
  debug $ "adding "++descriptor++": "++name
  assertUniqueName name
  b <- get
  let sym = Dvda.sym name
      map0 = b ^. bSyms
  when (M.notMember overMe map0) $ impossible $ descriptor ++ " not in symbol map"
  put $ over bSyms (M.adjust (M.insert name sym) overMe) b
  return sym


--daeSummary :: String -> DaeBuilderState -> IO ()
--daeSummary prefix dae = do
--  let blah name overMe = putStrLn $ prefix ++ show (length keys) ++ " " ++ name ++ " " ++ show keys
--        where
--          keys = M.keys (dae ^. overMe)
--  blah "diff states" dbX
--  blah "alg vars   " dbZ
--  blah "controls   " dbU
--  blah "parameters " dbP
--  blah "constants  " dbC
--  let ieqs = dae ^. dbImplicitEq
--      istates = M.keys $ dae ^. dbIntermediateState
--  putStrLn $ prefix ++ show (length ieqs) ++ " implicit equations:"
--  mapM_ (\(lhs,rhs) -> putStrLn (show lhs ++ " == " ++ show rhs)) ieqs
--  putStrLn $ prefix ++ show (length istates) ++ " intermediate states: " ++ withEllipse 70 (show istates)

showSymbols :: Show s => M.Map s (M.Map String Expr) -> IO ()
showSymbols blahs = do
  putStrLn "symbols: "
  mapM_ (\(key,vals) -> putStrLn $ "  " ++ show key ++ ": " ++ show (M.keys vals)) $ M.toList blahs

showIntermediateStates :: M.Map String Expr -> IO ()
showIntermediateStates is = do
  putStrLn $ "intermediate states: (" ++ show (M.size is) ++ ")"
  mapM_ (\(key,val) -> putStrLn $ "  " ++ key ++ ": " ++ withEllipse 40 (show val)) $ M.toList is

showImplicitEqs :: [(Expr,Expr)] -> IO ()
showImplicitEqs ieqs = do
  putStrLn $ "implicit equations: (" ++ show (length ieqs) ++ ")"
  mapM_ (\(lhs,rhs) -> putStrLn (withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs))) ieqs

summary :: (Enum syms, Ord syms, Show syms) => Builder syms a -> IO ()
summary builder = do
  let (result, messages, b) = build builder
      (n0,n1,n2,n3) = countLogs messages
  putStrLn $ "log: (" ++ show n0 ++ " messages, "++show n1++" warnings, "++show n2++" errors, "++show n3++" \"impossible\" errors)"
  mapM_ (putStrLn . ("  " ++) . show) messages
  putStr "\nresult: "
  case result of Left (ErrorMessage x) -> putStrLn $ "Failure: " ++ x
                 Right _ -> do
                   putStrLn "Success!"
                   putStrLn ""
                   showSymbols (b ^. bSyms)
                   putStrLn ""
                   showIntermediateStates (b ^. bIntermediateStates)
                   putStrLn ""
                   showImplicitEqs (b ^. bImplicitEqs)
