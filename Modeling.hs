{-# OPTIONS_GHC -Wall #-}
{-# Language TemplateHaskell #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Modeling ( (@=)
--                , (===)
                , Expr
                , Builder (..)
                , build
                , addSym
                ) where


import Control.Lens ( makeLenses, over )
import Control.Lens.Setter ( Setting )
import Control.Monad ( liftM, when )
import Control.Monad.Error ( ErrorT, MonadError, runErrorT )
import Control.Monad.State ( State, MonadState, runState, get, put )
import Control.Monad.Writer ( WriterT, MonadWriter, runWriterT )
import qualified Data.Map as M
import qualified Data.Set as S

import qualified Dvda

import LogsAndErrors
import Utils ( withEllipse )

type Expr = Dvda.Expr Double

data BuilderState s = BuilderState { _symbols :: s
                                   , _intermediateState :: M.Map String Expr
                                   }
newtype Builder s a =
  Builder
  { runBuilder :: ErrorT ErrorMessage (WriterT [LogMessage] (State (BuilderState s))) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadState (BuilderState s)
             , MonadWriter [LogMessage]
             )

makeLenses ''BuilderState

build :: s -> Builder s a -> (Either ErrorMessage a, [LogMessage], BuilderState s)
build emptyUser builder = (result, logs, state)
  where
    emptyBuilder = BuilderState emptyUser M.empty
    ((result,logs),state) =
      flip runState emptyBuilder . runWriterT . runErrorT . runBuilder $ builder

getAllSyms :: Builder s (M.Map String (M.Map String Expr))
--getAllSyms = liftM (^. symbols) get
getAllSyms = error "implement getAllSyms"
  
getAllNames :: Builder s (S.Set String)
getAllNames = liftM ((S.unions . (map M.keysSet) . M.elems)) getAllSyms

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
  state0 <- get
  put $ over intermediateState (M.insert name expr) state0

--infix 4 ===
--(===) :: Expr -> Expr -> Builder s ()
--(===) lhs rhs = do
--  debug $ "adding implicit equation: " ++
--    withEllipse 30 (show lhs) ++ " == " ++ withEllipse 30 (show rhs)
--  state0 <- get
--  put $ over dbImplicitEq ((lhs,rhs):) state0

addSym :: Setting (->) s s (M.Map String Expr) (M.Map String Expr)
       -> String -> String -> Builder s Expr
addSym overMe descriptor name = do
  debug $ "adding "++descriptor++": "++name
  assertUniqueName name
  state0 <- get
  let sym = Dvda.sym name
  put $ over symbols (over overMe (M.insert name sym)) state0
  return sym
