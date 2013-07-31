{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Woo ( Cg
           , CgState (..)
           , CgConfig (..)
           , ErrorMessage (..)
           , LogMessage (..)
           , runCg
           , cgVar
           , newCg
           , defaultConfig
             -- * debug messages
           , debug
           , warn
           , err
             -- * utils
           , summary
           ) where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Set as S
import Data.HashSet as HS
import LogsAndErrors

import Expr

data CgState = CgState { cgsVars :: HashSet Expr
                       , cgsNames :: Set String
                       }

newCg :: CgState
newCg = CgState HS.empty S.empty

data CgConfig = CgConfig { cgcPrefix :: String
                         }
defaultConfig :: CgConfig
defaultConfig = CgConfig "namespaceYo"

newtype Cg a = Cg {
  runCg' :: ErrorT ErrorMessage (WriterT [LogMessage] (ReaderT CgConfig (State CgState))) a
  } deriving ( Monad
             , MonadError ErrorMessage
             , MonadReader CgConfig
             , MonadState CgState
             , MonadWriter [LogMessage]
             )

runCg :: Cg a -> CgState -> CgConfig -> (Either ErrorMessage a, [LogMessage], CgState)
runCg cg state0 config = (x,y,z)
  where
    ((x,y),z) = flip runState state0 . flip runReaderT config .
                runWriterT . runErrorT . runCg' $ cg

cgVar :: String -> Cg Expr
cgVar name = do
  debug $ "adding codegen var \"" ++ name ++ "\""
  state0 <- get
  let names0 = cgsNames state0
      vars0 = cgsVars state0
      sym = Sym (Dim 1 1) name
  if name `S.member` names0
    then err $ "cgVar got redundant name \""++name++"\""
    else do when (sym `HS.member` vars0) (impossible $ "cgVar got redundant variable: " ++ show sym)
            put $ state0 { cgsNames = S.insert name names0
                         , cgsVars = HS.insert sym vars0
                         }
            return sym

problem :: Cg Expr
problem = do
  x <- cgVar "x"
  y <- cgVar "y"
  return (x * y)

summary :: Show a => Cg a -> IO ()
summary cg = do
  let (result, messages, _) = runCg cg newCg defaultConfig
  putStrLn "log:"
  mapM_ (putStrLn . ("  " ++) . show) messages
  putStr "\nresult: "
  putStrLn $ case result of Left (ErrorMessage x) -> "Failure\n  " ++ x
                            Right x -> "Success\n  " ++ show x
