{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language FlexibleContexts #-}

module LogsAndErrors ( ErrorMessage (..)
                     , LogMessage (..)
                     , debug
                     , warn
                     , err
                     , impossible
                     ) where

import Control.Monad.Error ( Error, MonadError, throwError )
import Control.Monad.Writer ( MonadWriter, tell )

data LogMessage = Debug String
                | Warning String
                | Error String
                | Impossible String

instance Show LogMessage where
  show (Debug x) = "Debug: " ++ x
  show (Warning x) = "Warning: " ++ x
  show (Error x) = "Error: " ++ x
  show (Impossible x) = "\"Impossible\" Error: " ++ x

newtype ErrorMessage = ErrorMessage String deriving (Error, Show)

logMessage :: MonadWriter [t] m => t -> m ()
logMessage x = tell [x]

debug :: MonadWriter [LogMessage] m => String -> m ()
debug = logMessage . Debug

warn :: MonadWriter [LogMessage] m => String -> m ()
warn = logMessage . Warning

err :: (MonadError ErrorMessage m, MonadWriter [LogMessage] m) =>
       String -> m a
err x = logMessage (Error x) >> (throwError $ ErrorMessage ("error: " ++ x))

impossible :: (MonadError ErrorMessage m, MonadWriter [LogMessage] m) =>
              String -> m b
impossible x = logMessage (Impossible x) >> (throwError $ ErrorMessage ("\"impossible error\": " ++ x))

