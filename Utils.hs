{-# OPTIONS_GHC -Wall #-}

module Utils ( withEllipse
             ) where

withEllipse :: Int -> String -> String
withEllipse n blah
  | length blah <= n = blah
  | otherwise = take n blah ++ "..."

