{-# OPTIONS_GHC -Wall #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}

module DaeBuilder ( Dae
                    -- * dae symbols
                  , diffState, algVar, control, parameter, constant
                    -- * example:
                  , someDae
                  ) where

import Builder

data DaeSymbols = DifferentialState
                | DifferentialStateDeriv
                | Control
                | AlgebraicVar
                | Parameter
                | Constant
                deriving ( Show, Ord, Eq, Enum )

type Dae a = Builder DaeSymbols a

diffState, algVar, control, parameter, constant :: String -> Dae Expr
diffState = addSym DifferentialState
algVar    = addSym AlgebraicVar
control   = addSym Control
parameter = addSym Parameter
constant  = addSym Constant

--ddt :: String -> Dae Expr
--ddt name = addSym DifferentialStateDeriv "ddt(" ++ name ++ ")"

someDae :: Dae ()
someDae = do
  _    <- diffState "pos"
  vel  <- diffState "vel"
  mass <- diffState "mass"

  thrust <- control "thrust"

  k <- constant "viscous damping"
  -- parameter "pos" -- causes an error

  let accel = thrust/mass - vel*k

  accel @= "acceleration"

--  ddt "pos"  === vel
--  ddt "vel"  === accel
--  ddt "mass" === -0.8*thrust*thrust

  vel === -0.8*thrust*thrust

go :: IO ()
go = summary someDae
