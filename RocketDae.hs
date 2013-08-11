{-# OPTIONS_GHC -Wall #-}

module RocketDae ( rocketDae
                 , go
                 ) where

import Dae

rocketDae :: Dae ()
rocketDae = do
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
go = daeSummary rocketDae
