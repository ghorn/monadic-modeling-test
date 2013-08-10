{-# OPTIONS_GHC -Wall #-}
-- {-# Language GeneralizedNewtypeDeriving #-}
{-# Language TemplateHaskell #-}

module Estimator ( Estimator
                 , propogate
                 ) where

import Control.Lens ( makeLenses )
import qualified Data.Map as M
import Linear

import Modeling
import LogsAndErrors

data Estimator = Estimator { _eState :: M.Map String Expr
                           , _eProcess :: M.Map String Expr
                           , _eProcessNoise :: M.Map String Expr
                           , _eConstant :: M.Map String Expr
                           }
makeLenses ''Estimator

setNext :: Expr -> Expr -> Builder Estimator ()
setNext = undefined

setNextV3 :: V3 Expr -> V3 Expr -> Builder Estimator ()
setNextV3 (V3 x0 y0 z0) (V3 x1 y1 z1) = do
  setNext x0 x1
  setNext y0 y1
  setNext z0 z1

state :: String -> Builder Estimator Expr
state = addSym eState "state"

process :: String -> Builder Estimator Expr
process = addSym eProcess "process"

processNoise :: String -> Builder Estimator Expr
processNoise = addSym eProcessNoise "process noise"

constant :: String -> Builder Estimator Expr
constant = addSym eConstant "constant"

vecSym :: (String -> Builder a Expr) -> String -> String -> Builder a (V3 Expr)
vecSym getter getterName name = do
  debug $ "adding "++getterName++" vector for "++name
  x <- getter $ name ++ "_x"
  y <- getter $ name ++ "_y"
  z <- getter $ name ++ "_z"
  return (V3 x y z)

stateV3,processV3,processNoiseV3 :: String -> Builder Estimator (V3 Expr)
stateV3 = vecSym state "state"
processV3 = vecSym process "process"
processNoiseV3 = vecSym processNoise "process noise"

propogate :: Builder Estimator ()
propogate = do
  q_b2e <- stateV3 "q_b2e"
  r_n2b_n <- stateV3 "r_n2b_n"
  v_bn_n <- stateV3 "v_bn_n"
  gyroBias <- stateV3 "gyroBias"
  accelBias <- stateV3 "accelBias"

  gyroSensor <- processV3 "gyroSensor"
  gyroNoise <- processNoiseV3 "gyroNoise"

  accelSensor <- processV3 "accelSensor"
  accelNoise <- processNoiseV3 "accelNoise"
    
  tauGyro <- constant "tauGyro"
  tauAccel <- constant "tauAccel"

  [q_n2b_0,q_n2b_x,q_n2b_y,q_n2b_z] <- mapM constant ["q_n2b_0","q_n2b_x","q_n2b_y","q_n2b_z"]

  ts <- constant "ts"
  
  let q_n2b = Quaternion q_n2b_0 (V3 q_n2b_x q_n2b_y q_n2b_z)
      dcm_b2n = fromQuaternion q_n2b
      dcm_e2b = fromQuaternion (Quaternion 1 q_b2e)
      dcm_e2n = dcm_b2n !*! dcm_e2b
  
      a_bn_n = dcm_e2n !* (accelSensor + accelNoise) - V3 0 0 9.8

  setNextV3 q_b2e $ q_b2e + fmap (0.5*ts*) (gyroSensor - gyroBias + gyroNoise)
  setNextV3 r_n2b_n $ r_n2b_n + fmap (ts*) v_bn_n + fmap (0.5*ts*ts*) a_bn_n
  setNextV3 v_bn_n $ v_bn_n + fmap (ts*) a_bn_n
  setNextV3 gyroBias $ fmap (*exp(-ts/tauGyro)) gyroBias
  setNextV3 accelBias $ fmap (*exp(-ts/tauAccel)) accelBias
