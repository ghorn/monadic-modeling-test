{-# OPTIONS_GHC -Wall #-}

module KinematicMekf ( kinematicMekf
                     , go
                     ) where

import Linear
import Estimator

kinematicMekf :: Estimator ()
kinematicMekf = do
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

go :: IO ()
go = estimatorSummary kinematicMekf
