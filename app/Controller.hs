{-# LANGUAGE FlexibleContexts #-}
module Controller where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Time.Clock

import Configuration

-- Arm or disarm the controller
-- Only allows state to change once every two seconds -- handle time difference between last press and now
armController :: MonadState Controller m => UTCTime -> m ()
armController now = do
   controller <- get
   let isArmed = armed controller               -- Current armed state
   let lastPressed = lastArmed controller       -- When was arm last pressed
   let diff = (addUTCTime 2 lastPressed)        -- add 2 seconds to when last pressed
   let diffOK = diff > now
   case updateController diffOK of
      True -> put (controller {armed = not isArmed, lastArmed = now})
      False -> put controller
   where 
      updateController True = True
      updateController False = False

moveJoystick :: MonadState Controller m => JoystickMove -> m ()
moveJoystick direction = do
   controller <- get
   let _axis = (getPosition . joystick) controller
   -- let yAxis = snd axis controller
   put (controller {joystick = Joystick (move direction _axis)})
   where
      move U (x, y) = (x, y + 2)
      move D (x, y) = (x, y - 2)
      move L (x, y) = (x - 2, y)
      move R (x, y) = (x + 2, y)