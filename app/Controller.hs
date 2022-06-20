{-# LANGUAGE FlexibleContexts #-}
module Controller where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Gloss
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)

import Configuration

-- Arm or disarm the controller
-- Only allows state to change once every two seconds -- handle time difference between last press and now
armController :: MonadState Controller m => m ()
armController = do
   controller <- get
   let isArmed = armed controller               -- Current armed state
   let lastPressed = lastArmed controller       -- When was arm last pressed
   let now = getSystemTime                      -- current system time in seconds
   setArmed isArmed lastPressed now controller  -- set armed state if time between last press and now is > 2 secs
   where
      setArmed armState last now controller =
         when (last + 2 >= now) $
         if armState == On then
            put $ controller {armed = Off, lastArmed = now} -- update state to OFF
         else
            put $ controller {armed = On, lastArmed = now} -- update state to ON


-- moveJoystick :: MonadState Controller m => JoystickMove -> m ()
-- moveJoystick direction = do 
--    contoller <- get
--    let (x, y) = axis controller

redCircle = Color red $ Circle 50

-- Draw a truster at the specified angle
drawJoystickPosition :: Joystick -> Picture
drawJoystickPosition joystick =
   pictures [Translate 80 0 redCircle]