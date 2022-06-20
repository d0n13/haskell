{-# LANGUAGE FlexibleContexts #-}
module Controller where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Graphics.Gloss
import Data.Time.Clock
import Debug.Trace

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
      
   -- if diffOK then
   --    if isArmed == On then
   --       put (controller {armed = Off, lastArmed = now}) -- update state to OFF
   --    else
   --       put $ controller {armed = On, lastArmed = now} -- update state to ON
   -- else 
   --    put $ controller




--    setArmed isArmed diff controller             -- set armed state if time between last press and now is > 2 secs
--    where
--       setArmed armState diff controller =
--          if diff >= 2 then
--             if (armState == On) then
--                put (controller {armed = Off, lastArmed = now}) -- update state to OFF
--             else
--                put $ controller {armed = On, lastArmed = now} -- update state to ON
--          else 
--             return ()

-- addSeconds :: NominalDiffTime -> UTCTime -> UTCTime
-- addSeconds seconds = addUTCTime (seconds)


-- moveJoystick :: MonadState Controller m => JoystickMove -> m ()
-- moveJoystick direction = do 
--    contoller <- get
--    let (x, y) = axis controller

redCircle = Color red $ Circle 50

-- Draw a truster at the specified angle
drawJoystickPosition :: Joystick -> Picture
drawJoystickPosition joystick =
   pictures [Translate 80 0 redCircle]