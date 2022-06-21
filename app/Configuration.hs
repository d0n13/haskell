{-# LANGUAGE FlexibleContexts #-}
module Configuration where

import Data.Time

-- Joystic center position (50%, 50%) is no trottle and mid steering
-- Joystick left and right control the truster positions which
-- rotate around their center positions by a configurabele limit
type Axis         = Int
newtype Joystick  = Joystick { getPosition :: (Axis, Axis)} deriving Show
data JoystickMove = U | D | L | R deriving Show
data Armed        = On | Off deriving Show
type Battery      = Int
type PowerLevel   = Int
data PowerLimit   = Low | Medium | High deriving Show

-- Rendering
type ScreenPos    = (Int, Int)

-- Events
data Event
  = TickEvent
  | KeyEvent Char deriving Show

data Controller   = Controller
   {
      joystick       :: Joystick,   -- Joystick axis (x,y) 0-100% 50% is center
      armed          :: Bool,       -- On off switch, inputs have no response when not armed
      lastArmed      :: UTCTime,    -- Time the arm button was last pressed. Arm or disarm must be debounced
      armPressed     :: UTCTime,    -- Time user presses arm
      trusterPower   :: PowerLevel, -- Truster power level 0-100%
      batteryLevel   :: Battery,    -- Current battery percentage
      batteryLowLevel:: Battery,    -- Low battery level warning level - truster enters low power mode
      batteryShutdown:: Battery,    -- Battery shutdown level, disarms controller
      updateUI       :: Bool        -- If state is updated then this must be set to true to update UI
   }

initController :: IO Controller
initController = do
   timeNow <- getCurrentTime
   return $ Controller
      {
         joystick          = Joystick (50, 50), -- centered
         armed             = False,             -- controller off
         lastArmed         = timeNow,           -- can't arm within two seconds from now
         armPressed        = timeNow,           -- can't arm within two seconds from now
         trusterPower      = 0,
         batteryLevel      = 100,
         batteryLowLevel   = 10,
         batteryShutdown   = 2,
         updateUI          = False
      }

data Config       = Config
   {
      trusterMaxTurn :: Int,        -- Maximum amount truster can turn left or right in degrees
      powerLimit     :: PowerLimit  -- Max amount of power available (low, med, high)
   }
   deriving Show

initConfig :: IO Config
initConfig = do
  return $ Config
    {
      trusterMaxTurn = 45,
      powerLimit = High
    }
