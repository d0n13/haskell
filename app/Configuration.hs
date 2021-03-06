{-# LANGUAGE FlexibleContexts #-}
module Configuration where

import Data.Time

-- Joystic center position (50%, 50%) is no trottle and mid steering
-- Joystick left and right control the truster positions which
-- rotate around their center positions by a configurabele limit
type Axis         = Int
type Angle        = Int
newtype Joystick  = Joystick { getPosition :: (Axis, Axis)} deriving Show
data JoystickMove = U | D | L | R deriving Show
type ArmState     = Bool
type Battery      = Int
type PowerLevel   = Int
data PowerLimit   = Low | Medium | High deriving Show

-- Rendering
type ScreenPos    = (Int, Int)

-- Events
data Event
  = TickEvent
  | BatterDrop
  | KeyEvent Char deriving Show

data Controller   = Controller
   {
      joystick       :: Joystick,   -- Joystick axis (x,y) 0-100% 50% is center
      armed          :: ArmState,   -- On off switch, inputs have no response when not armed
      lastArmed      :: UTCTime,    -- Time the arm button was last pressed. Arm or disarm must be debounced
      armPressed     :: UTCTime,    -- Time user presses arm
      trusterPower   :: PowerLevel, -- Truster power level 0-100%
      powerLimit     :: PowerLimit, -- Truster power limit
      trusterAngle   :: Angle,      -- Truster angle
      batteryLevel   :: Battery     -- Current battery percentage
   }

initController :: IO Controller
initController = do
   timeNow <- getCurrentTime
   return $ Controller
      {
         joystick          = Joystick (50, 0),  -- centered direction and zero power
         armed             = False,             -- controller off
         lastArmed         = timeNow,           -- can't arm within two seconds from now
         armPressed        = timeNow,           -- can't arm within two seconds from now
         trusterPower      = 0,                 -- No power
         powerLimit        = High,              -- Max power available
         trusterAngle      = 270,               -- Centered looking from top
         batteryLevel      = 100                -- Batter is 100% at start
      }

data Config       = Config
   {
      trusterMaxTurn  :: Angle,      -- Maximum amount truster can turn left or right in degrees
      batteryMedLevel :: Battery,    -- Battery medium level - restricts truster power to MED
      batteryLowLevel :: Battery,    -- Battery low level - restricts truster power to LOW
      batteryShutdown :: Battery     -- Battery shut down level - disarms controller
   }
   deriving Show

initConfig :: IO Config
initConfig = do
  return $ Config
    {
      trusterMaxTurn = 45,
      batteryMedLevel = 70,
      batteryLowLevel = 25,
      batteryShutdown = 5
    }

-- Convert the PowerLimit string into a string
limitToString :: PowerLimit -> String
limitToString limit = 
   case limit of 
      Low      -> "Low"
      Medium   -> "Medium"
      High     -> "High"