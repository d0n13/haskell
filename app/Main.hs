{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import Data.Time
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Graphics.Gloss
import Graphics.Gloss.Data.ViewState (CommandConfig)
import Data.Time.Clock.System (SystemTime (MkSystemTime), getSystemTime)

--- Main
--main :: IO ()
main = do
   controller <- initController
   config <- initConfig
   chan <- newChan
   forkIO $ castTick chan
   forkIO $ castKey chan
   runStateT (runReaderT (runController chan) config) controller

--main = animate window azure (draw)

-- Generate a tick event every 50ms to get 20fps
castTick :: Chan Event -> IO ()
castTick chan = forever $ do
  threadDelay (50 ^ 4)
  writeChan chan TickEvent

-- Read the keyboard
castKey :: Chan Event -> IO ()
castKey chan = forever $ do
  hSetEcho stdin False
  c <- getChar
  writeChan chan (KeyEvent c)

-- Controller loop
runController :: Chan Event -> ReaderT Config (StateT Controller IO) ()
runController chan = forever $ do
  event <- liftIO $ readChan chan
  case event of
    TickEvent   -> do liftIO (putStr "tick"); return ()
    KeyEvent k  -> do
      case k of
        'a' -> armController
      --   'w' -> moveJoystick U
      --   's' -> moveJoystick D
      --   'j' -> moveJoystick L
      --   'k' -> moveJoystick R
        _  -> return ()

-- Arm or disarm the controller
-- Only allows state to change once per seconds -- handle time difference between last press and now
armController :: MonadState Controller m => m ()
armController = do
   controller <- get
   let isArmed = armed controller
   let lastPressed = lastArmed controller
   let now = getSystemTime
   setArmed isArmed lastPressed now controller
   where
      setArmed armState last now controller =
         when (last + 2 >= now) $
         if armState == On then
            put $ controller {armed = Off, lastArmed = now}
         else
            put $ controller {armed = On, lastArmed = now}

-- moveJoystick :: MonadState Controller m => JoystickMove -> m ()
-- moveJoystick direction = do 
--    contoller <- get
--    let (x, y) = axis controller

-- Joystic center position (50%, 50%) is no trottle and mid steering
-- Joystick left and right control the truster position which
-- rotate around their center positions by a configurabele limit
type Axis         = Int
newtype Joystick  = Joystick { getPosition :: (Axis, Axis)} deriving Show
data JoystickMove = U | D | L | R deriving Show
data Armed        = On | Off deriving Show
type Battery      = Int
type PowerLevel   = Int
data PowerLimit   = Low | Medium | High deriving Show

data Event
  = TickEvent
  | KeyEvent Char deriving Show

data Controller   = Controller
   {
      axis           :: Joystick,   -- Joystick axis (x,y) 0-100% 50% is center
      armed          :: Armed,      -- On off switch, inputs have no response when not armed
      lastArmed      :: SystemTime, -- Time the arm button was last pressed. Arm or disarm must be debounced
      trusterPower   :: PowerLevel, -- Truster power level 0-100%
      batteryLevel   :: Battery,    -- Current battery percentage
      batteryLowLevel:: Battery,    -- Low battery level warning level - truster enters low power mode
      batteryShutdown:: Battery     -- Battery shutdown level, disarms controller
   }

initController :: IO Controller
initController = do
   timeNow <- getSystemTime
   return $ Controller
      {
         axis              = Joystick (50, 50),
         armed             = Off,
         lastArmed         = timeNow,
         trusterPower      = 0,
         batteryLevel      = 100,
         batteryLowLevel   = 10,
         batteryShutdown   = 2
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


-- scratch space for graphics. Adding later. 
circles = pictures [Translate 80 0 one, Translate (-80) 0 two]
one = Color yellow $ Circle 80
two = Color orange $ Circle 50

draw :: Float -> Picture
draw t
   | t <= 0.1    = blank                                              -- in the first second
   | t <= 0.2   = pictures [Translate 80 0 one]                       -- between 1s and 2s
   | otherwise = pictures [Translate 80 0 one, Translate (-80) 0 two] -- afterwards

window :: Display
window = InWindow "Microcontroller" (640, 480) (100, 100)




-- https://github.com/MondayMorningHaskell/SimpleMaze/blob/main/SimpleMaze.cabal
