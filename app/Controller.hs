{-# LANGUAGE FlexibleContexts #-}
module Controller where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Time.Clock
import System.Console.ANSI
import System.IO
import Control.Concurrent

import Configuration
import Joystick
import Truster
import Battery

-- Controller loop
runController :: Chan Event -> ReaderT Config (StateT Controller IO) ()
runController chan = forever $ do
  controller <- get
  event <- liftIO $ readChan chan
  let now   = armPressed controller
  let isArmed = armed controller
  let lastArmed_ = lastArmed controller
  let armPressed_ = armPressed controller
  case event of
    -- Render screen on tick event
    TickEvent   -> do
      liftIO clearScreen
      adjustPower
      renderController
      return ()

    BatterDrop  -> do
      reduceBatteryLevel
      checkIfDisarmNeeded
      return ()

    KeyEvent k  -> do
      case k of
        'a' -> armController now
        'w' -> moveJoystick U
        's' -> moveJoystick D
        'j' -> moveJoystick L
        'l' -> moveJoystick R
        _  -> return ()

-- Arm or disarm the controller
-- Only allows state to change once every two seconds -- handle time difference between last press and now
armController :: MonadState Controller m => UTCTime -> m ()
armController now = do
   controller <- get
   let isArmed = armed controller               -- Current armed state
   let lastPressed = lastArmed controller       -- When was arm last pressed
   let diff = addUTCTime 2 lastPressed          -- add 2 seconds to when last pressed
   let diffOK = diff > now
   if updateController diffOK then
      put (controller {armed = not isArmed, lastArmed = now})
   else put controller
   where
      updateController True = True
      updateController False = False

-- Move Joystick
moveJoystick :: MonadState Controller m => JoystickMove -> m ()
moveJoystick direction = do
   controller <- get
   let _armed = armed controller
   let _axis = (getPosition . joystick) controller
   if not _armed then
      return ()
   else
      put (controller {joystick = Joystick (move direction _axis),
                       trusterPower = snd _axis -- y controld power level
                       })
      where
         -- Limit the values between 0 - 100 % on both axis
         move U (x, 100)   = (x, 100)
         move U (x, y)     = (x, y + 2)
         move D (x, 0)     = (x, 0)
         move D (x, y)     = (x, y - 2)
         move L (0, y)     = (0, y)
         move L (x, y)     = (x - 2, y)
         move R (100, y)   = (100, y)
         move R (x, y)     = (x + 2, y)

-- Handle power delivery
adjustPower :: MonadState Controller m => m ()
adjustPower = do
   controller <- get
   let power = trusterPower controller
   let limit = powerLimit controller
   let powerLimit = getPowerLimit limit
   if power > powerLimit then
      put (controller { trusterPower = powerLimit})
   else
      put (controller { trusterPower = power})

getPowerLimit :: PowerLimit -> PowerLevel
getPowerLimit limit = do
   case limit of
      Low      -> 25
      Medium   -> 70
      High     -> 100

-- Render Armed status
renderArmed :: ScreenPos -> ArmState -> IO ()
renderArmed (row, col) armed = do
  setCursorPosition row col; putStr "Armed: "
  if armed then
     setSGR [SetColor Foreground Vivid Green]
  else
     setSGR [SetColor Foreground Dull Red]
  setCursorPosition row (col + 7); putStr $ "" ++ showArmState armed
  setSGR [Reset]

showArmState :: ArmState -> String
showArmState state
   | not state = "OFF"
   | otherwise = "ON"

-- Instructions: 
--           W
-- Move:   J   L   Arm: A
--           S
renderHelp :: ScreenPos -> IO ()
renderHelp (row, col) = do
  setCursorPosition row col;         putStr $ "Instructions:"
  setCursorPosition (row + 2) col;   putStr $ "          W"
  setCursorPosition (row + 3) col;   putStr $ "Move:   J   L   Arm: A"
  setCursorPosition (row + 4) col;   putStr $ "          S"

renderController :: (MonadReader Config m, MonadState Controller m, MonadIO m) => m ()
renderController = do
  config <- ask
  controller <- get
  liftIO $ renderArmed (2,2) (armed controller)
  liftIO $ renderJoystick (4,2) (joystick controller)
  liftIO $ renderTruster (4,14) (trusterPower controller) (trusterAngle controller) (powerLimit controller)
  liftIO $ renderBattery (4, 45) (batteryLevel controller) (batteryMedLevel config) (batteryLowLevel config)
  liftIO $ renderHelp (8, 2)
  return ()