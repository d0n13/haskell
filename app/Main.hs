{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import Data.Time
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import System.Console.ANSI

import Truster
import Controller
import Configuration 
import Data.Time (getCurrentTime)

--- Main
-- main :: IO ()
main = do
  -- liftIO . putStrLn $ "Displaying Window"
  -- forkIO $ display window background drawing 
  controller <- initController
  config <- initConfig
  setNoBuffering
  chan <- newChan
  forkIO $ castTick chan
  forkIO $ castKey chan
  liftIO . putStrLn $ "Running"
  runStateT (runReaderT (runController chan) config) controller

--main = animate window azure (draw)

-- util
setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor

-- Generate a tick event every 50ms to get 20fps
castTick :: Chan Event -> IO ()
castTick chan = forever $ do
  threadDelay (10 ^ 5)
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
  controller <- get
  event <- liftIO $ readChan chan
  let now   = armPressed controller
  let isArmed = armed controller
  let lastArmed_ = lastArmed controller
  let armPressed_ = armPressed controller
  case event of

    TickEvent   -> do 
      liftIO clearScreen
      renderController
      return ()

    KeyEvent k  -> do
      -- liftIO $ print k
      case k of
        'a' -> armController now
        'w' -> moveJoystick U
        's' -> moveJoystick D
        'j' -> moveJoystick L
        'k' -> moveJoystick R
        _  -> return ()

-- Render screen
renderArmed :: ScreenPos -> Bool -> IO ()
renderArmed (row, col) armed = do
  setCursorPosition row col
  putStr $ "Armed: " ++ show armed

renderJoystick :: ScreenPos -> Joystick -> IO ()
renderJoystick (row, col) joystick = do
  setCursorPosition row col
  putStr $ "x: " ++ show (fst (getPosition joystick))
  setCursorPosition (row + 1) col
  putStr $ "y: " ++ show (snd (getPosition joystick))

renderController :: (MonadReader Config m, MonadState Controller m, MonadIO m) => m ()
renderController = do
  config <- ask
  controller <- get
  liftIO $ renderArmed (2,2) (armed controller)
  liftIO $ renderJoystick (4,2) (joystick controller)
  return ()

-- scratch space for graphics. Adding later. 
-- circles = pictures [Translate 80 0 one, Translate (-80) 0 two]
-- one = Color yellow $ Circle 80
-- two = Color orange $ Circle 50

-- draw :: Float -> Picture
-- draw t
--    | t <= 0.1    = blank                                              -- in the first second
--    | t <= 0.2   = pictures [Translate 80 0 one]                       -- between 1s and 2s
--    | otherwise = pictures [Translate 80 0 one, Translate (-80) 0 two] -- afterwards

-- window :: Display
-- window = InWindow "Microcontroller" (640, 480) (100, 100)

-- background :: Color
-- background = black

-- -- drawing :: Picture
-- -- drawing = circle 80

-- drawing :: Picture
-- drawing = pictures
--   [ translate (-295) (215) $ color ballColor $ circleSolid 15
--   , translate (-295) (215) $ color white $ circle 15
--   , color paddleColor $ rectangleSolid 10 50
--   ]
--   where
--     ballColor = dark red 
--     paddleColor = light (light blue) 

-- https://github.com/MondayMorningHaskell/SimpleMaze/blob/main/SimpleMaze.cabal
 