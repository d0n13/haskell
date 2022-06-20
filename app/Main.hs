{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import Data.Time
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Graphics.Gloss

import Truster
import Control
import Configuration 

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
    TickEvent   -> do liftIO (putStr "tick"); return () -- here we update the graphics as requried
    KeyEvent k  -> do
      case k of
        'a' -> armController
      --   'w' -> moveJoystick U
      --   's' -> moveJoystick D
      --   'j' -> moveJoystick L
      --   'k' -> moveJoystick R
        _  -> return ()

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
