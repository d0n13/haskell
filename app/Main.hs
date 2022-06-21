{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import Data.Time
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import System.Console.ANSI
import Data.Time (getCurrentTime)

import Truster
import Controller
import Configuration 

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
