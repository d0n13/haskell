{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI

import Controller
import Configuration 

-- Main Loop
main = do
  controller <- initController
  config <- initConfig
  setNoBuffering
  chan <- newChan
  forkIO $ castTick chan
  forkIO $ castKey chan
  forkIO $ batteryTick chan
  liftIO . putStrLn $ "Running"
  runStateT (runReaderT (runController chan) config) controller

-- disable screen buffering
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

-- Reduce the batter by soem amount (depending omn power level) every tick
batteryTick :: Chan Event -> IO ()
batteryTick chan = forever $ do
  threadDelay (30 * (10 ^ 5)) -- 3 sec
  writeChan chan BatterDrop

-- Read the keyboard
castKey :: Chan Event -> IO ()
castKey chan = forever $ do
  hSetEcho stdin False
  c <- getChar
  writeChan chan (KeyEvent c)
