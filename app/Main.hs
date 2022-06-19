{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
--import System.Console.ANSI
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent 
import Graphics.Gloss
import Graphics.Gloss.Data.ViewState (CommandConfig)

--- Main
main :: IO ()
main = do
   controller <- initController
   config <- initConfig
   chan <- newChan
   forkIO $ castTick chan
   putStr $ "tick"

--main = animate window azure (draw)

castTick :: Chan Event -> IO ()
castTick chan = forever $ do
  threadDelay (50 ^ 4)
  writeChan chan TickEvent

-- State of control trusters

-- Joystic center position (50%, 50%) is no trottle and mid steering
-- Joystick left and right control the truster position which
-- rotate around their center positions by a configurabele limit
type Axis         = Int
newtype Joystick  = Joystick { getPosition :: (Axis, Axis)} deriving Show
data Armed        = On | Off deriving Show
type Battery      = Int
type PowerLevel   = Int
data PowerLimit   = Low | Medium | High deriving Show 

data Event
  = TickEvent
  | KeyEvent Char deriving Show

data Controller   = Controller 
   {
      axis     :: Joystick,
      armed    :: Armed,
      power    :: PowerLevel,
      batLevel :: Battery
   }
   deriving Show

initController :: IO Controller
initController = return $ Controller
  {
    axis       = Joystick (50, 50),
    armed      = Off,
    power      = 0,
    batLevel   = 100
  }

data Config       = Config
   {
      trusterMaxTurn :: Int,
      powerLimit     :: PowerLimit
   }
   deriving Show

initConfig :: IO Config
initConfig = do
  return $ Config
    {
      trusterMaxTurn = 45,
      powerLimit = Low
    }

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
