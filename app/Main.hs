{-# LANGUAGE FlexibleContexts #-}
module Main where

import System.IO
--import System.Console.ANSI
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent 
import Graphics.Gloss

-- State of control trusters

-- Joystic center position is no trottle and mid steering
-- Joystick left and right control the truster position which
-- rotate around their center positions by ±45°
type Axis           = Int
newtype Joystick    = Joystick { getPosition :: (Axis, Axis)} deriving Show
data Armed          = On | Off deriving Show
type Battery        = Int

circles = pictures [Translate 80 0 one, Translate (-80) 0 two]
one = Color yellow $ Circle 80
two = Color orange $ Circle 50

draw :: Float -> Picture
draw t
   | t <= 0.5    = blank                                                -- in the first second
   | t <= 3.1   = pictures [Translate 80 0 one]                        -- between 1s and 2s
   | otherwise = pictures [Translate 80 0 one, Translate (-80) 0 two] -- afterwards

main :: IO ()
main = animate (InWindow "Microcontroller" (640, 480) (100, 100)) azure (draw)