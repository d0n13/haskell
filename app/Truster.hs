{-# LANGUAGE FlexibleContexts #-}

module Truster where

import System.Console.ANSI

import Configuration
import Joystick

renderTruster :: ScreenPos -> PowerLevel -> Angle -> IO ()
renderTruster (row, col) power angle = do
  setCursorPosition row col; putStr $ "Truster Power: " ++ replicate (power `div` 10) '>'
  setCursorPosition (row + 1) col; putStr $ "Truster Angle: " ++ show angle

