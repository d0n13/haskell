{-# LANGUAGE FlexibleContexts #-}

module Battery where

import System.Console.ANSI

import Configuration

renderBattery :: ScreenPos -> Battery -> Battery -> Battery -> IO ()
renderBattery (row, col) level lowLevel = do
  setCursorPosition row col; putStr $ "Truster Angle: " ++ show (fst (getPosition joystick))
  setCursorPosition (row + 1) col; putStr $ "Truster Power: " ++ show (fst (getPosition joystick))


batteryLevel      = 100,
batteryLowLevel   = 10,
batteryShutdown   = 2