{-# LANGUAGE FlexibleContexts #-}

module Battery where

import System.Console.ANSI
import Control.Monad.State

import Configuration

-- Battery level drops depending on the power limits
reduceBatteryLevel :: MonadState Controller m => m ()
reduceBatteryLevel = do
    controller <- get
    let level = batteryLevel controller
    let limit = powerLimit controller
    let isArmed = armed controller
    if not isArmed then 
        return ()
    else     
        put (controller { batteryLevel = updateBattery level limit}) 
        where
            updateBattery level limit = do
                case limit of
                    Low     -> decBattery level 1 
                    Medium  -> decBattery level 2
                    High    -> decBattery level 3

-- Don't decrement battery lower than 0 
decBattery :: Battery -> Int -> Battery
decBattery 0 decrementBy = 0
decBattery level decrementBy = level - decrementBy

-- Render battery info on screen
renderBattery :: ScreenPos -> Battery -> IO ()
renderBattery (row, col) level = do
  setCursorPosition row col; putStr $ "Battery: " ++ replicate (level `div` 10) '>'
  setCursorPosition (row + 1) (col + 9); putStr $ "" ++ show level ++ "%"