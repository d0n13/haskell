{-# LANGUAGE FlexibleContexts #-}

module Battery where

import System.Console.ANSI
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Configuration

-- Battery level drops depending on the power limits
-- Only drops if controller armed
reduceBatteryLevel :: (MonadReader Config m, MonadState Controller m) => m ()
reduceBatteryLevel = do
    controller <- get
    config <- ask
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
decBattery level decremntBy = do
    let newVal = level - decremntBy
    if newVal < 0 then 0
    else 
        newVal

-- disarm controller if battery below shutdown value limit
checkIfDisarmNeeded :: (MonadReader Config m, MonadState Controller m) => m ()
checkIfDisarmNeeded = do
    controller <- get
    config <- ask
    Control.Monad.when (batteryLevel controller < batteryShutdown config) $
        put (controller { armed = False } )

-- Render battery info on screen
renderBattery :: ScreenPos -> Battery -> Battery -> Battery -> IO ()
renderBattery (row, col) level battMedLevel battLowLevel = do
    setCursorPosition row col; putStr $ "Battery: "
    renderBatteryColor level battMedLevel battLowLevel
    setCursorPosition row (col + 9); putStr $ "" ++ replicate (level `div` 10) '>'
    setSGR [Reset]
    setCursorPosition (row + 1) (col + 9); putStr $ "" ++ show level ++ "%"

renderBatteryColor :: Battery -> Battery -> Battery -> IO ()
renderBatteryColor level battMedLevel battLowLevel  = do
    case level of
        x | x >= battMedLevel   -> setSGR [SetColor Foreground Vivid Green]
        x | x > battLowLevel    -> setSGR [SetColor Foreground Vivid Yellow]
        _                       -> setSGR [SetColor Foreground Vivid Red]
