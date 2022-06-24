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
                    High    -> decBattery level 4

-- Don't decrement battery lower than 0 
decBattery :: Battery -> Int -> Battery
decBattery level decremntBy = do
    let newVal = level - decremntBy
    if newVal < 0 then 0
    else 
        newVal

-- If battery drops below setpoints then limit the truster power
reducePowerAtSetpoints :: (MonadReader Config m, MonadState Controller m) => m ()
reducePowerAtSetpoints = do
    controller <- get
    config <- ask
    let level = batteryLevel controller
    let medLevel = batteryMedLevel config
    let lowLevel = batteryLowLevel config
    case level of
        x | x <= lowLevel   -> put (controller { powerLimit = Low } )
        x | x <= medLevel   -> put (controller { powerLimit = Medium } )
        _                   -> put (controller { powerLimit = High } )

-- disarm controller if battery below shutdown value limit
checkIfDisarmNeeded :: (MonadReader Config m, MonadState Controller m) => m ()
checkIfDisarmNeeded = do
    controller <- get
    config <- ask
    when (batteryLevel controller < batteryShutdown config) $
        put (controller { armed = False } )

-- Render battery info on screen
renderBattery :: ScreenPos -> Battery -> Battery -> Battery -> IO ()
renderBattery (row, col) level battMedLevel battLowLevel = do
    setCursorPosition row col; putStr $ "Battery: "
    renderBatteryColor level battMedLevel battLowLevel
    setCursorPosition row (col + 9); putStr $ "" ++ replicate (level `div` 10) '>'
    setSGR [Reset]
    setCursorPosition (row + 1) (col + 9); putStr $ "" ++ show level ++ "%"

-- Render the battery colour at different colours depending on battery level
renderBatteryColor :: Battery -> Battery -> Battery -> IO ()
renderBatteryColor level battMedLevel battLowLevel  = do
    case level of
        x | x >= battMedLevel   -> setSGR [SetColor Foreground Vivid Green]
        x | x > battLowLevel    -> setSGR [SetColor Foreground Vivid Yellow]
        _                       -> setSGR [SetColor Foreground Vivid Red]

-- Return the values for PowerLimit
getPowerLimit :: PowerLimit -> PowerLevel
getPowerLimit limit = do
   case limit of
      Low      -> 25
      Medium   -> 70
      High     -> 100