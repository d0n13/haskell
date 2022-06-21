{-# LANGUAGE FlexibleContexts #-}

module Truster where

import System.Console.ANSI
import Control.Monad.State

import Configuration
import Joystick

renderTruster :: ScreenPos -> PowerLevel -> Angle -> PowerLimit -> IO ()
renderTruster (row, col) power angle limit = do
  setCursorPosition row col; putStr $       "Truster Power : " ++ replicate (power `div` 10) '>'
  setCursorPosition (row + 1) col; putStr $ "Truster Angle : " ++ show angle
  setCursorPosition (row + 2) col; putStr $ "Limit         : " ++ limitToString limit

limitToString :: PowerLimit -> String
limitToString limit = 
   case limit of 
      Low      -> "Low"
      Medium   -> "Medium"
      High     -> "High"