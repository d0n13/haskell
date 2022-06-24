{-# LANGUAGE FlexibleContexts #-}

module Truster where

import System.Console.ANSI

import Configuration

-- Render the truster info screen
renderTruster :: ScreenPos -> PowerLevel -> Angle -> PowerLimit -> IO ()
renderTruster (row, col) power angle limit = do

  setCursorPosition (row - 1) (col + 16); putStr $ "" ++ show power ++ "%"
  setCursorPosition row col; putStr $       "Truster Power : " 

  setSGR [SetColor Foreground Vivid Yellow]
  setCursorPosition row (col + 16); putStr $ replicate (power `div` 10) '>'
  setSGR [Reset]
  
  setCursorPosition (row + 1) col; putStr $ "        Limit : " ++ limitToString limit
  setCursorPosition (row + 2) col; putStr $ "Truster Angle : " ++ show angle
  