{-# LANGUAGE FlexibleContexts #-}

module Joystick where

import System.Console.ANSI

import Configuration ( ScreenPos, Joystick(getPosition) )

-- Render joystick position
renderJoystick :: ScreenPos -> Joystick -> IO ()
renderJoystick (row, col) joystick = do
  setCursorPosition row col
  putStr $ "x: " ++ show (fst (getPosition joystick))
  setCursorPosition (row + 1) col
  putStr $ "y: " ++ show (snd (getPosition joystick))