{-# LANGUAGE FlexibleContexts #-}

module Util where

-- Map a value in one range into another
mapRange :: Integral a => (a, a) -> (a, a) -> a -> a
mapRange (a1, a2) (b1, b2) s = (s - a1) * (b2 - b1) `div` (a2 - a1) + b1