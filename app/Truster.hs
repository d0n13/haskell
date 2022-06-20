{-# LANGUAGE FlexibleContexts #-}

module Truster where

import Graphics.Gloss

orangeCircle = Color orange $ Circle 50

-- Draw a truster at the specified angle
drawTruster :: Float -> Picture
draw angle =
   pictures [Translate 80 0 orangeCircle]