module Main where

import Prelude
import Flare
import Flare.Drawing
import Graphics.Drawing
import Control.Monad.Eff
import Control.Apply (lift3)

fib :: Int -> Int
fib n
  | n <= 0 = 1
  | n == 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

fib' :: forall e. UI e Int
fib' = fib <$> (int "Fibonnaci" 3)

ui :: forall eff. UI eff String
ui = show <$> fib'

coloredCircle :: Number -> Number -> Number -> Drawing
coloredCircle hue sideV sideH =
  filled (fillColor (hsl hue 0.8 0.4)) (rectangle 50.0 50.0 sideV sideH)

uiG = lift3 coloredCircle 
              (numberSlider "Hue"    0.0 360.0 1.0 140.0)
              (numberSlider "Side H" 2.0 145.0 10.0  25.0)
              (numberSlider "Side V" 2.0 145.0 10.0 25.0)

main = do
  runFlare "Multiplication" "Output" ui
  runFlareDrawing "Graphics" "GraphOut" uiG
