module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Math (sqrt, pi)
import Global.Unsafe

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello CAMM!"
  log $ show $ map (+3) [1,2,3]
  print (diagonal 10.0 10.0)
  log (unsafeStringify $ circleArea 10.0)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = 2.0 * pi * (sqrt r)
