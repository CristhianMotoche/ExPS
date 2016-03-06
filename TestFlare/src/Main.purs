module Main where

import Prelude
import Control.Monad.Eff
import Flare

f :: forall e. UI e Int
f = (int "A" 6) * (int "B" 7)

ui :: forall eff. UI eff String
ui = show <$> f

main = runFlare "Multiplication" "Output" ui
