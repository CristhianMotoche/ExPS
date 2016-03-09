module Main where

import Prelude
import Control.Monad.Eff
import Flare

fib :: Int -> Int
fib n
    | n <= 0 = 1
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fib' :: forall e. UI e Int
fib' = fib <$> (int "Fibonnaci" 3)

ui :: forall eff. UI eff String
ui = show <$> fib'

main = runFlare "Multiplication" "Output" ui
