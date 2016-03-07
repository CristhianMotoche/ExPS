module Main where

import Prelude (Unit, show, ($), bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Tree ( createIntTree
    , countLeaves
    , treeToList
    , treeToArray
    , Tree(..))

main :: forall t. Eff ( console :: CONSOLE | t ) Unit
main = do
      log "TREE: "
      log $ show tree
      log "Number of leaves: "
      log $ show $ countLeaves tree
        where
            tree = Leaf Empty 2 (Leaf Empty 2 Empty)
