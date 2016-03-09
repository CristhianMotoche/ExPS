module Tree
    ( createIntTree
    , countLeaves
    , treeToList
    , treeToArray
    , Tree(..))
    where

import Prelude (class Show, show, (++), (+))
import Data.List (List(Nil, Cons))

data Tree a = Leaf (Tree a) a (Tree a) | Empty

createIntTree :: Int -> Tree Int
createIntTree x = Leaf Empty x Empty

countLeaves :: forall typ. Tree typ -> Int
countLeaves Empty = 0
countLeaves (Leaf l _ r) = 1 + countLeaves l + countLeaves r

treeToList :: forall typ. Tree typ -> List typ
treeToList Empty = Nil
treeToList (Leaf Empty x Empty) = Cons x (Nil)
treeToList (Leaf l x r) = treeToList l ++ Cons x (Nil) ++ treeToList r

treeToArray :: forall typ. Tree typ -> Array typ
treeToArray Empty = []
treeToArray (Leaf Empty x Empty) = [x]
treeToArray (Leaf l x r) = treeToArray l ++ [ x ] ++ treeToArray r

instance showTree :: (Show a) => Show (Tree a) where
    show (Leaf l x r) =
        "T: " ++ show x ++
            "(" ++ (show l) ++ ")" ++
            "(" ++ (show r) ++ ")"
    show Empty = "-"
