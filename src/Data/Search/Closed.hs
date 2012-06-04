module Data.Search.Closed
( Closed (Closed)
, leftAdjacent
, rightAdjacent
, empty
, push
) where

import qualified Data.IntMap as I

import Data.Search.Node

data Closed a = Closed
    { begOn :: I.IntMap [(Node, a)]
    , endOn :: I.IntMap [(Node, a)] }

empty :: Closed a
empty = Closed I.empty I.empty

leftAdjacent :: Node -> Closed a -> [(Node, a)]
leftAdjacent n c = case I.lookup (beg n - 1) (endOn c) of
    Just xs -> xs
    Nothing -> []

rightAdjacent :: Node -> Closed a -> [(Node, a)]
rightAdjacent n c = case I.lookup (end n + 1) (begOn c) of
    Just xs -> xs
    Nothing -> []

push :: (Node, a) -> Closed a -> Closed a
push p c = Closed begOn' endOn'
  where
    begOn' = I.insertWith (++) (beg n) [p] (begOn c)
    endOn' = I.insertWith (++) (end n) [p] (endOn c)
    n = fst p
