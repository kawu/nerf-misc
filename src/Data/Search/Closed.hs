module Data.Search.Closed
( Closed (Closed)
, leftAdjacent
, rightAdjacent
, empty
, push
) where

import Data.Search.Node

data Closed a = Closed a

leftAdjacent :: Node -> Closed a -> [(Node, a)]
leftAdjacent = undefined

rightAdjacent :: Node -> Closed a -> [(Node, a)]
rightAdjacent = undefined

empty :: Closed a
empty = undefined

push :: Closed a -> (Node, a) -> Closed a
push = undefined
