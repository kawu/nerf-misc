module Proof.Forest.Set
( forestSet
) where

import Proof.Nerf
import Proof.Tree
import Proof.Forest.Internal

-- | Recursively build a forest set per given span.
forestSet :: Nerf a -> Pos -> Pos -> [Forest a]
forestSet nerf i j = map reverse $ recSet i j
  where
    recSet i j
        | i > j     = [[]]
        | otherwise = recSet i (j - 1) ++
            [ t : f
            | k <- [i..j]
            , f <- recSet i (k - 1)
            , t <- treeSet nerf k j ]
