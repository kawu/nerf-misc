module Proof.Forest
( Forest
, forestSet
) where

import qualified Data.MemoTrie as Memo
import Prelude hiding (sum, product)

import Proof.LogMath
import Proof.Tree

-- | Invariant: trees in a forest are preserved in ascending order
-- (order defined with respect to leaf positions).
type Forest a = [Tree a]

-- | Recursively build a forest set per given span.
forestSet :: Active a -> Nerf a -> Pos -> Pos -> [Forest a]
forestSet active nerf i j = map reverse $ recSet i j
  where
    recSet i j
        | i > j     = [[]]
        | otherwise = recSet i (j - 1) ++
            [ t : f
            | k <- [i..j]
            , f <- recSet i (k-1)
            , t <- treeSetSpan active nerf k j ]

-- | Potential of a given forest.
phiForest :: Nerf a -> Forest a -> Phi
phiForest nerf f = product [phiTree nerf t | t <- f]
