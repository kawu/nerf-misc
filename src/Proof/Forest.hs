module Proof.Forest
( Forest
, forestSet
) where

import Prelude hiding (sum, product)
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.MemoTrie as Memo

import Proof.Utils
import Proof.LogMath
import Proof.Tree

import Debug.Trace (trace)

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
            , f <- recSet i (k - 1)
            , t <- treeSetSpan active nerf k j ]

-- | Potential of a given forest.
phiForest :: Nerf a -> Forest a -> Phi
phiForest nerf f = product [phiTree nerf t | t <- f]

sumPhiF :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a -> Pos -> Pos -> Phi
sumPhiF active nerf = sumPhiM
  where
    sumPhiM = Memo.memo2 sumPhi'
    sumPhi' i j
        | i > j     = one
        | otherwise = sumPhiM i (j - 1) .+. sum
            [sumPhiM i (k - 1) `mul` sumPhiTree k j  | k <- [i..j]]
    mul x (Just y) = x .*. y
    mul _ Nothing  = zero
    sumPhiTree = sumPhiSpan active nerf

sumPhiF' :: Ord a => Active a -> Nerf a -> Pos -> Pos -> Phi
sumPhiF' active nerf i j = sum 
    [phiForest nerf f | f <- forestSet active nerf i j]

maxPhiF' :: Ord a => Active a -> Nerf a -> Pos -> Pos -> Phi
maxPhiF' active nerf i j = maximum
    [phiForest nerf f | f <- forestSet active nerf i j]

propSumPhiF :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propSumPhiF test =
    trace (show (y, y')) (y ~== y')
  where
    y  = Just $ sumPhiF  active nerf i j
    y' = Just $ sumPhiF' active nerf i j
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    (i, j, _x) = testComp test
