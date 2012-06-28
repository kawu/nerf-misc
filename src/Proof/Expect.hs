module Proof.Expect where

import Prelude hiding (sum, product)
import qualified Data.MemoTrie as Memo

import Proof.LogMath
import Proof.Utils
import Proof.Tree
import Proof.Forest

import Debug.Trace (trace)

phiN :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a
     -> Pos -> Pos -> Pos -> Pos -> Double
phiN active nerf p q i j =
    let phiZ = sumPhiF active nerf
    in  phiZ p (i-1) .*. phiZ (j+1) q ./. phiZ p q

probTreeSpan :: (Ord a, Memo.HasTrie a)
             => Active a -> Nerf a -> Tree a
             -> Pos -> Pos -> Pos -> Pos -> Double
probTreeSpan active nerf tree p q i j =
    phiTree nerf tree .*. phiN active nerf p q i j

probTreeSpan' :: (Ord a, Memo.HasTrie a)
              => Active a -> Nerf a -> Tree a
              -> Pos -> Pos -> Pos -> Pos -> Double
probTreeSpan' active nerf tree p q i j = sum
    [ probForest active nerf p q forest
    | forest <- forestSet active nerf p q
    , forest `hasTree` tree ]

probSpan :: (Ord a, Memo.HasTrie a)
         => Active a -> Nerf a
         -> Pos -> Pos -> Pos -> Pos -> Double
probSpan active nerf p q i j =
    case sumPhiSpan active nerf i j of
        Just x  -> x .*. phiN active nerf p q i j
        Nothing -> zero

probSpan' :: (Ord a, Memo.HasTrie a)
          => Active a -> Nerf a
          -> Pos -> Pos -> Pos -> Pos -> Double
probSpan' active nerf p q i j = sum
    [ probTreeSpan active nerf tree p q i j
    | tree <- treeSetSpan active nerf i j ]

-- | Expected number of features conditional on tree span
-- (tree setting assumed).
expectedNumT :: (Ord a, Memo.HasTrie a)
             => Active a -> Nerf a -> Feature a
             -> Pos -> Pos -> Double
expectedNumT active nerf feat i j = sum
    [ probTree active nerf tree .*. featureNum feat tree
    | tree <- treeSetSpan active nerf i j ]

-- | Expected number of features in a sentence (optimized version).
expectedNumF :: (Ord a, Memo.HasTrie a)
             => Active a -> Nerf a -> Feature a
             -> Pos -> Pos -> Double
expectedNumF active nerf feat p q = sum
    [ probSpan active nerf p q i j .*.
      expectedNumT active nerf feat i j
    | i <- [p..q], j <- [i..q] ]

-- | Expected number of features in a sentence.
expectedNumF' :: (Ord a, Memo.HasTrie a)
              => Active a -> Nerf a -> Feature a
              -> Pos -> Pos -> Double
expectedNumF' active nerf feat p q = sum
    [ probForest active nerf p q f .*. featureNumF feat f
    | f <- forestSet active nerf p q ]

-- 
-- QuickCheck properties
--

propTreeSpan :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propTreeSpan test =
  case mTree of
    Just tree -> trace (show (y, y')) (y ~== y')
      where
        y  = Just $ exp $ probTreeSpan  active nerf tree p q i j
        y' = Just $ exp $ probTreeSpan' active nerf tree p q i j
    Nothing -> True
  where
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    mTree = testTree test
    (p, q) = testSpan test
    (i, j, _x) = testComp test

propSpan :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propSpan test =
    trace (show (y, y')) (y ~== y')
  where
    y  = Just $ exp $ probSpan  active nerf p q i j
    y' = Just $ exp $ probSpan' active nerf p q i j
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    mTree = testTree test
    (p, q) = testSpan test
    (i, j, _x) = testComp test

propExpected :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propExpected test =
    trace (show (y, y')) (y ~== y')
  where
    y  = Just $ exp $ expectedNumF  active nerf feat p q
    y' = Just $ exp $ expectedNumF' active nerf feat p q
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    mTree = testTree test
    feat = featFromDesc $ testFeat test
    (p, q) = testSpan test
    (i, j, _x) = testComp test
