{-# LANGUAGE RecordWildCards #-}

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

-- | Check, if forest satisfy basic invariants.
checkForest :: Forest a -> Bool
checkForest f = and [end t < beg t' | (t, t') <- zip f (tail f)]

-- | Tangible span of the forest.
spanF :: Forest a -> Maybe (Pos, Pos)
spanF [] = Nothing
spanF xs = Just (beg $ head xs, end $ last xs)

-- | Potential of a given forest.
phiForest :: Nerf a -> Forest a -> Phi
phiForest nerf f = product [phiTree nerf t | t <- f]

-- | Gamma computation details.
data Gamma a = Gamma
    { gammaOut    :: a
    , gammaPlus   :: a -> a -> a
    , gammaMult   :: a -> Maybe a -> a
    , gammaConcat :: [a] -> a
    , gammaTree   :: Pos -> Pos -> Maybe a }

gammaSum :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a -> Gamma Phi
gammaSum active nerf = Gamma one (.+.) mul sum (sumPhiSpan active nerf)
  where
    mul x (Just y) = x .*. y
    mul _ Nothing  = zero

gammaMax :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a -> Gamma Phi
gammaMax active nerf = Gamma one max mul maximum (maxPhiSpan active nerf)
  where
    mul x (Just y) = x .*. y
    mul _ Nothing  = zero

gamma :: (Ord a, Memo.HasTrie a)
      => Gamma b -> Active a -> Nerf a
      -> Pos -> Pos -> b
gamma Gamma{..} active nerf = gammaM
  where
    gammaM = Memo.memo2 gamma'
    gamma' i j
        | i > j     = gammaOut
        | otherwise = gammaM i (j - 1) `gammaPlus` gammaConcat
            [gammaM i (k - 1) `gammaMult` gammaTree k j  | k <- [i..j]]

sumPhiF :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a -> Pos -> Pos -> Phi
sumPhiF active nerf = gamma (gammaSum active nerf) active nerf

sumPhiF' :: Ord a => Active a -> Nerf a -> Pos -> Pos -> Phi
sumPhiF' active nerf i j = sum 
    [phiForest nerf f | f <- forestSet active nerf i j]

maxPhiF :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a -> Pos -> Pos -> Phi
maxPhiF active nerf = gamma (gammaMax active nerf) active nerf

maxPhiF' :: Ord a => Active a -> Nerf a -> Pos -> Pos -> Phi
maxPhiF' active nerf i j = maximum
    [phiForest nerf f | f <- forestSet active nerf i j]

-- | Probability of a given forest with respect to a given range.
probForest :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a
           -> Pos -> Pos -> Forest a -> Phi
probForest active nerf p q f
    | i >= p && j <= q = phiForest nerf f ./. sumPhiF active nerf p q
    | otherwise = error "probForest: forest outside the range"
  where
    (i, j) = case spanF f of
        Just (i, j) -> (i, j)
        Nothing     -> (p, q)

--
-- QuickCheck properties
--

propForestOrd :: (Show a, Ord a) => TestPoint a -> Bool 
propForestOrd test =
    and . map checkForest $ forestSet active nerf p q
  where
    (p, q) = testSpan test
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test

propSumPhiF :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propSumPhiF test =
    trace (show (y, y')) (y ~== y')
  where
    y  = Just $ sumPhiF  active nerf i j
    y' = Just $ sumPhiF' active nerf i j
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    (i, j, _x) = testComp test

propMaxPositive :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propMaxPositive test = maxPhiF active nerf i j >= 0
  where
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    (i, j, _x) = testComp test

propMaxPhiF :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propMaxPhiF test =
    trace (show (y, y')) (y ~== y')
  where
    y  = Just $ maxPhiF  active nerf i j
    y' = Just $ maxPhiF' active nerf i j
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    (i, j, _x) = testComp test
