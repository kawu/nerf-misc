{-# LANGUAGE RecordWildCards #-}

module Proof.Forest.Model
( phi
, prob  
, sumPhi
, sumPhi'
, maxPhi
, maxPhi'
, featureNum
) where

import Prelude hiding (span)
import qualified Data.MemoTrie as Memo

import Proof.Nerf
import qualified Proof.Tree as T
import qualified Proof.Feature as F
import Proof.Forest.Internal
import Proof.Forest.Set
import Proof.Forest.Gamma
  
-- | Potential of a given forest.
phi :: Nerf a -> Forest a -> Phi
phi nerf f = product [T.phi nerf t | t <- f]

-- | Probability of a given forest with respect to a given range.
prob :: (Ord a, Memo.HasTrie a) => Nerf a
     -> Pos -> Pos -> Forest a -> LogDouble
prob nerf p q f
    | i >= p && j <= q = phi nerf f / sumPhi nerf p q
    | otherwise = error "probForest: forest outside the range"
  where
    (i, j) = case span f of
        Just (i, j) -> (i, j)
        Nothing     -> (p, q)

sumPhi' :: Ord a => Nerf a -> Pos -> Pos -> Phi
sumPhi' nerf i j = sum 
    [phi nerf f | f <- forestSet nerf i j]

sumPhi :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> Phi
sumPhi nerf = gamma (gammaSum nerf) nerf

maxPhi' :: Ord a => Nerf a -> Pos -> Pos -> Phi
maxPhi' nerf i j = maximum
    [phi nerf f | f <- forestSet nerf i j]

maxPhi :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> Phi
maxPhi nerf = gamma (gammaMax nerf) nerf

-- | FIXME: Tree-level featureNum is in Feature module, this is in
-- Forest.Model module. This is inconsistent.
featureNum :: F.Feature a -> Forest a -> LogDouble
featureNum feat =
    fromIntegral . sum . map (F.featureNumP feat . T.mkTreeP)
