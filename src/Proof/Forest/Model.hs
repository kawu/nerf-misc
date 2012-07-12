{-# LANGUAGE RecordWildCards #-}

module Proof.Forest.Model
( phi
, prob  
, normZ
, normN

, probTree
, probTree'
, probSpan
, probSpan'

, sumPhi
, sumPhi'
, maxPhi
, maxPhi'

, featNum
, expFeatNum
, expFeatNum'
) where

import Prelude hiding (span)
import qualified Data.MemoTrie as Memo

import Proof.Nerf
import Proof.Forest.Internal
import Proof.Forest.Set
import Proof.Forest.Gamma
import qualified Proof.Tree as T
  
-- | Potential of a given forest.
phi :: Nerf a -> Forest a -> Phi
phi nerf f = product [T.phi nerf t | t <- f]

-- | Probability of a given forest with respect to a given range.
prob :: (Ord a, Memo.HasTrie a) => Nerf a
     -> Pos -> Pos -> Forest a -> LogDouble
prob nerf p q f
    | i >= p && j <= q = phi nerf f / normZ nerf p q
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

normZ :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> Phi
normZ = sumPhi

-- | Forest-level normalization factor.
normN :: (Ord a, Memo.HasTrie a) => Nerf a
      -> Pos -> Pos -> Pos -> Pos -> LogDouble
normN nerf p q i j =
    let z = normZ nerf
    in  z p (i-1) * z (j+1) q / z p q

-- | Probability, that the tree is located on the given span (base definition).
probTree' :: (Ord a, Memo.HasTrie a) => Nerf a -> T.Tree a
          -> Pos -> Pos -> Pos -> Pos -> LogDouble
probTree' nerf tree p q i j = sum
    [ prob nerf p q forest
    | forest <- forestSet nerf p q
    , forest `hasTree` tree ]
        
-- | Probability, that the tree is located on the given span.  
probTree :: (Ord a, Memo.HasTrie a) => Nerf a -> T.Tree a
         -> Pos -> Pos -> Pos -> Pos -> LogDouble
probTree nerf tree p q i j =
    let n = normN nerf
    in  T.phi nerf tree * n p q i j 

-- | Probability, that some tree is located on the given span (base definition).
probSpan' :: (Ord a, Memo.HasTrie a) => Nerf a
          -> Pos -> Pos -> Pos -> Pos -> LogDouble
probSpan' nerf p q i j = sum
    [ probTree nerf tree p q i j
    | tree <- T.treeSet nerf i j ]

-- | Probability, that some tree is located on the given span. 
probSpan :: (Ord a, Memo.HasTrie a) => Nerf a
         -> Pos -> Pos -> Pos -> Pos -> LogDouble
probSpan nerf p q i j = 
    z i j * n p q i j        
  where
    z = T.norm nerf
    n = normN nerf

-- | Number of features of given type in a forest.
featNum :: Feature a -> Forest a -> LogDouble
featNum feat =
    fromIntegral . sum . map (T.featNumP feat . T.mkTreeP)

-- | Expected number of features in a sentence (base version).
expFeatNum' :: (Ord a, Memo.HasTrie a) => Nerf a 
            -> Feature a -> Pos -> Pos -> LogDouble
expFeatNum' nerf feat p q = sum
    [ prob nerf p q forest * featNum feat forest
    | forest <- forestSet nerf p q ]

-- | Expected number of features in a sentence (optimized version).
expFeatNum :: (Ord a, Memo.HasTrie a) => Nerf a 
           -> Feature a -> Pos -> Pos -> LogDouble
expFeatNum nerf feat p q = sum
    [ probSpan' i j * expFeatNum' feat i j
    | i <- [p..q], j <- [i..q] ]
  where
    expFeatNum' = T.expFeatNum nerf
    probSpan'   = probSpan nerf p q   -- ^ TODO: Remove p, q args?
