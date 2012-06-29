{-# LANGUAGE RecordWildCards #-}

module Proof.Tree.Model
( phi
, prob  
, norm

, maxPhiLb'
, maxPhiLb
, maxPhi'  
, maxPhi 

, sumPhiLb'
, sumPhiLb
, sumPhi'  
, sumPhi 

, maxPhiLbR'
, maxPhiLbR

, sumPhiLbR'
, sumPhiLbR

, featNum
, featNumP
, expFeatNum
) where  

import Prelude hiding (span)
import Data.Maybe (catMaybes, fromJust, maybeToList)
import qualified Data.MemoTrie as Memo

import Proof.Nerf
import Proof.Utils
import Proof.Tree.Internal
import Proof.Tree.Set
import Proof.Tree.Alpha
import Proof.Tree.Beta
  
-- | Tree potential.
phi :: Nerf a -> Tree a -> Phi
phi nerf = phiP nerf . mkTreeP

-- | TreeP potential.
phiP :: Nerf a -> TreeP a -> Phi
phiP Nerf{..} LeafP{..} = phiBase posP labelP
phiP nerf root
    = phiRule nerf rulePos rule 
    * phiP nerf left 
    * phiP nerf right
  where
    rulePos = (begP left, endP left, endP right)
    rule    = Rule (labelP left) (labelP root) (labelP right)
    left    = leftP root
    right   = rightP root

-- | Probability of a tree.
prob :: (Ord a, Memo.HasTrie a) => Nerf a -> Tree a -> LogDouble
prob nerf tree =
    let (p, q) = span tree  
    in  phi nerf tree / sumPhi nerf p q

-- | Find tree with a maximum potential (base version).
maxPhiLb' :: Ord a => Nerf a -> Pos -> Pos -> a -> Maybe Phi
maxPhiLb' nerf i j x = catchNull maximum 
    [phi nerf t | t <- treeSetLb nerf i j x]

maxPhi' :: Ord a => Nerf a -> Pos -> Pos -> Maybe Phi
maxPhi' nerf i j = catchNull maximum $ catMaybes
    [maxPhiLb' nerf i j x | x <- labels nerf]

-- | Find tree with a maximum potential (alpha computation version).
maxPhiLb :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> a -> Maybe Phi
maxPhiLb = alpha alphaMax

maxPhi :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> Maybe Phi
maxPhi nerf i j = catchNull maximum $ catMaybes
    [maxPhiLb nerf i j x | x <- labels nerf]

-- | Sum potentials of all relevant trees (base version).
sumPhiLb' :: Ord a => Nerf a -> Pos -> Pos -> a -> Phi
sumPhiLb' nerf i j x = sum 
    [phi nerf t | t <- treeSetLb nerf i j x]

sumPhi' :: Ord a => Nerf a -> Pos -> Pos -> Phi
sumPhi' nerf i j = sum
    [sumPhiLb' nerf i j x | x <- labels nerf]

-- | Sum potentials of all relevant trees (alpha computation version).
sumPhiLb :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> a -> Phi
sumPhiLb nerf i j x = 
  case doIt i j x of
    Just y  -> y
    Nothing -> 0
  where                         
    doIt = alpha alphaSum nerf

sumPhi :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> Phi
sumPhi nerf i j = sum
    [sumPhiLb nerf i j x | x <- labels nerf]
    
norm :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> Phi
norm = sumPhi

-- | Maximum potential without a subtree on a given value-span (base version).
maxPhiLbR' :: Ord a => Nerf a -> Pos -> Pos -> Pos -> Pos -> a -> Maybe Phi
maxPhiLbR' nerf p q i j x = catchNull maximum
    [ phi nerf t / phi nerf t'
    | y  <- labels nerf
    , t  <- treeSetLb nerf p q y
    , t' <- maybeToList $ subTree t i j
    , label t' == x ]

-- | Maximum potential without a subtree on a given value-span (beta version).
maxPhiLbR :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos
          -> Pos -> Pos -> a -> Maybe Phi
maxPhiLbR = beta alphaMax

-- | Sum of potentials without a subtree on a given value-span (base version).
-- FIXME: It is a correct definition, but it could be simplified.
sumPhiLbR' :: Ord a => Nerf a -> Pos -> Pos -> Pos -> Pos -> a -> Phi
sumPhiLbR' nerf p q i j x = sum
    [ phi nerf t / phi nerf t'
    | t' <- takeOne $ treeSetLb nerf i j x
    , y  <- labels nerf        
    , t  <- treeSetLb nerf p q y
    , t' `isSubTree` t ]
  where
    takeOne [] = []
    takeOne xs = [head xs]

-- | Sum of potentials without a subtree on a given value-span (beta version).
sumPhiLbR :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos
          -> Pos -> Pos -> a -> Phi
sumPhiLbR nerf p q i j x =
  case doIt p q i j x of
    Just y  -> y
    Nothing -> 0
  where                         
    doIt = beta alphaSum nerf

-- | Number of features of given type in a tree.
featNum :: Feature a -> Tree a -> LogDouble
featNum feat = fromIntegral . featNumP feat . mkTreeP

-- | Number of features of a given type in a (position) tree.
featNumP :: Feature a -> TreeP a -> Int
featNumP Feature{..} LeafP{..} = fromEnum $ featBase posP labelP
featNumP feat root
    = fromEnum (featRule feat rulePos rule)
    + featNumP feat left
    + featNumP feat right
  where
    rulePos = (begP left, endP left, endP right)
    rule    = Rule (labelP left) (labelP root) (labelP right)
    left    = leftP root
    right   = rightP root

-- | Expected number of features per given tree span.  FIXME: Optimize it, 
-- use alpha and beta computations.
expFeatNum :: (Ord a, Memo.HasTrie a) => Nerf a 
           -> Feature a -> Pos -> Pos -> LogDouble
expFeatNum nerf feat i j = sum
    [ prob nerf tree * featNum feat tree
    | tree <- treeSet nerf i j ]
