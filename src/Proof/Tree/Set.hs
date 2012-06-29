{-# LANGUAGE RecordWildCards #-}

module Proof.Tree.Set
( treeSet
, treeSetLb  
) where

import Proof.Nerf
import Proof.Tree.Internal
import qualified Proof.Rule as R

-- | Tree set per given span.
treeSet :: Nerf a -> Pos -> Pos -> [Tree a]
treeSet nerf i j = concat
    [ treeSetLb nerf i j x
    | x <- labels nerf ]

-- | Build recursively a set of trees T using given nerf definition.
treeSetLb :: Nerf a -> Pos -> Pos -> a -> [Tree a]
treeSetLb nerf@Nerf{..} i j x
    | i == j = [Leaf x i]
    | i < j  =
        [ Branch x t_l t_r
        | r <- perTop x
        , k <- [i..j-1]
        , active i     k (R.left r)
        , active (k+1) j (R.right r)
        , t_l <- treeSetLb nerf i     k (R.left r)
        , t_r <- treeSetLb nerf (k+1) j (R.right r) ]
    | otherwise = error "treeSet: i > j"

{-
-- | For comparison, without "active strategy" the set of trees T would
-- be defined as follows:
treeSetLb :: Nerf a -> Pos -> Pos -> a -> [Tree a]
treeSetLb nerf@Nerf{..} i j x
    | i == j = [Leaf x i]
    | i < j  =
        [ Branch x t_l t_r
        | r <- perTop x
        , k <- [i..j-1]
        , t_l <- treeSetLb nerf i     k (R.left r)
        , t_r <- treeSetLb nerf (k+1) j (R.right r) ]
    | otherwise = error "treeSet: i > j"
-}
