{-# LANGUAGE RecordWildCards #-}

module Proof.Forest.Gamma
( Gamma (..)
, gamma
, gammaSum
, gammaMax  
) where

import qualified Data.MemoTrie as Memo
  
import Proof.Nerf
import qualified Proof.Tree as T
  
-- | Gamma computation details.
data Gamma a = Gamma
    { gammaOut    :: a
    , gammaPlus   :: a -> a -> a
    , gammaMult   :: a -> Maybe a -> a
    , gammaConcat :: [a] -> a
    , gammaTree   :: Pos -> Pos -> Maybe a }

gamma :: (Ord a, Memo.HasTrie a) => Gamma b -> Nerf a -> Pos -> Pos -> b
gamma Gamma{..} nerf = gammaM
  where
    gammaM = Memo.memo2 gamma'
    gamma' i j
        | i > j     = gammaOut
        | otherwise = gammaM i (j - 1) `gammaPlus` gammaConcat
            [gammaM i (k - 1) `gammaMult` gammaTree k j  | k <- [i..j]]

-- | FIXME: T.sumPhi is always Just, is there a way to implement
-- this function more naturally?
gammaSum :: (Ord a, Memo.HasTrie a) => Nerf a -> Gamma Phi
gammaSum nerf = Gamma one (+) mul sum tree 
  where
    tree i j = Just $ T.sumPhi nerf i j
    mul x (Just y) = x * y
    mul _ Nothing  = zero

gammaMax :: (Ord a, Memo.HasTrie a) => Nerf a -> Gamma Phi
gammaMax nerf = Gamma one max mul maximum (T.maxPhi nerf)
  where
    mul x (Just y) = x * y
    mul _ Nothing  = zero
