{-# LANGUAGE RecordWildCards #-}

module Proof.Tree.Beta
( beta
) where  

import qualified Data.MemoTrie as Memo

import Proof.Nerf
import Proof.Tree.Alpha
import Proof.Tree.Internal

-- | Beta computation.  FIXME: take alpha computation as argument (to
-- avoid duplicate computations, e.g. in T.expFeatNum).
beta :: (Ord a, Memo.HasTrie a)
     => AlphaM b -> Nerf a -> Pos -> Pos
     -> Pos -> Pos -> a -> Maybe b
beta comp@AlphaM{..} nerf@Nerf{..} p q i j x =
  case alphaM i j x of
    Just _  -> betaM i j x
    Nothing -> Nothing
  where
    betaM = Memo.memo3 beta'
    beta' i j x
        | i == p && j == q =
            case alphaM p q x of      -- ^ TODO: Is this test necessary?
                Just _  -> alphaRootM
                Nothing -> Nothing
        | i >= p && j <= q && i <= j = alphaConcatM
            [ alphaConcatM
                [ alphaRuleM
                    (alphaM k (i-1) (left r))
                    (betaM  k j     (top r))
                    (phiRule (k, i-1, j) r)
                | r <- perRight x
                , k <- [p .. i-1]
                , active k (i-1) (left r)
                , active i j     x ]
            , alphaConcatM
                [ alphaRuleM  
                    (alphaM (j+1) k (right r))
                    (betaM  i     k (top r))
                    (phiRule (i, j, k) r)
                | r <- perLeft x
                , k <- [j+1 .. q]
                , active i     j x
                , active (j+1) k (right r) ] ]
        | otherwise = error "beta: bad arguments"
    alphaM = alpha comp nerf