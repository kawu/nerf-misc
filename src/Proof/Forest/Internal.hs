module Proof.Forest.Internal
( Forest
, check  
, span
, hasTree
) where

import Prelude hiding (span)
import Proof.Nerf
import Proof.Tree hiding (span)

-- | Invariant: trees in a forest are preserved in ascending order
-- (where order is defined with respect to leaf positions).
type Forest a = [Tree a]

-- | Check, if forest satisfy basic invariants.
check :: Forest a -> Bool
check f = and [end t < beg t' | (t, t') <- zip f (tail f)]

-- | Tangible span of the forest.
span :: Forest a -> Maybe (Pos, Pos)
span [] = Nothing
span xs = Just (beg $ head xs, end $ last xs)

-- | Check, if forest has a given tree.
hasTree :: Eq a => Forest a -> Tree a -> Bool
hasTree f t = any (t==) f
