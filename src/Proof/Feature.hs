{-# LANGUAGE RecordWildCards #-}

module Proof.Feature
( Feature (..)
, featureNum
, featureNumP
) where

import Proof.Nerf
import Proof.Tree

-- | Feature component functions tell, if on a given tree
-- position (base or rule) the feature is present. Note, that
-- this is an abstract representation of model feature and it
-- will be used only in the context of QuickCheck.
data Feature a = Feature
    { featBase :: Pos -> a -> Bool
    , featRule :: RulePos -> Rule a -> Bool }

-- | Number of features of given type in a tree.
featureNum :: Feature a -> Tree a -> LogDouble
featureNum feat = fromIntegral . featureNumP feat . mkTreeP

-- | Number of features of a given type in a (position) tree.
featureNumP :: Feature a -> TreeP a -> Int
featureNumP Feature{..} LeafP{..} = fromEnum $ featBase posP labelP
featureNumP feat root
    = fromEnum (featRule feat rulePos rule)
    + featureNumP feat left
    + featureNumP feat right
  where
    rulePos = (begP left, endP left, endP right)
    rule    = Rule (labelP left) (labelP root) (labelP right)
    left    = leftP root
    right   = rightP root
