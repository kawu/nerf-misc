{-# LANGUAGE RecordWildCards #-}

module Proof.Nerf
( module Proof.LogMath  
, Pos
, Phi  
, Feature (..)
, Rule (..)
, RulePos
, Nerf (..)
) where

import Proof.LogMath

-- | Position in a sentence (positive).
type Pos = Int
  
-- | Type synonym for potential. 
type Phi = LogDouble

data Rule a = Rule
    { left  :: a
    , top   :: a
    , right :: a }
    deriving (Show, Eq, Ord)

-- | Data representing position of rule application.
type RulePos = (Pos, Pos, Pos)

-- | Feature component functions tell, if on a given tree
-- position (base or rule) the feature is present. Note, that
-- this is an abstract representation of model feature and it
-- will be used only in the context of QuickCheck.
data Feature a = Feature
    { featBase :: Pos -> a -> Bool
    , featRule :: RulePos -> Rule a -> Bool }

data Nerf a = Nerf
    -- | List of (distinct) labels.
    { labels    :: [a]

    -- | List of rules with a top/left/right component equall to given value.
    , perTop    :: a -> [Rule a]
    , perLeft   :: a -> [Rule a]
    , perRight  :: a -> [Rule a]

    -- | Potential values.
    , phiBase   :: Pos -> a -> Phi
    , phiRule   :: RulePos -> Rule a -> Phi

    -- | Active strategy can be used to reduce the number of relevant trees.
    -- The treeSet function (see Proof.Tree module) is defined on top of the
    -- active strategy.
    , active    :: Pos -> Pos -> a -> Bool }
