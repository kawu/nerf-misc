{-# LANGUAGE RecordWildCards #-}

module Proof.Nerf
( Nerf (..)
, Phi  
, module Proof.LogMath  
, module Proof.Base
, module Proof.Rule
) where

import Proof.LogMath
import Proof.Base
import Proof.Rule
  
type Phi = LogDouble

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
