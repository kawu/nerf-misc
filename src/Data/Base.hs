module Data.Base
( Ob
, Lb
, Ps
, ObMx
) where

type Ob = Int   -- ^ Observation
type Lb = Int   -- ^ Label
type Ps = Int   -- ^ Sentence position

-- | Observation matrix: map from sentence position to a list of observations.
type ObMx = Ps -> [Ob] 
