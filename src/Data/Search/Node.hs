module Data.Search.Node
( Node (..)
) where

import Data.Base (Ob, Lb, Ps)

data Node = Node
    { beg   :: !Ps
    , label :: !Lb
    , end   :: !Ps }
    deriving (Show, Eq)

-- | TODO: Give compare definition instead of (<=)?
instance Ord Node where
    Node p _ q <= Node p' _ q'
        | q == q'   = p <= p'
        | otherwise = q <= q'

