module Data.Feature
( features
) where

import Data.Base
import qualified Data.BinTree as T
import Data.BinTree hiding (Tree, Forest)

-- | TODO: Consider storing observation lists in leaves. Perhaps it would
-- make the representation to distant from the situation when we perform
-- expectation computations?
type Tree = T.Tree Lb (Lb, Ps)

rootLb :: Tree -> Lb
rootLb (Leaf (lb, ps)) = lb
rootLb (Node lb _ _)   = lb

-- | TODO: Annotate as unboxed? The same in CRF package?
data Feature = OFeature !Ob !Lb     -- ^ Observation feature
             | RFeature !Lb !Lb !Lb -- ^ Rule (parent, left, right) feature

-- | All features with assigned probabilities for given position.
features :: ObMx -> Tree -> [Feature]
features obMx (Leaf (lb, ps)) = [OFeature ob lb | ob <- obMx ps]
features obMx (Node lb tl tr) = RFeature lb (rootLb tl) (rootLb tr)
                              : features obMx tl
                             ++ features obMx tr
