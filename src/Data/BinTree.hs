module Data.BinTree
( Tree (..)
, Forest (..)
, binarize
, unBinarize
, mkRose
, pBiject
) where

import Data.List (foldl1)
import qualified Data.AnnTree as R

-- | Binary annotation tree with @a@ internal labels and additional @b@
-- values in leaves.  Invariant: label from internal node is not O. 
data Tree a b = Node a (Tree a b) (Tree a b)
              | Leaf b
              deriving (Show, Eq, Ord)
type Forest a b = [Tree a b]

-- | Transform rose tree into a binary tree.  When internal node has only one
-- child, the two nodes are joined.
binarize :: R.Tree a b -> Tree [a] ([a], b)
binarize (R.Leaf y) = Leaf ([], y)
binarize (R.Node x [t]) = case binarize t of
    Leaf (xs, y)    -> Leaf (x:xs, y)
    Node xs tl tr   -> Node (x:xs) tl tr
-- binarize (R.Node x [t, t']) =
--     Node [x] (binarize t) (binarize t')
binarize (R.Node x ts) =
    mkRoot . foldl1 update $ map binarize ts
  where
    update t t' = Node [] t t'
    mkRoot (Node _ t t') = Node [x] t t'

unBinarize :: Tree [a] ([a], b) -> R.Tree a b
unBinarize (Leaf ([], y))   = R.Leaf y
unBinarize (Leaf (x:xs, y)) = R.Node x [unBinarize $ Leaf (xs, y)]
unBinarize (Node [x] tl tr) =
    R.Node x $ map unBinarize $ collect tl ++ [tr]
unBinarize (Node (x:xs) tl tr) =
    R.Node x [unBinarize $ Node xs tl tr]

-- | Collect dummy branch children.
collect :: Tree [a] ([a], b) -> [Tree [a] ([a], b)]
collect (Node [] tl tr) = collect tl ++ [tr]
collect t               = [t]

mkRose :: Tree a b -> R.Tree a b
mkRose (Leaf y) = R.Leaf y
mkRose (Node x t t') = R.Node x (map mkRose [t, t'])

pBiject :: R.Tree Int Char -> Bool
pBiject x =
    let _id = unBinarize . binarize
    in  x == _id x
