module Data.BinTree
( Tree (..)
, Forest (..)
, binarize
, unBinarize
, mkRose
) where

import Data.List (foldl1)
import qualified Data.AnnTree as R

-- | Binary annotation tree with @a@ internal labels and additional @b@
-- values in leaves.  Invariant: label from internal node is not O. 
data Tree a b = Node a (Tree a b) (Tree a b)
              | Leaf b
              deriving (Eq, Ord, Show)
type Forest a b = [Tree a b]

-- | Leaf compound label.
type LL a = [a]

-- | Internal node compound label.
data NL a = Path [a]
          | Dummy a
          deriving (Eq, Ord, Show)

{- TODO: There is an alternative NL representation:
 -
 -   data NL a = Path [a]
 -             | Dummy [a]
 -             deriving (Eq, Ord, Show)
 -  
 - where Dummy compound label is equall to its parent label. Try it! -}

-- | Transform rose tree into a binary tree.  When internal node has only one
-- child, the two nodes are joined.
binarize :: R.Tree a b -> Tree (NL a) (LL a, b)
binarize (R.Leaf y) = Leaf ([], y)
binarize (R.Node x [t]) = case binarize t of
    Leaf (xs, y)         -> Leaf (x:xs, y)
    Node (Path xs) tl tr -> Node (Path (x:xs)) tl tr
binarize (R.Node x ts) =
    mkPath $ foldl1 update $ map binarize ts
  where
    update t t' = Node (Dummy x) t t'
    mkPath (Node (Dummy x) t t') = Node (Path [x]) t t'

unBinarize :: Tree (NL a) (LL a, b) -> R.Tree a b
unBinarize (Leaf ([], y))   = R.Leaf y
unBinarize (Leaf (x:xs, y)) = R.Node x [unBinarize $ Leaf (xs, y)]
unBinarize (Node (Path [x]) tl tr) =
    R.Node x $ map unBinarize $ collect tl ++ [tr]
unBinarize (Node (Path (x:xs)) tl tr) =
    R.Node x [unBinarize $ Node (Path xs) tl tr]

-- | Collect dummy branch children.
collect :: Tree (NL a) (LL a, b) -> [Tree (NL a) (LL a, b)]
collect (Node (Dummy _) tl tr) = collect tl ++ [tr]
collect t                      = [t]

mkRose :: Tree a b -> R.Tree a b
mkRose (Leaf y) = R.Leaf y
mkRose (Node x t t') = R.Node x (map mkRose [t, t'])

