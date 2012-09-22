{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.AnnTree
( Tree (..)
, Forest
, drawTree
, drawForest
, size
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Test.QuickCheck hiding (label)

-- | @AnnTree a b@ represents a tree with @a@ annotation values in internal
-- nodes and @b@ values in leaves.
--
-- Invariants not enforced by data structure:
-- * Node has non-empty list of children
data Tree a b = Node a (Forest a b)
              | Leaf b
              deriving (Show, Eq, Ord)
type Forest a b = [Tree a b]

size :: Tree a b -> Int
size (Leaf _) = 1
size (Node _ ts) = 1 + sum (map size ts)

-- | Pretty printing.
class (Show a) => Label a where
    label :: a -> String

instance Label String where label x = x
instance Label Char where label x = [x]
instance (Show a) => Label a where label = show

drawTree :: (Label a, Label b) => Tree a b -> String
drawTree (Leaf x) = label x
drawTree (Node x f) =
    let indent = unlines . map ("  "++) . lines
    in  concat (label x : "\n" : map (indent.drawTree) f)

drawForest :: (Label a, Label b) => Forest a b -> String
drawForest = unlines . map drawTree

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tree a b) where
    arbitrary = sized tree where
        tree 0 = Leaf <$> arbitrary
        tree n = oneof [leaf, node n]
        leaf   = Leaf <$> arbitrary
        node n = do
            k <- choose (1, 10)
            let subtree = tree ((n-1) `div` k)
            Node <$> arbitrary <*> replicateM k subtree 
