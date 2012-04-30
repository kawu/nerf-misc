{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.AnnTree
( Tree (..)
, Forest
, drawTree
, drawForest
) where

-- | @AnnTree a b@ represents a tree with @a@ annotation values in internal
-- nodes and @b@ values in leaves.
--
-- Invariants not enforced by data structure:
-- * Node has non-empty list of children
data Tree a b = Node a (Forest a b)
              | Leaf b
              deriving (Show)
type Forest a b = [Tree a b]

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
