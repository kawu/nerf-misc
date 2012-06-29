{-# LANGUAGE RecordWildCards #-}

module Proof.Tree.Internal
( Tree (..)
, size
, beg
, end
, span
, subTree
, isSubTree

, TreeP (..)
, begP
, endP
, spanP
, mkTreeP
, unTreeP
) where

import Prelude hiding (span)
import Control.Applicative ((<$>), (<*>), (<|>))

import Proof.Base

-- | Binary tree.
data Tree a
    = Branch { label :: a
             , left  :: (Tree a)
             , right :: (Tree a) }
    | Leaf   { label :: a
             , pos   :: Pos }
    deriving (Show, Eq, Ord)

size :: Tree a -> Int
size Leaf{} = 1
size (Branch _ l r) = 1 + size l + size r

beg :: Tree a -> Pos
beg Leaf{..}    = pos
beg Branch{..}  = beg left 

end :: Tree a -> Pos
end Leaf{..}    = pos
end Branch{..}  = end right 

span :: Tree a -> (Pos, Pos)
span tree = (beg tree, end tree)

-- | Tree adorned with position information.
data TreeP a
    = BranchP { labelP :: a
              , leftP  :: TreeP a
              , rightP :: TreeP a
              , ranP   :: (Pos, Pos) }
    | LeafP   { labelP :: a
              , posP   :: Pos } 
    deriving Show

begP :: TreeP a -> Pos
begP LeafP{..}   = posP
begP BranchP{..} = fst ranP

endP :: TreeP a -> Pos
endP LeafP{..}   = posP
endP BranchP{..} = snd ranP

spanP :: TreeP a -> (Pos, Pos)
spanP = (,) <$> begP <*> endP

mkTreeP :: Tree a -> TreeP a
mkTreeP Leaf{..}   = LeafP label pos
mkTreeP Branch{..} = BranchP label leftP rightP (p, q)
  where
    leftP  = mkTreeP left
    rightP = mkTreeP right
    (p, q) = (begP leftP, endP rightP)

unTreeP :: TreeP a -> Tree a
unTreeP LeafP{..}  = Leaf labelP posP
unTreeP BranchP{..} = Branch labelP (unTreeP leftP) (unTreeP rightP)

-- | Find subtree with a given span.
subTree :: Eq a => Tree a -> Pos -> Pos -> Maybe (Tree a) 
subTree tree i j = unTreeP <$> subTreeP (mkTreeP tree) i j

-- | Find subtree with a given span.
subTreeP :: Eq a => TreeP a -> Pos -> Pos -> Maybe (TreeP a)
subTreeP tree i j
    | p == i && j == q = Just tree
    | p <= i && j <= q =
        subTreeP (leftP tree) i j <|>
        subTreeP (rightP tree) i j
    | otherwise = Nothing
  where
    (p, q) = spanP tree

-- | Check, if one tree is a subtree of the second tree.
isSubTree :: Eq a => Tree a -> Tree a -> Bool
isSubTree s t = case subTree t p q of
    Just s' -> s == s'
    Nothing -> False
  where
    (p, q) = span s