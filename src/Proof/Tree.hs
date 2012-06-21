module Proof.Tree
( Pos
, Tree (..)
, size
, Rule (..)
, RuleSet (..)
, treeSet
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MemoTrie as Memo
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

-- | Arbitrary set of maximum k elements (minimum 1).
arbitrarySet :: Ord a => Int -> Gen a -> Gen [a]
arbitrarySet k gen = nub <$> vectorOf k gen

-- | QuickCheck parameters.
leafMax = 10
ruleMax = 10

-- | Position in a sentence.
type Pos = Int

-- | Binary tree.
data Tree a = Branch { label  :: a
                     , leftT  :: (Tree a)
                     , rightT :: (Tree a) }
            | Leaf   { label  :: a
                     , pos    :: Pos }
            deriving Show

size :: Tree a -> Int
size Leaf{} = 1
size (Branch _ l r) = 1 + size l + size r

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        n <- choose (1, leafMax)
        arbitraryOn (1, n)
      where
        arbitraryOn (p, q)
            | p == q    = Leaf <$> arbitrary <*> return p
            | otherwise = do
                k <- choose (p, q - 1)
                Branch <$> arbitrary
                       <*> arbitraryOn (p, k)
                       <*> arbitraryOn (k+1, q)

data Rule a = Rule
    { left  :: a
    , top   :: a
    , right :: a }
    deriving (Show, Eq, Ord)

instance Arbitrary a => Arbitrary (Rule a) where
    arbitrary = Rule <$> arbitrary <*> arbitrary <*> arbitrary

newtype RuleSet a = RuleSet
    { perTop :: M.Map a [Rule a] }
    deriving Show

instance (Ord a, Arbitrary a) => Arbitrary (RuleSet a) where
    arbitrary = do
        k <- choose (0, ruleMax)
        xs <- arbitrarySet k arbitrary
        return $ RuleSet $ M.fromListWith (++) [(top x, [x]) | x <- xs]

-- | Build recursively a set of trees T using given rules set.
treeSet :: (Ord a, Memo.HasTrie a) => RuleSet a -> Pos -> Pos -> a -> [Tree a]
treeSet rules = Memo.memo3 treeSet'
  where
    treeSet' i j x
        | i == j = [Leaf x i]
        | i < j  =
            [ Branch x t_l t_r
            | r <- justList (x `M.lookup` perTop rules)
            , k <- [i..j-1]
            , t_l <- treeSet rules i     k (left r)
            , t_r <- treeSet rules (k+1) j (right r) ]
        | otherwise = error "treeSet: i > j"

alpha :: (Ord a, Memo.HasTrie a) => RuleSet a -> Pos -> Pos -> a -> Double
alpha rules = Memo.memo3 alpha'
  where
    alpha' i j x
        | i == j = phiBase i x
        | i < j  = maximum
            [     phiRule (i, k, j) r
              .*. alpha rules i     k (left r)
              .*. alpha rules (k+1) j (right r)
            | r <- justList (x `M.lookup` perTop rules)
            , k <- [i..j-1] ]
        | otherwise = error "alpha: i > j"

alpha' :: (Ord a, Memo.HasTrie a) => RuleSet a -> Pos -> Pos -> a -> Double
alpha' rules i j x = maximum $ map phiTree $ treeSet rules i j x

phiTree = undefined
phiBase = undefined
phiRule = undefined

(.*.) :: Num a => a -> a -> a
(.*.) = (+)

justList :: Maybe [a] -> [a]
justList (Just xs) = xs
justList Nothing   = []
