module Proof.Tree
( Pos
, Tree (..)
, size
, Rule (..)
, Nerf (..)
, NerfDesc (..)
, nerfFromDesc
, treeSet
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MemoTrie as Memo
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

-- | Arbitrary set of maximum kMax elements (minimum 1).
arbitrarySet :: Ord a => Int -> Gen a -> Gen [a]
arbitrarySet kMax gen = do
    k <- choose (1, kMax)
    nub <$> vectorOf k gen

-- | QuickCheck parameters.
posMax   = 5
labelMax = 5
ruleMax  = 10
phiMax   = 10
valMax   = 5.0

-- | Position in a sentence.
type Pos = Int

arbitraryPos :: Pos -> Gen Pos
arbitraryPos kMax = choose (1, kMax)

arbitraryValue :: Gen Double
arbitraryValue = choose (-valMax, valMax)

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
        n <- arbitraryPos posMax
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

-- | Data representing position of rule application.
type RulePos = (Pos, Pos, Pos)

arbitraryRule :: [a] -> Gen (Rule a)
arbitraryRule xs =
    let x = elements xs
    in  Rule <$> x <*> x<*> x

-- | FIXME: this is incorrect definition!!!!
arbitraryRulePos :: Pos -> Gen RulePos
arbitraryRulePos kMax = (,,)
    <$> arbitraryPos kMax
    <*> arbitraryPos kMax
    <*> arbitraryPos kMax

data Nerf a = Nerf
    { labels  :: [a]
    , perTop  :: a -> [Rule a]
    , phiBase :: Pos -> a -> Double
    , phiRule :: RulePos -> Rule a -> Double }

data NerfDesc a = NerfDesc
    { labelsD  :: [a]
    , perTopD  :: M.Map a [Rule a]
    , phiBaseD :: M.Map (Pos, a) Double
    , phiRuleD :: M.Map (RulePos, Rule a) Double }
    deriving Show

nerfFromDesc :: Ord a => NerfDesc a -> Nerf a
nerfFromDesc = undefined

arbitraryPhiBase :: Pos -> [a] -> Gen (Pos, a)
arbitraryPhiBase kMax labels =
    (,) <$> arbitraryPos kMax <*> elements labels 

arbitraryPhiRule :: Pos -> [a] -> Gen (RulePos, Rule a)
arbitraryPhiRule kMax labels =
    (,) <$> arbitraryRulePos kMax <*> arbitraryRule labels

instance (Ord a, Arbitrary a) => Arbitrary (NerfDesc a) where
    arbitrary = do
        labels <- arbitrarySet labelMax arbitrary
        rules  <- arbitrarySet ruleMax (arbitraryRule labels)
        let perTop = M.fromListWith (++) [(top x, [x]) | x <- rules]
        phiBase <- phiWith labels arbitraryPhiBase
        phiRule <- phiWith labels arbitraryPhiRule
        return $ NerfDesc labels perTop phiBase phiRule
      where
        phiWith :: Ord b => [a] -> (Pos -> [a] -> Gen b)
                -> Gen (M.Map b Double)
        phiWith labels gen = do
            k <- choose (0, phiMax)
            xs <- vectorOf k $ gen posMax labels
            vs <- vectorOf k $ arbitraryValue
            return $ M.fromList $ zip xs vs

-- | Build recursively a set of trees T using given rules set.
treeSet :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> a -> [Tree a]
treeSet nerf = Memo.memo3 treeSet'
  where
    treeSet' i j x
        | i == j = [Leaf x i]
        | i < j  =
            [ Branch x t_l t_r
            | r <- perTop nerf x
            , k <- [i..j-1]
            , t_l <- treeSet nerf i     k (left r)
            , t_r <- treeSet nerf (k+1) j (right r) ]
        | otherwise = error "treeSet: i > j"

-- alpha :: (Ord a, Memo.HasTrie a) => RuleSet a -> Pos -> Pos -> a -> Double
-- alpha rules = Memo.memo3 alpha'
--   where
--     alpha' i j x
--         | i == j = phiBase i x
--         | i < j  = maximum
--             [     phiRule (i, k, j) r
--               .*. alpha rules i     k (left r)
--               .*. alpha rules (k+1) j (right r)
--             | r <- justList (x `M.lookup` perTop rules)
--             , k <- [i..j-1] ]
--         | otherwise = error "alpha: i > j"
-- 
-- alpha' :: (Ord a, Memo.HasTrie a) => RuleSet a -> Pos -> Pos -> a -> Double
-- alpha' rules i j x = maximum $ map phiTree $ treeSet rules i j x
-- 
-- phiTree = undefined
-- phiBase = undefined
-- phiRule = undefined
-- 
-- (.*.) :: Num a => a -> a -> a
-- (.*.) = (+)
