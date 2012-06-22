{-# LANGUAGE RecordWildCards #-}

module Proof.Tree
( Pos
, Tree (..)
, size
, Rule (..)
, Nerf (..)
, NerfDesc (..)
, nerfFromDesc
, treeSet
, alpha
, alpha'
, propAlpha
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MemoTrie as Memo
import Data.List (intercalate)
import Control.Monad (forM_)
import Control.Monad.Writer (execWriter, tell)
import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck

import Debug.Trace (trace)

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

-- | Arbitrary set of maximum kMax elements (minimum 1).
arbitrarySet :: Ord a => Int -> Gen a -> Gen [a]
arbitrarySet kMax gen = do
    k <- choose (1, kMax)
    nub <$> vectorOf k gen

-- | QuickCheck parameters.
posMax   = 6
labelMax = 4
ruleMax  = 10
phiMax   = 10.0
descMax  = 100

-- | Position in a sentence (positive).
type Pos = Int

-- | Value of potential (in logarithmic scale).
type Phi = Double

-- | Multiplication in logarithmic scale.
(.*.) :: Phi -> Phi -> Phi
(.*.) = (+)

arbitraryPos :: Gen Pos
arbitraryPos = choose (1, posMax)

arbitraryRan :: Gen (Pos, Pos)
arbitraryRan =
    let pair = (,) <$> arbitraryPos <*> arbitraryPos
    in  pair `suchThat` \(i, j) -> i <= j

arbitraryValue :: Gen Phi
arbitraryValue = choose (-phiMax, phiMax)

-- | Binary tree.
data Tree a
    = Branch { label  :: a
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
        n <- arbitraryPos
        arbitraryOn (1, n)
      where
        arbitraryOn (p, q)
            | p == q    = Leaf <$> arbitrary <*> return p
            | otherwise = do
                k <- choose (p, q - 1)
                Branch <$> arbitrary
                       <*> arbitraryOn (p, k)
                       <*> arbitraryOn (k+1, q)

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

mkTreeP :: Tree a -> TreeP a
mkTreeP Leaf{..}   = LeafP label pos
mkTreeP Branch{..} = BranchP label leftP rightP (p, q)
  where
    leftP  = mkTreeP leftT
    rightP = mkTreeP rightT
    (p, q) = (begP leftP, endP rightP)

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

arbitraryRulePos :: Gen RulePos
arbitraryRulePos =
    let triplet = (,,) <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos
    in  triplet `suchThat` \(i, k, j) -> i <= k && k < j

data Nerf a = Nerf
    { labels  :: [a]
    , perTop  :: a -> [Rule a]
    -- | Potential values (in logarithmic scale).
    , phiBase :: Pos -> a -> Phi
    , phiRule :: RulePos -> Rule a -> Phi }

phiTree :: Nerf a -> Tree a -> Phi
phiTree nerf = phiTreeP nerf . mkTreeP

phiTreeP :: Nerf a -> TreeP a -> Phi
phiTreeP nerf LeafP{..} = phiBase nerf posP labelP
phiTreeP nerf root
    = phiRule nerf rulePos rule
    .*. phiTreeP nerf left
    .*. phiTreeP nerf right
  where
    rulePos = (begP left, endP left, endP right)
    rule    = Rule (labelP left) (labelP root) (labelP right)
    left    = leftP root
    right   = rightP root

data NerfDesc a = NerfDesc
    { labelsD  :: [a]
    , perTopD  :: M.Map a [Rule a]
    , phiBaseD :: M.Map (Pos, a) Phi
    , phiRuleD :: M.Map (RulePos, Rule a) Phi }

instance Show a => Show (NerfDesc a) where
    show NerfDesc{..} = execWriter $ do
        tell $ "labels: " ++ show labelsD
        tell "\n"

        tell "\nrules:\n"
        forM_ (concat $ M.elems perTopD) $ \rule -> do
            tell (show rule) >> tell "\n"

        tell "\nphiBase:\n"
        forM_ (M.toList phiBaseD) $ \(x, y) -> do
            tell (show x)
            tell " => " 
            tell (show y)
            tell "\n"

        tell "\nphiRule:\n"
        forM_ (M.toList phiRuleD) $ \(x, y) -> do
            tell (show x)
            tell " => " 
            tell (show y)
            tell "\n"

nerfFromDesc :: Ord a => NerfDesc a -> Nerf a
nerfFromDesc NerfDesc{..} = Nerf labels perTop phiBase phiRule
  where
    labels = labelsD
    perTop x = case x `M.lookup` perTopD of
        Just xs -> xs
        Nothing -> []
    phiBase = phiWith phiBaseD
    phiRule = phiWith phiRuleD
    phiWith :: (Ord a, Ord b) => M.Map (a, b) Phi -> a -> b -> Phi
    phiWith phiMap i x = case (i, x) `M.lookup` phiMap of
        Just v  -> v
        Nothing -> 0.0  -- log 1.0

arbitraryPhiBase :: [a] -> Gen (Pos, a)
arbitraryPhiBase labels = (,) <$> arbitraryPos <*> elements labels 

arbitraryPhiRule :: [a] -> Gen (RulePos, Rule a)
arbitraryPhiRule labels = (,) <$> arbitraryRulePos <*> arbitraryRule labels

instance (Ord a, Arbitrary a) => Arbitrary (NerfDesc a) where
    arbitrary = do
        labels <- arbitrarySet labelMax arbitrary
        rules  <- arbitrarySet ruleMax (arbitraryRule labels)
        let perTop = M.fromListWith (++) [(top x, [x]) | x <- rules]
        phiBase <- phiWith labels arbitraryPhiBase
        phiRule <- phiWith labels arbitraryPhiRule
        return $ NerfDesc labels perTop phiBase phiRule
      where
        phiWith :: Ord b => [a] -> ([a] -> Gen b) -> Gen (M.Map b Phi)
        phiWith labels gen = do
            k <- choose (0, descMax)
            xs <- vectorOf k $ gen labels
            vs <- vectorOf k $ arbitraryValue
            return $ M.fromList $ zip xs vs

-- | Build recursively a set of trees T using given nerf definition.
treeSet :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> a -> [Tree a]
treeSet nerf i j x
    | i == j = [Leaf x i]
    | i < j  =
        [ Branch x t_l t_r
        | r <- perTop nerf x
        , k <- [i..j-1]
        , t_l <- treeSet nerf i     k (left r)
        , t_r <- treeSet nerf (k+1) j (right r) ]
    | otherwise = error "treeSet: i > j"

alpha :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> a -> Maybe Phi
alpha nerf = Memo.memo3 alpha'
  where
    alpha' i j x
        | i == j = Just $ phiBase nerf i x
        | i < j  = maximumM
            [     alphaNerf i     k (left r)
              .?. alphaNerf (k+1) j (right r)
              .?. Just (phiRule nerf (i, k, j) r)
            | r <- perTop nerf x
            , k <- [i..j-1] ]
        | otherwise = error "alpha: i > j"
    x .?. y = (.*.) <$> x <*> y
    maximumM :: Ord a => [Maybe a] -> Maybe a
    maximumM [] = Nothing
    maximumM xs = maximum xs
    alphaNerf = alpha nerf  -- ^ No memoization without this trick

alpha' :: (Ord a, Memo.HasTrie a) => Nerf a -> Pos -> Pos -> a -> Maybe Phi
alpha' nerf i j x = case null trees of
    False -> Just $ maximum $ map (phiTree nerf) trees
    True  -> Nothing
  where
    trees = treeSet nerf i j x

------------------------------------------------------------------------------

data TestPoint a = TestPoint
    { testNerf  :: NerfDesc a
    , testComp :: (Pos, Pos, a) }

instance Show a => Show (TestPoint a) where
    show TestPoint{..} = execWriter $ do
        tell "=== nerf ===\n"
        tell $ show testNerf 
        tell "\n=== computation ==\n"
        tell $ show testComp

instance (Ord a, Arbitrary a) => Arbitrary (TestPoint a) where
    arbitrary = do
        nerfDesc <- arbitrary
        (i, j) <- arbitraryRan
        x <- elements $ labelsD nerfDesc
        return $ TestPoint nerfDesc (i, j, x)

propAlpha :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propAlpha test =
    trace (show (y, y')) (y ~== y')
  where
    y  = alpha  nerf i j x
    y' = alpha' nerf i j x
    nerf = nerfFromDesc $ testNerf test
    (i, j, x) = testComp test

(~==) :: RealFrac a => Maybe a -> Maybe a -> Bool
Just x ~== Just y =
    x == y || (1 - eps <= z && z <= 1 + eps)
  where
    z = x / y
    eps = 1.0e-10
Nothing ~== Nothing = True
_ ~== _ = False
