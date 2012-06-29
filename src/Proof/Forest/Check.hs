{-# LANGUAGE RecordWildCards #-}

module Proof.Forest.Check
( NerfDesc (..)
, nerfFromDesc
, propForestOrd
, propSumPhi
, propMaxPositive
, propMaxPhi
, Test (..)
) where

import qualified Data.MemoTrie as Memo
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (forM_)
import Control.Monad.Writer (execWriter, tell)
import Control.Applicative ((<$>), (<*>), (<|>))
import Test.QuickCheck

import Proof.Nerf
import Proof.Utils
import Proof.Feature
import qualified Proof.Tree as T

-- | FIXME: When Model is imported, Internal and Set should be 
-- imported automatically. Same for Tree.Model.
import Proof.Forest.Internal
import Proof.Forest.Set
import Proof.Forest.Model
  
-- | QuickCheck parameters.
posMax    = 5
labelMax  = 4
ruleMax   = 100
phiMax    = 10.0
descMax   = 100
activeMax = 25
featMax   = 10

-- | Arbitrary set of maximum kMax elements (minimum 1).
arbitrarySet :: Ord a => Int -> Gen a -> Gen [a]
arbitrarySet kMax gen = do
    k <- choose (1, kMax)
    nub <$> vectorOf k gen

arbitraryPos :: Gen Pos
arbitraryPos = choose (1, posMax)

arbitrarySpan :: Gen (Pos, Pos)
arbitrarySpan =
    let pair = (,) <$> arbitraryPos <*> arbitraryPos
    in  pair `suchThat` \(i, j) -> i <= j

arbitraryPhi :: Gen Phi
arbitraryPhi = LogDouble <$> choose (-phiMax, phiMax)

arbitraryRule :: [a] -> Gen (Rule a)
arbitraryRule xs =
    let x = elements xs
    in  Rule <$> x <*> x<*> x

arbitraryRulePos :: Gen RulePos
arbitraryRulePos =
    let triplet = (,,) <$> arbitraryPos <*> arbitraryPos <*> arbitraryPos
    in  triplet `suchThat` \(i, k, j) -> i <= k && k < j

type Active a = Pos -> Pos -> a -> Bool
type ActiveDesc a = S.Set (Pos, Pos, a)

data NerfDesc a = NerfDesc
    { labelsD  :: [a]
    , rulesD   :: [Rule a]
    , phiBaseD :: M.Map (Pos, a) Phi
    , phiRuleD :: M.Map (RulePos, Rule a) Phi
    , activeD  :: ActiveDesc a }

instance Show a => Show (NerfDesc a) where
    show NerfDesc{..} = execWriter $ do
        tell $ "labels: " ++ show labelsD
        tell "\n"

        tell "\nrules:\n"
        forM_ rulesD $ \rule -> do
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

        tell "\nactive:\n"
        tell (show activeD)

activeFromDesc :: Ord a => S.Set (Pos, Pos, a) -> Active a
activeFromDesc activeSet i j x = (i, j, x) `S.member` activeSet

nerfFromDesc :: Ord a => NerfDesc a -> Nerf a
nerfFromDesc NerfDesc{..} =
    Nerf labels perTop perLeft perRight phiBase phiRule active
  where
    labels = labelsD

    topM = mapWith top
    leftM = mapWith left
    rightM = mapWith right

    perTop = perWith topM
    perLeft = perWith leftM
    perRight = perWith rightM

    phiBase = phiWith phiBaseD
    phiRule = phiWith phiRuleD
    
    active = activeFromDesc activeD

    mapWith f = M.fromListWith (++) [(f x, [x]) | x <- rulesD]
    perWith m x = case x `M.lookup` m of
        Just xs -> xs
        Nothing -> []
    phiWith phiMap i x = case (i, x) `M.lookup` phiMap of
        Just v  -> v
        Nothing -> one

arbitraryPhiBase :: [a] -> Gen (Pos, a)
arbitraryPhiBase labels = (,) <$> arbitraryPos <*> elements labels 

arbitraryPhiRule :: [a] -> Gen (RulePos, Rule a)
arbitraryPhiRule labels = (,) <$> arbitraryRulePos <*> arbitraryRule labels

arbitraryActive :: Ord a => [a] -> Gen (ActiveDesc a)
arbitraryActive xs = S.fromList <$> do
    k <- choose (0, activeMax)
    vectorOf k activeElem
  where
    activeElem = do
        (i, j) <- arbitrarySpan
        x <- elements xs
        return (i, j, x)

instance (Ord a, Arbitrary a) => Arbitrary (NerfDesc a) where
    arbitrary = do
        labels  <- arbitrarySet labelMax arbitrary
        rules   <- arbitrarySet ruleMax (arbitraryRule labels)
        phiBase <- phiWith labels arbitraryPhiBase
        phiRule <- phiWith labels arbitraryPhiRule
        active  <- arbitraryActive labels
        return $ NerfDesc labels rules phiBase phiRule active
      where
        phiWith :: Ord b => [a] -> ([a] -> Gen b) -> Gen (M.Map b Phi)
        phiWith labels gen = do
            k <- choose (0, descMax)
            xs <- vectorOf k $ gen labels
            vs <- vectorOf k $ arbitraryPhi
            return $ M.fromList $ zip xs vs

data FeatDesc a = FeatDesc
    { featBaseD :: M.Map (Pos, a) Bool
    , featRuleD :: M.Map (RulePos, Rule a) Bool }
    deriving Show

featFromDesc :: Ord a => FeatDesc a -> Feature a
featFromDesc FeatDesc{..} = Feature featBase featRule
  where
    featBase = featWith featBaseD
    featRule = featWith featRuleD
    featWith featMap i x = case (i, x) `M.lookup` featMap of
        Just v  -> v
        Nothing -> False

arbitraryFeatDesc :: Ord a => [a] -> Gen (FeatDesc a)
arbitraryFeatDesc labels = do
    featBase <- featWith arbitraryPhiBase
    featRule <- featWith arbitraryPhiRule
    return $ FeatDesc featBase featRule
  where
    featWith gen = do
        k <- choose (0, featMax)
        xs <- vectorOf k (gen labels)
        vs <- vectorOf k arbitrary
        return $ M.fromList $ zip xs vs


------------------------------------------------------------------------------

data Test a = Test
    { testNerf   :: NerfDesc a
    , testSpan   :: (Pos, Pos)
    , testComp   :: (Pos, Pos, a)
    , testTree   :: Maybe (T.Tree a)
    , testFeat   :: FeatDesc a }

instance Show a => Show (Test a) where
    show Test{..} = execWriter $ do
        tell "=== nerf ===\n"
        tell $ show testNerf 
        tell "\n=== test size ==\n"
        tell $ show testSpan
        tell "\n=== computation ==\n"
        tell $ show testComp
        tell "\n=== tree ==\n"
        tell $ show testTree
        tell "\n=== feature ==\n"
        tell $ show testFeat

instance (Ord a, Arbitrary a) => Arbitrary (Test a) where
    arbitrary = do
        nerf   <- arbitrary
        (p, q) <- arbitrarySpan
        (i, j) <- arbitrarySpan `suchThat` \(i, j) -> p <= i && j <= q
        x      <- elements $ labelsD nerf
        tree   <- case T.treeSetLb (nerfFromDesc nerf) i j x of
                    [] -> return Nothing
                    xs -> Just <$> elements xs
        feat   <- arbitraryFeatDesc $ labelsD nerf
        return $ Test nerf (p, q) (i, j, x) tree feat

--
-- QuickCheck properties
--

propForestOrd :: (Show a, Ord a) => Test a -> Bool 
propForestOrd test =
    and . map check $ forestSet nerf p q
  where
    (p, q) = testSpan test
    nerf = nerfFromDesc $ testNerf test

propSumPhi :: (Show a, Ord a, Memo.HasTrie a) => Test a -> Bool
propSumPhi test = (~==)
    (sumPhi  nerf i j)
    (sumPhi' nerf i j)
  where
    nerf = nerfFromDesc $ testNerf test
    (i, j, _x) = testComp test

propMaxPositive :: (Show a, Ord a, Memo.HasTrie a) => Test a -> Bool
propMaxPositive test = maxPhi nerf i j >= 0
  where
    nerf = nerfFromDesc $ testNerf test
    (i, j, _x) = testComp test

propMaxPhi :: (Show a, Ord a, Memo.HasTrie a) => Test a -> Bool
propMaxPhi test = (~==)
    (maxPhi  nerf i j)
    (maxPhi' nerf i j)
  where
    nerf = nerfFromDesc $ testNerf test
    (i, j, _x) = testComp test