{-# LANGUAGE RecordWildCards #-}

module Proof.Tree
( Pos
, Phi
, Tree (..)
, size
, beg
, end
, span
, Rule (..)
, Nerf (..)
, NerfDesc (..)
, nerfFromDesc
, Active
, activeFromDesc
, treeSet
, treeSetSpan
, phiTree
, Alpha (..)
, alpha
, beta
, mkAlphaM
, alphaMax
, alphaSum
, maxPhi
, maxPhi'
, sumPhi
, sumPhi'
, sumPhiSpan
, probTree
, maxPhiR
, maxPhiR'
, propMaxPhi
, propMaxPhiR
, propSumPhi
, TestPoint (..)
, catchNull
) where

import Prelude hiding (sum, product, span)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.MemoTrie as Memo
import Data.List (intercalate)
import Data.Maybe (maybeToList, catMaybes, fromJust)
import Control.Monad (forM_)
import Control.Monad.Writer (execWriter, tell)
import Control.Applicative ((<$>), (<*>), (<|>))
import Test.QuickCheck hiding (label, labels)

import Debug.Trace (trace)

import Proof.Utils
import Proof.LogMath

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

-- | Arbitrary set of maximum kMax elements (minimum 1).
arbitrarySet :: Ord a => Int -> Gen a -> Gen [a]
arbitrarySet kMax gen = do
    k <- choose (1, kMax)
    nub <$> vectorOf k gen

-- | QuickCheck parameters.
posMax    = 6
labelMax  = 4
ruleMax   = 10
phiMax    = 10.0
descMax   = 100
activeMax = 100

-- | Position in a sentence (positive).
type Pos = Int

-- | Value of potential (in logarithmic scale).
type Phi = Double

arbitraryPos :: Gen Pos
arbitraryPos = choose (1, posMax)

arbitrarySpan :: Gen (Pos, Pos)
arbitrarySpan =
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
    deriving (Show, Eq, Ord)

size :: Tree a -> Int
size Leaf{} = 1
size (Branch _ l r) = 1 + size l + size r

beg :: Tree a -> Pos
beg Leaf{..}    = pos
beg Branch{..}  = beg leftT 

end :: Tree a -> Pos
end Leaf{..}    = pos
end Branch{..}  = end rightT 

span :: Tree a -> (Pos, Pos)
span tree = (beg tree, end tree)

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

spanP :: TreeP a -> (Pos, Pos)
spanP = (,) <$> begP <*> endP

mkTreeP :: Tree a -> TreeP a
mkTreeP Leaf{..}   = LeafP label pos
mkTreeP Branch{..} = BranchP label leftP rightP (p, q)
  where
    leftP  = mkTreeP leftT
    rightP = mkTreeP rightT
    (p, q) = (begP leftP, endP rightP)

unTreeP :: TreeP a -> Tree a
unTreeP LeafP{..}  = Leaf labelP posP
unTreeP BranchP{..} = Branch labelP (unTreeP leftP) (unTreeP rightP)

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
    { labels   :: [a]
    , perTop   :: a -> [Rule a]
    , perLeft  :: a -> [Rule a]
    , perRight :: a -> [Rule a]
    -- | Potential values (in logarithmic scale).
    , phiBase  :: Pos -> a -> Phi
    , phiRule  :: RulePos -> Rule a -> Phi }

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
    , rulesD   :: [Rule a]
    , phiBaseD :: M.Map (Pos, a) Phi
    , phiRuleD :: M.Map (RulePos, Rule a) Phi }

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

nerfFromDesc :: Ord a => NerfDesc a -> Nerf a
nerfFromDesc NerfDesc{..} =
    Nerf labels perTop perLeft perRight phiBase phiRule
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

    mapWith f = M.fromListWith (++) [(f x, [x]) | x <- rulesD]
    perWith m x = case x `M.lookup` m of
        Just xs -> xs
        Nothing -> []
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
        phiBase <- phiWith labels arbitraryPhiBase
        phiRule <- phiWith labels arbitraryPhiRule
        return $ NerfDesc labels rules phiBase phiRule
      where
        phiWith :: Ord b => [a] -> ([a] -> Gen b) -> Gen (M.Map b Phi)
        phiWith labels gen = do
            k <- choose (0, descMax)
            xs <- vectorOf k $ gen labels
            vs <- vectorOf k $ arbitraryValue
            return $ M.fromList $ zip xs vs

-- | Strategy can be used to reduce the number of trees. Set of trees itself
-- is defined on top of strategy (see treeSet function). Strategy strat should
-- be interpreted as follows: TODO
type Active a = Pos -> Pos -> a -> Bool

newtype ActiveDesc a = ActiveDesc
    { activeSet :: S.Set (Pos, Pos, a) }
    deriving Show

activeFromDesc :: Ord a => ActiveDesc a -> Active a
activeFromDesc ActiveDesc{..} i j x = (i, j, x) `S.member` activeSet

arbitraryActive :: Ord a => [a] -> Gen (ActiveDesc a)
arbitraryActive xs = ActiveDesc . S.fromList <$> do
    k <- choose (0, activeMax)
    vectorOf k activeElem
  where
    activeElem = do
        (i, j) <- arbitrarySpan
        x <- elements xs
        return (i, j, x)

-- | Build recursively a set of trees T using given nerf definition.
treeSet :: Active a -> Nerf a -> Pos -> Pos -> a -> [Tree a]
treeSet active nerf i j x
    | i == j = [Leaf x i]
    | i < j  =
        [ Branch x t_l t_r
        | r <- perTop nerf x
        , k <- [i..j-1]
        , active i     k (left r)
        , active (k+1) j (right r)
        , t_l <- treeSet active nerf i     k (left r)
        , t_r <- treeSet active nerf (k+1) j (right r) ]
    | otherwise = error "treeSet: i > j"

-- | Tree set per given span.
treeSetSpan :: Active a -> Nerf a -> Pos -> Pos -> [Tree a]
treeSetSpan active nerf i j = concat
    [ treeSet active nerf i j x
    | x <- labels nerf ]

-- | Find subtree with a given span.
subTree :: Eq a => Pos -> Pos -> Tree a -> Maybe (Tree a) 
subTree i j tree = unTreeP <$> subTreeP i j (mkTreeP tree)

-- | Find subtree with a given span.
subTreeP :: Eq a => Pos -> Pos -> TreeP a -> Maybe (TreeP a)
subTreeP i j tree
    | p == i && j == q = Just tree
    | p <= i && j <= q =
        subTreeP i j (leftP tree) <|>
        subTreeP i j (rightP tree)
    | otherwise = Nothing
  where
    (p, q) = spanP tree

-- | Operation definitions for alpha (and beta) computations. 
-- With Alpha you can represent algorithms like sum-product
-- or maxarg (finding the most probable tree structure).
-- We assume, that argument of aconcat is non-empty.
data Alpha a = Alpha
    { alphaRoot   :: a  -- ^ For beta computation
    , alphaBase   :: Phi -> a
    , alphaRule   :: a -> a -> Phi -> a
    , alphaConcat :: [a] -> a }

-- | Alpha computations lifted to Maybe setting.
data AlphaM a = AlphaM
    { alphaRootM    :: Maybe a
    , alphaBaseM    :: Phi -> Maybe a
    , alphaRuleM    :: Maybe a -> Maybe a -> Phi -> Maybe a
    , alphaConcatM  :: [Maybe a] -> Maybe a }

mkAlphaM :: Alpha a -> AlphaM a
mkAlphaM Alpha{..} = AlphaM root base rule concat
  where
    root = Just $ alphaRoot
    base = Just . alphaBase
    rule mx my phi = alphaRule <$> mx <*> my <*> Just phi
    concat xs
        | null ys   = Nothing
        | otherwise = Just . alphaConcat $ ys
      where 
        ys = catMaybes xs

-- | Probability of the most probable tree. We assume, that phi
-- values are represented in logarithmic scale.
alphaMax :: AlphaM Phi
alphaMax =
    let rule x y z = x .*. y .*. z
    in  mkAlphaM $ Alpha one id rule maximum

-- | Sum of all tree probabilities.
alphaSum :: AlphaM Phi
alphaSum =
    let rule x y z = x .*. y .*. z
    in  mkAlphaM $ Alpha one id rule sum

alpha :: (Ord a, Memo.HasTrie a)
      => AlphaM b -> Active a -> Nerf a
      -> Pos -> Pos -> a -> Maybe b
alpha AlphaM{..} active Nerf{..} = alphaM
  where
    alphaM = Memo.memo3 alpha'
    alpha' i j x
        | i == j = alphaBaseM $ phiBase i x
        | i < j  = alphaConcatM
            [ alphaRuleM
                (alphaM i     k (left r))
                (alphaM (k+1) j (right r))
                (phiRule (i, k, j) r)
            | r <- perTop x
            , k <- [i..j-1]
            , active i     k (left r)
            , active (k+1) j (right r) ]
        | otherwise = error "alpha: i > j"

-- | Find tree with a maximum potential.
maxPhi :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a
       -> Pos -> Pos -> a -> Maybe Phi
maxPhi = alpha alphaMax

-- | Find tree with a maximum potential -- definition.
maxPhi' :: Ord a => Active a -> Nerf a -> Pos -> Pos -> a -> Maybe Phi
maxPhi' active nerf i j x = catchNull maximum 
    [phiTree nerf t | t <- treeSet active nerf i j x]

sumPhi :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a
       -> Pos -> Pos -> a -> Maybe Phi
sumPhi = alpha alphaSum

sumPhiSpan :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a
           -> Pos -> Pos -> Maybe Phi
sumPhiSpan active nerf i j = catchNull sum $ catMaybes
    [ sumPhi active nerf i j x
    | x <- labels nerf ]

sumPhi' :: Ord a => Active a -> Nerf a -> Pos -> Pos -> a -> Maybe Phi
sumPhi' active nerf i j x = catchNull sum 
    [phiTree nerf t | t <- treeSet active nerf i j x]

-- | Probability of a tree (in logarithimc scale, of course).
probTree :: (Ord a, Memo.HasTrie a) => Active a -> Nerf a -> Tree a -> Phi
probTree active nerf tree =
    phiTree nerf tree ./. z 
  where
    (p, q) = span tree
    z = fromJust $ sumPhiSpan active nerf p q

-- | Beta computation.
beta :: (Ord a, Memo.HasTrie a)
     => AlphaM b -> Active a -> Nerf a -> Pos -> Pos
     -> Pos -> Pos -> a -> Maybe b
beta comp@AlphaM{..} active nerf@Nerf{..} p q i j x =
  case alphaM i j x of
    Just _  -> betaM i j x
    Nothing -> Nothing
  where
    betaM = Memo.memo3 beta'
    beta' i j x
        | i == p && j == q =
            case alphaM p q x of
                Just _  -> alphaRootM
                Nothing -> Nothing
        | i >= p && j <= q && i <= j = alphaConcatM
            [ alphaConcatM
                [ alphaRuleM
                    (alphaM k (i-1) (left r))
                    (betaM  k j     (top r))
                    (phiRule (k, i-1, j) r)
                | r <- perRight x
                , k <- [p .. i-1]
                , active k (i-1) (left r)
                , active i j     x ]
            , alphaConcatM
                [ alphaRuleM  
                    (alphaM (j+1) k (right r))
                    (betaM  i     k (top r))
                    (phiRule (i, j, k) r)
                | r <- perLeft x
                , k <- [j+1 .. q]
                , active i     j x
                , active (j+1) k (right r) ] ]
        | otherwise = error "beta: bad arguments"
    alphaM = alpha comp active nerf

maxPhiR :: (Ord a, Memo.HasTrie a)
        => Active a -> Nerf a -> Pos -> Pos
        -> Pos -> Pos -> a -> Maybe Phi
maxPhiR = beta alphaMax

maxPhiR' :: Ord a
         => Active a -> Nerf a -> Pos -> Pos
         -> Pos -> Pos -> a -> Maybe Phi
maxPhiR' active nerf p q i j x = catchNull maximum
    [ phiTree nerf t ./. phiTree nerf t'
    | y  <- labels nerf
    , t  <- treeSet active nerf p q y
    , t' <- maybeToList $ subTree i j t
    , label t' == x ]

------------------------------------------------------------------------------

data TestPoint a = TestPoint
    { testNerf   :: NerfDesc a
    , testActive :: ActiveDesc a
    , testSpan   :: (Pos, Pos)
    , testComp   :: (Pos, Pos, a) }

instance Show a => Show (TestPoint a) where
    show TestPoint{..} = execWriter $ do
        tell "=== nerf ===\n"
        tell $ show testNerf 
        tell "\n=== test size ==\n"
        tell $ show testSpan
        tell "\n=== computation ==\n"
        tell $ show testComp

instance (Ord a, Arbitrary a) => Arbitrary (TestPoint a) where
    arbitrary = do
        nerfDesc <- arbitrary
        active <- arbitraryActive $ labelsD nerfDesc
        (p, q) <- arbitrarySpan
        (i, j) <- arbitrarySpan `suchThat` \(i, j) -> p <= i && j <= q
        x <- elements $ labelsD nerfDesc
        return $ TestPoint nerfDesc active (p, q) (i, j, x)

propMaxPhi :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propMaxPhi test =
    trace (show (y, y')) (y ~== y')
  where
    y  = maxPhi  active nerf i j x
    y' = maxPhi' active nerf i j x
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    (i, j, x) = testComp test

propSumPhi :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propSumPhi test =
    trace (show (y, y')) (y ~== y')
  where
    y  = sumPhi  active nerf i j x
    y' = sumPhi' active nerf i j x
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    (i, j, x) = testComp test

propMaxPhiR :: (Show a, Ord a, Memo.HasTrie a) => TestPoint a -> Bool
propMaxPhiR test =
    trace (show (y, y')) (y ~== y')
  where
    y  = maxPhiR  active nerf p q i j x
    y' = maxPhiR' active nerf p q i j x
    (p, q) = testSpan test
    nerf = nerfFromDesc $ testNerf test
    active = activeFromDesc $ testActive test
    (i, j, x) = testComp test
