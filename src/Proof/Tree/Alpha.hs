{-# LANGUAGE RecordWildCards #-}

module Proof.Tree.Alpha
( Alpha (..)
, AlphaM (..)
, alpha
, alphaMax
, alphaSum
) where  

import qualified Data.MemoTrie as Memo
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>), (<*>), (<|>))

import Proof.Nerf
import Proof.Utils
import Proof.Tree.Internal hiding (left, right)

-- | Operation definitions for alpha (and beta) computations. 
-- With Alpha you can represent algorithms like sum-product
-- or maxarg (finding the most probable tree structure).
-- We assume, that argument of alphaConcat is non-empty.
data Alpha a = Alpha
    { alphaRoot   :: a  -- ^ For beta computation
    , alphaBase   :: LogDouble -> a
    , alphaRule   :: a -> a -> LogDouble -> a
    , alphaConcat :: [a] -> a }

-- | Alpha computations lifted to Maybe setting.
data AlphaM a = AlphaM
    { alphaRootM    :: Maybe a
    , alphaBaseM    :: LogDouble -> Maybe a
    , alphaRuleM    :: Maybe a -> Maybe a -> LogDouble -> Maybe a
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

-- | Alpha computation.
alpha :: (Ord a, Memo.HasTrie a) => AlphaM b -> Nerf a
      -> Pos -> Pos -> a -> Maybe b
alpha AlphaM{..} Nerf{..} = alphaM
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

-- | Probability of the most probable tree. We assume, that phi
-- values are represented in logarithmic scale.
alphaMax :: AlphaM LogDouble
alphaMax =
    let rule x y z = x * y * z
    in  mkAlphaM $ Alpha one id rule maximum

-- | Sum of all tree probabilities.
alphaSum :: AlphaM LogDouble
alphaSum =
    let rule x y z = x * y * z
    in  mkAlphaM $ Alpha one id rule sum
