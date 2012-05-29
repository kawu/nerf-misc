module Data.Model
(
) where

import qualified Data.Vector as V
import qualified Data.MemoCombinators as Memo
import Data.List (maximumBy)
import Data.Ord (comparing)

import qualified Data.BinTree as T
import qualified Data.Feature as F

type Tree = T.Tree Lb Lb
type Forest = [Tree]

type ObMx = V.Vector [Ob]

-- | Feature index.
type FeatIx = Int

data Cut a
    = NoCut
        { cValue :: Double
        , cRule :: Rule
        , cData :: a }
    | Cut
        { cValue :: Double
        , cRule :: Rule
        , cData :: a }

data CutStrategy a = CutStrategy
    { cInit :: a
    , cJoin :: Rule -> Cut a -> Cut a -> Maybe (Cut a) }

cInit :: CutStrategy a -> Double -> Maybe (Val a)

ner :: CutStrategy a -> Model -> ObMx -> Forest
ner cuts m obMx =
  where
    aM i j x = Memo.memo3 Memo.integral Memo.integral Memo.integral a
    a i j x
        | i == j = init $ phiBase m x (obMx V.! i)
        | otherwise = maximum <$> sequence
            [ join (phiRule m r)
                   (aM i     p (ruleLeft r))
                   (aM (p+1) j (ruleRight r))
            | r <- rules m x, p <- [i..j-1] ]
    init = cInit cuts
    join = cJoin cuts

-- tag :: Cuts -> Model -> ObMx -> Forest
-- tag cuts m obMx =
-- --     [  
-- --     | i <- [0 .. V.length obMx - 1]
-- --     , j <- cut i [i .. V.length obMx - 1] ]
--   where
--     aM i j x = Memo.memo3 Memo.integral Memo.integral Memo.integral a
--     a i j x
--         | i == j = (undefined, phiAtom m x $ obMx V.! i)
--         | otherwise = argmax
--             [ (r, phiRule m r + aM i     p (ruleLeft r)
--                               + aM (p+1) j (ruleRight r))
--             | r <- rules m x, p <- [i..j-1] ]
-- 
--     argmax = maximumBy (comparing snd)


-- -- | Expected numbers of features per given sentece range.
-- expectedFeaturesR :: Model -> ObMx -> (Ps, Ps) -> [(FeatIx, Double)]
-- expectedFeaturesR m obMx (p, q) =
-- 
-- expectedFeatures :: Model -> ObMx -> [(FeatIx, Double)]
-- expectedFeatures m obMx = concat
--     [ map ((tPT ! (i, j))*) $ expectedFeaturesR m obMx (i, j)
--     | i <- [0 .. V.length obMx - 1]
--     , j <- cut i [i .. V.length obMx - 1] ]
--   where
--     cut i = takeWhile (\j -> tPT ? (i, j)) 
