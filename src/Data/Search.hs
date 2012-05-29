module Data.Search
(
) where

import qualified Data.Vector as V

import Data.Feature (Ob, Lb, Ps)
import Data.CutStrategy

-- Seaching through forest space (only binary trees) with 
-- respect to given cutting strategy.

type ObMx = V.Vector Ps [Ob]

data Search a = Search
    { onBase :: Lb -> Ps -> a
    , onRule :: a -> Rule -> a -> a }

data Rule = Rule
    { rLeft  :: !Lb
    , rRoot  :: !Lb
    , rRight :: !Lb }

data RangeMap = ...

size :: RangeMap -> Int
empty :: RangeMap
pop   :: RangeMap -> (RangeMap, ???)

search :: Search a -> Strategy a b -> ObMx -> RangeMap (a, b)
search sch stg obMx =
  where
    open0 = map (\(n, a) -> (n, (a, sLeaf stg a)))
        [ (Node i x i, onBase sch x i)
        | i <- [0 .. V.length obMx - 1] ]

    (open, closed) = last $ takeWhile ((>0).size.fst)
                   $ iterate update (open0, empty)

    update (open, closed) =
        (open'', closed')
      where
        (open', n) = pop open
        nsL = leftAdjacent n closed 
        nsR = rightAdjacent n closed 

        rs n m = rulesMatching (label n) (label m)

        open'' = foldl add open' $
            [join n r m | m <- nsR, r <- rs n m] ++
            [join m r n | m <- nsL, r <- rs m n]

        closed' = if sCut stg n
            then add closed n
            else closed
