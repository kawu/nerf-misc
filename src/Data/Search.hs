module Data.Search
(
) where

import Data.Feature (Ob, Lb, Ps)
import Data.CutStrategy

-- Seaching through forest space (only binary trees) with 
-- respect to given cutting strategy.

data Phi a = Phi
    { onBase :: Lb -> Ps -> a
    , onRule :: a -> Rule -> a -> a }

data Rule = Rule
    { rLeft  :: !Lb
    , rRoot  :: !Lb
    , rRight :: !Lb }

data Node a b = Node
    { beg   :: !Ps
    , label :: !Lb
    , end   :: !Ps
    , aN    :: !a
    , bN    :: !b }

data Nerf = Nerf
    { labelSet :: [Lb]
    , matchChildren :: Lb -> Lb -> [Rule] }

-- data RuleSet = RuleSet
--     -- | Find all rules matching (on left and right child) given labels.
--     { matchChildren :: Lb -> Lb -> [Rule] }

data RangeMap a b = RangeMap a b

leftAdjacent :: Node a b -> RangeMap a b -> [Node a b]
leftAdjacent = undefined

rightAdjacent :: Node a b -> RangeMap a b -> [Node a b]
rightAdjacent = undefined

empty :: RangeMap a b
empty = undefined

push :: Node a b -> RangeMap a b -> RangeMap a b
push = undefined

search :: Strategy a b -> Nerf -> Phi a -> Int -> RangeMap a b
search strategy nerf phi sentLen = closed
  where
    (open, closed) = last $ takeWhile ((>0).length.fst)
                   $ iterate (update strategy nerf phi) (open0, empty)
    open0 = map (\(x, i, a) -> Node i x i a (sLeaf strategy a))
        [ (x, i, onBase phi x i)
        | i <- [0 .. sentLen - 1]
        , x <- labelSet nerf ]


update :: Strategy a b -> Nerf -> Phi a
       -> ([Node a b], RangeMap a b)
       -> ([Node a b], RangeMap a b)
update strategy nerf phi (n:open, closed) =
    (open', closed')
  where
    nsL = leftAdjacent n closed 
    nsR = rightAdjacent n closed 

    open' = [jn n r m | m <- nsR, r <- rs n m]
         ++ [jn m r n | m <- nsL, r <- rs m n]
         ++ open
    rs n m = matchChildren nerf (label n) (label m)
    jn = join strategy phi

    closed' = if sCut strategy (aN n, bN n)
        then push n closed
        else closed

join :: Strategy a b -> Phi a -> Node a b -> Rule -> Node a b -> Node a b
join strategy phi n r m =
    Node (beg n) (rRoot r) (end m) a' b'
  where
    a' = onRule phi (aN n) r (aN m)
    b' = sJoin strategy (aN n, bN n) a' (aN m, bN m)
