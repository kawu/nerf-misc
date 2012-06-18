module Data.Search
(
) where

import Data.Base (Ob, Lb, Ps)
import Data.Search.Node
import Data.Rule
import Data.CutStrategy
import qualified Data.Search.Open as O
import qualified Data.Search.Closed as C

-- Searching through forest space (only binary trees) with 
-- respect to given cutting strategy.

-- | FIXME: Move Nerf definition to separate module.
data Nerf = Nerf
    { labelSet :: [Lb]
    , matchChildren :: Lb -> Lb -> [Rule] }

data Phi a = Phi
    -- | Base phi value for given label and position.
    { onBase :: Lb -> Ps -> a
    -- | Phi value given for given rule and children phi values.
    , onRule :: a -> Rule -> a -> a
    -- | Determine phi value from values acquired by different onRule calls.
    , phJoin :: a -> a -> a }

-- | We search through the space of "computation nodes".  We preserve
-- two main algorithm data structures:
-- a) Set of "open" (p, x, q) nodes together with related values
--    [p -- beginning, x -- label, q -- ending].
-- b) Set of "closed" nodes.
--
-- * Values related to closed nodes are final (invariant).
-- * Values related to open nodes may change (kind of mutable map).
-- * Set of open nodes behaves also like a queue.  Order of elements
--   in the queue has to preserve invariant.

search :: Strategy a b -> Nerf -> Phi a -> Int -> C.Closed (a, b)
search strategy nerf phi sentLen = closed
  where
    (open, closed) = iterMaybe (update strategy nerf phi) (open0, C.empty)
    open0 = O.fromList $ map
        (\(x, i, a) -> (Node i x i, (a, sLeaf strategy a)))
        [ (x, i, onBase phi x i)
        | i <- [0 .. sentLen - 1]
        , x <- labelSet nerf ]
    iterMaybe f x = case f x of
        Just x' -> iterMaybe f x'
        Nothing -> x


type Point a b = (O.Open (a, b), C.Closed (a, b))

update :: Strategy a b -> Nerf -> Phi a -> Point a b -> Maybe (Point a b)
update strategy nerf phi what = case O.pop (fst what) of
    Nothing        -> Nothing
    Just (n, open) -> Just (open', closed')
      where
        closed' = if sCut strategy (snd n)
            then C.push n closed
            else closed
        -- FIXME: is this correct? Shouldn't be:
        -- closed' = if sCut strategy (snd n)
        --     then closed
        --     else C.push n closed
        closed = snd what

        open' = O.appendWith dt open $
            [jn n r m | m <- nsR, r <- rs n m] ++
            [jn m r n | m <- nsL, r <- rs m n]
    
        nsL = C.leftAdjacent  (fst n) closed 
        nsR = C.rightAdjacent (fst n) closed 
    
        rs n m = matchChildren nerf (label $ fst n) (label $ fst m)
        jn = join strategy phi
        dt (a, b) (a', b') = (phJoin phi a a', sDet strategy b b')


join :: Strategy a b -> Phi a
     -> (Node, (a, b)) -> Rule -> (Node, (a, b))
     -> (Node, (a, b))
join strategy phi n r m =
    (Node (getBeg n) (root r) (getEnd m), (a', b'))
  where
    a' = onRule phi (getA n) r (getA m)
    b' = sJoin strategy (snd n) (snd m)
    getBeg = beg.fst
    getEnd = end.fst
    getA = fst.snd
    getB = snd.snd
