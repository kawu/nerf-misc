module Data.CutStrategy
( Strategy (..)
, (<+>)
, greedy
, greedyTh
, positive
, moreThan
) where

import Control.Applicative ((<$>), (<*>))

{- TODO: Update description, it's out-of-date. 
 -
 - Strategies and what we expect from them
 -
 - We want to add information about computation "cut", which means that
 - given range (p, q) will not be reused in further computations (it
 - is cut).  Strategy can also add some addtional pieces of information
 - about state, which could be used to determine whether some embracing
 - range should be cut or not.
 -
 - There are some interleaved aspects of individual strategies:
 - a) How do we infere, that computation should be cut,
 - b) Given the inference method, how do we structure our computations.
 - For example, if we have a (cut(i, j) => cut(p, q)) rule for all
 - (i, j) <= (p, q) (by which we mean that (p, q) range includes (i, j)),
 - we can structure computations in a simple manner.
 -
 -
 - Different setting: we use "soft" inference rules to structure computations
 - so that we do not even try to reuse disabled (cut) range nodes.  Original
 - operation (a -> r -> a -> a) takes root value from left tree, rule
 - r and root value from right tree to produce a new value.  Each value
 - is stored in a map with (p, q, x) key type, where (p, q) is a range
 - and x is a label value assigned over this range.
 -
 - We use bottom-up computation.  We have a map A with elements of
 - ((p, q, x) -> a) form.  Map B is initially empty. Iteratively, we
 - - pop one element from A,
 - - join it with adjacent elements from B, producing new
 -   ((p, q, x), a) elements and pushing them to A map; if
 -   ((p, q, x), a') element is present in the A map, we
 -   merge a with a' using appropriate join function. 
 - - move the popped element from A to B.
 -
 - To find adjacent ((i, j, y), b) elements to ((p, q, x), a) element
 - (second step in bottom-up computation) we need to extract from B all
 - elements of ((i, p-1, y), b) [and (q+1, j, y), b)] form.  Then, we
 - need to find all rules of (x, z, y) [and (y, z, x)] form. That is all
 - we need to construct new elemenets.
 -
 - Finally, all elements are in the B map, with correct @a@ values.
 - If B map facilitates also a FIFO interface, we can take nodes from
 - B in "correct" order (by which we mean, that when n node has been
 - popped from B, all n' nodes on which n depends have been popped
 - earlier). It further means, that we can use B queue to structure
 - other computations (product-sum, e.g.) in exactly the same way
 - as we have done with NER, argmax computation. 
 -
 - BUT: B queue may be not enough -- when we check ((p, q, x), a)
 - node from B queue, we have to check all divisions and all rules
 - with respect to (p, q, x). Even though some of them might be disabled
 - due to computation cuts.
 -
 - There might be also a different way -- we can try to do other computations
 - together with argmax, NER computation, thus using the same computation
 - structure (if we use the same cutting method) as we have got in NER. 
 -
 -
 - Finally, we define a strategy:
 -
 -   data Strategy a b = Strategy
 -      { sLeaf :: a -> b
 -      , sCut  :: (a, b) -> Bool
 -      , sJoin :: (a, b) -> a -> (a, b) -> Maybe b }
 -
 - @Strategy a b@ structures computations with @a@ result, while @b@ is
 - used as an additional data structure used to determine cut points.
 - Function sLeaf is used on atomic, leaf computation nodes, to acquire
 - initial @b@ value.  sJoin is used to merge computation and strategy
 - values @a@ and @b@ to construct a new strategy value @b@.  It may
 - return Nothing, which means immediate cut.  Finally, sCut function
 - tells us if we should cut a given node.  It is called when a node
 - is moved from A to B map.
 -
 -}

data Strategy a b = Strategy
    -- | @b@ value for leaf computation.
    { sLeaf :: a -> b
    -- | Cut computation node or not?
    , sCut  :: (a, b) -> Bool
    -- | Join two computations.
    , sJoin :: (a, b) -> (a, b) -> b
    -- | Determine @b@ value acquired by different joins. 
    , sDet  :: b -> b -> b }

-- | AND strategies.
(<+>) :: Strategy a b -> Strategy a c -> Strategy a (b, c)
(<+>) (Strategy l c j d) (Strategy l' c' j' d') =
    Strategy leaf cut join det
  where
    leaf x = (l x, l' x)
    cut (a, (x, y)) = c (a, x) && c' (a, y)
    join (a, (x, y)) (c, (x', y')) =
        ( j  (a, x) (c, x')
        , j' (a, y) (c, y') )
    det (x, y) (x', y') = (d x x', d' y y')

-- TODO: OR strategies.


-- | Greedy strategy.
greedy :: (Ord b, Num b) => (a -> b) -> Strategy a b
greedy f = greedyTh f 0
    
-- | Greedy strategy with threshold.
greedyTh :: (Ord b, Num b) => (a -> b) -> b -> Strategy a b
greedyTh f k = Strategy leaf cut join det
  where
    leaf        = f
    cut (x, y)  = f x < y + k
    join (x, _) (y, _) = max (f x) (f y)
    det = max

positive :: (Ord b, Num b) => (a -> b) -> Strategy a ()
positive f = moreThan f 0

moreThan :: (Ord b, Num b) => (a -> b) -> b -> Strategy a ()
moreThan f k = Strategy leaf cut join det
  where
    leaf _     = ()
    cut (x, _) = f x > k
    join _ _   = ()
    det  _ _   = ()
