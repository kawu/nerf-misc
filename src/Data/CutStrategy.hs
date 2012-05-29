module Data.CutStrategy
( Strategy (..)
, (<+>)
, greedy
, greedyTh
, positive
, moreThan
where

import Control.Applicative ((<$>), (<*>))

{- 
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
   { sLeaf :: a -> b
   , sCut  :: (a, b) -> Bool
   , sJoin :: (a, b) -> a -> (a, b) -> b }

-- | AND strategies.
(<+>) :: Strategy a b -> Strategy a c -> Strategy a (b, c)
(<+>) (Strategy l c j) (Strategy l' c' j') =
    Strategy leaf cut join
  where
    leaf x = (l x, l' x)
    cut (a, (x, y)) = c (a, x) && c' (a, y)
    join (a, (x, y)) b (c, (x', y')) =
        ( j  (a, x) b (c, x')
        , j' (a, y) b (c, y') )

-- TODO: OR strategies.


-- | Greedy strategy.
greedy :: Ord a => Strategy a Bool
greedy = Strategy leaf cut join
  where
    leaf _      = False
    cut         = snd
    join (x, _) z (y, _)
        | x > z = True
        | y > z = True
        | otherwise = False
    
-- | Greedy strategy with threshold.
greedyTh :: Double -> Strategy Double Bool
greedyTh k = Strategy leaf cut join
  where
    leaf _ = False
    cut    = snd
    join (x, _) z (y, _)
        | x > z + k = True
        | y > z + k = True
        | otherwise = False

positive :: Strategy Double ()
positive = moreThan 0

moreThan :: Double -> Strategy Double ()
moreThan k = Strategy leaf cut join
  where
    leaf _     = ()
    cut (x, _) = x > k
    join _ _ _ = ()
