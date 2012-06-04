module Data.Search.Open
( Open (Open)
, empty
, null
, fromList
, pop
, pushWith
, appendWith
) where

import Prelude hiding (null, map)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.PQueue.Min as P

import Data.Search.Node

data Open a = Open
    { queue :: P.MinQueue Node
    , map   :: M.Map Node a }

empty :: Open a
empty = Open P.empty M.empty

null :: Open a -> Bool
null = M.null . map

pop :: Open a -> Maybe ((Node, a), Open a)
pop o = case P.minView (queue o) of
    Nothing     -> Nothing
    Just (n, q) -> Just ((n, x), Open q m)
      where
        x = map o M.! n
        m = n `M.delete` map o 

pushWith :: (a -> a -> a) -> (Node, a) -> Open a -> Open a
pushWith f (n, x) o = uncurry Open $
  case M.lookup n (map o) of
    Just y  ->
        ( queue o
        , M.insert n (f x y) (map o) )
    Nothing ->
        ( P.insert n (queue o)
        , M.insert n x (map o) )

appendWith :: (a -> a -> a) -> Open a -> [(Node, a)] -> Open a
appendWith f = foldl' (flip $ pushWith f)

fromList :: [(Node, a)] -> Open a
fromList = appendWith (error "Open.fromList: overlapping elements") empty
