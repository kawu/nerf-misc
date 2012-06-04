module Data.Search.Open
( Open (Open)
, null
, fromList
, pop
, pushWith
, appendWith
) where

import Prelude hiding (null)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.PQueue.Min as P

import Data.Search.Node

data Open a = Open
    { queue :: P.MinQueue Node
    , map   :: M.Map Node a }

null :: Open a -> Bool
null = undefined

fromList :: [(Node, a)] -> Open a
fromList = undefined

pop :: Open a -> ((Node, a), Open a)
pop = undefined

pushWith :: (a -> a -> a) -> (Node, a) -> Open a -> Open a
pushWith f = undefined

appendWith :: (a -> a -> a) -> Open a -> [(Node, a)] -> Open a
appendWith f = foldl' (flip $ pushWith f)
