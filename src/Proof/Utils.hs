module Proof.Utils
( (~==)
, eqMaybe  
, catchNull
, nub
) where

import qualified Data.Set as S
  
import Proof.LogMath

eqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
eqMaybe eq (Just x) (Just y) = x `eq` y
eqMaybe eq Nothing  Nothing  = True
eqMaybe eq _        _        = False

(~==) :: LogDouble -> LogDouble -> Bool
x ~== y = 
    x == y || (1 <= z + eps && z <= 1 + eps)
  where
    z = x / y
    eps = 1.0e-10

catchNull :: ([a] -> b) -> [a] -> Maybe b
catchNull f xs
    | null xs   = Nothing
    | otherwise = Just $ f xs

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
