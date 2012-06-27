module Proof.Utils
( (~==)
,  catchNull
) where

catchNull :: ([a] -> b) -> [a] -> Maybe b
catchNull f xs
    | null xs   = Nothing
    | otherwise = Just $ f xs

(~==) :: RealFrac a => Maybe a -> Maybe a -> Bool
Just x ~== Just y =
    x == y || (1 - eps <= z && z <= 1 + eps)
  where
    z = x / y
    eps = 1.0e-10
Nothing ~== Nothing = True
_ ~== _ = False
