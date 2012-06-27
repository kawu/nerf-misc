{-# LANGUAGE ForeignFunctionInterface #-}

-- | Computations in logarithmic scale.

module Proof.LogMath
( zero
, one
, (.*.)
, (./.)
, (.+.)
, sum
, product
) where

import Prelude hiding (sum, product)
import Data.List (foldl')

foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double

zero :: Double
zero = -(1/0)

one :: Double
one = 0

isZero :: Double -> Bool
isZero x = x == zero

(.*.) :: Double -> Double -> Double
(.*.) = (+)

(./.) :: Double -> Double -> Double
(./.) = (-)

(.+.) :: Double -> Double -> Double
x .+. y
    | isZero x  = y
    | x > y     = x + log1p(exp(y - x))
    | otherwise = y + log1p(exp(x - y))

sum :: [Double] -> Double
sum = foldl' (.+.) zero

product :: [Double] -> Double
product = foldl' (.*.) one
