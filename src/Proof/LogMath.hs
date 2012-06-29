{-# LANGUAGE ForeignFunctionInterface #-}

-- | Computations in logarithmic scale.

module Proof.LogMath
( LogDouble (..)
, zero
, one
) where

import Prelude hiding (sum, product)
import Data.List (foldl')

newtype LogDouble = LogDouble { unLog :: Double }
	deriving (Show, Read, Eq, Ord)

foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double

zero' :: Double
zero' = -(1/0)

one' :: Double
one' = 0

zero :: LogDouble
zero = LogDouble zero' 

one :: LogDouble
one = LogDouble one'

isZero' :: Double -> Bool
isZero' = (zero'==)

isZero :: LogDouble -> Bool
isZero = isZero' . unLog

instance Num LogDouble where
    LogDouble x * LogDouble y = LogDouble $ x + y
    LogDouble x + LogDouble y
        | isZero' x = LogDouble $ y
        | x > y     = LogDouble $ x + log1p(exp(y - x))
        | otherwise = LogDouble $ y + log1p(exp(x - y))
    LogDouble x - LogDouble y = error "LogDouble: (-) not supported"
    negate _    = error "LogDouble: negate not supported"
    abs         = id
    signum (LogDouble x)
        | x > zero' = 1
        | otherwise = 0
    fromInteger x
        | x == 0    = zero
        | x > 0     = LogDouble . log . fromInteger $ x
        | otherwise = error "LogDouble: fromInteger on negative argument"

instance Fractional LogDouble where
    LogDouble x / LogDouble y = LogDouble $ x - y
    fromRational x
        | x == 0    = zero
        | x > 0     = LogDouble . log . fromRational $ x
        | otherwise = error "LogDouble: fromRational on negative argument"
