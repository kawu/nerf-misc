{-# LANGUAGE MultiParamTypeClasses #-}

module Observation.Selection
( selectOn
, mkSent
, Word (..)
, Sent
) where

import qualified Data.Text as T

import qualified Data.CRF as CRF

import Text.Levels
import Observation.Types hiding (map)

data Word = Word
    { obs :: [T.Text]
    , lab :: T.Text }

instance CRF.HasObs Word T.Text where
    obs = obs

instance CRF.HasChoice Word T.Text where
    choice word = [(lab word, 1.0)]

type Sent = [Word]

-- | Select observations with respect to a given schema and
-- given position in the sentence s.
selectOn :: Schema s -> s -> Int -> [Obser]
selectOn schema sent pos = concat
    [ map (T.pack (show i) `T.append`) $ oRule sent pos
    | (oRule, i) <- zip schema [1..] ]

mkSent :: Labeled s => Schema s -> s -> Sent
mkSent schema sent =
    [ Word (obvs i) (choice i)
    | i <- [0 .. sentLen sent - 1] ]
  where
    obvs = selectOn schema sent
    choice i = label_ sent i
