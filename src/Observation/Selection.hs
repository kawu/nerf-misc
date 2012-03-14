module Observation.Selection
( selectOn
, mkSentRM
) where

import qualified Data.Text as T

import qualified Data.CRF.InOut as CRF
import Text.Levels
import Observation.Types hiding (map)

-- | TODO: WAZNE !! Nadac poszczegolnym wartosciom odpowiednie prefiksy !
-- Przy okazji, cos takiego jak grupa obserwacji bylaby mile widziana ! 
-- ANSWER: Czy tego juz nie ma ?

-- | Select observations with respect to a given schema and
-- given position in the sentence s.
selectOn :: Schema s -> s -> Int -> [Obser]
selectOn schema sent pos = concat
    [ map (T.pack (show i) `T.append`) $ oRule sent pos
    | (oRule, i) <- zip schema [1..] ]

-- | Make sentence in CRF outer format SentRM with selected observations.
-- By outer format we mean format, which CRF uses to comunicate with
-- outside world.
mkSentRM :: Labeled s => Schema s -> s -> CRF.SentRM T.Text
mkSentRM schema sent =
    [ CRF.WordRM (obvs i) [] [choice i]
    | i <- [0 .. sentLen sent - 1] ]
  where
    obvs = selectOn schema sent
    choice i = (label_ sent i, 1.0)

