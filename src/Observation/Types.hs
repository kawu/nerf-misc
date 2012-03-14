{-# LANGUAGE ExistentialQuantification
           , MultiParamTypeClasses #-}

module Observation.Types
( Schema
, ObserRule
, Obser
, orth
, prefix
, suffix
, group
, substrings
, interps
, map
) where

import		 Prelude hiding (map)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Maybe (maybeToList)
import           Control.Applicative ((<$>))

import qualified Text.Levels as L

-- Observation value
type Obser = T.Text

-- -- Class of observations types parametrized by
-- -- observation type t and sentence type s.
-- class ObserT t s where
--     -- | Observation value for given observation type,
--     -- sentence and sentence position.
--     oValue :: t -> s -> Int -> [Obser]

-- Observation type wrapper (using existentials)
-- data ObserBox s = forall t. ObserT t s => ObserBox t
type ObserRule s = s -> Int -> [Obser]

-- Selection schema
-- type Schema s = [ObserBox s]
type Schema s = [ObserRule s]

-- | TODO: Zastanowic sie nad taka implementacja, przy ktorej
-- wartosc orth bedzie obliczana dla danej pozycji co najwyzej raz !

-- | Orthographic value.
orth :: L.Segm s => Int -> ObserRule s
orth shift sent k = maybeToList $ L.word sent $ shift + k

map :: L.Segm s => (Obser -> Obser) -> ObserRule s -> ObserRule s
map f rule sent k = do
    x <- rule sent k
    [f x]

-- | Prefix(es) of given observation rule values.
prefix :: L.Segm s => Int -> ObserRule s -> ObserRule s
prefix prefOf rule sent k = do
    x <- rule sent k 
    xLen <- return $ fromIntegral $ T.length x
    prefOf' <- return $ if prefOf <= 0 then xLen + prefOf else prefOf
    if 0 < prefOf' && prefOf' <= xLen
        then [T.take prefOf' x]
        else []

-- | Suffix(es) of given observation rule values.
suffix :: L.Segm s => Int -> ObserRule s -> ObserRule s
suffix suffOf rule sent k = do
    x <- rule sent k 
    xLen <- return $ fromIntegral $ T.length x
    suffOf' <- return $ if suffOf <= 0 then xLen + suffOf else suffOf
    if 0 < suffOf' && suffOf' <= xLen
        then [T.drop (xLen - suffOf') x]
        else []

interps :: L.Morph s => Int -> ObserRule s
interps shift sent k = L.interps sent $ shift + k

-- Group of rules.  Values of rules from one group are
-- not distinguished when equall.
group :: L.Segm s => [ObserRule s] -> ObserRule s
group rules sent k = do
    rule <- rules
    rule sent k

-- | All substrings of size sn and given observation rule values.
-- TODO: Should `removeDups' really work ?
substrings :: L.Segm s => Int -> ObserRule s -> ObserRule s
substrings sn rule sent k = removeDups $ do
    x <- rule sent k
    i <- [0 .. fromIntegral (T.length x) - sn]
    return $ takeSub x i sn
  where
    takeSub x start n = T.take n $ T.drop start x
    -- removeDups = id
    removeDups = S.toList . S.fromList
