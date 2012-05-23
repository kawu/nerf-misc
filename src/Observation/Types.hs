{-# LANGUAGE ExistentialQuantification
           , MultiParamTypeClasses
           , OverloadedStrings #-}

module Observation.Types
( Schema
, ObserRule
, Obser
, orth
, beg
, lowerOrth
, upperOnlyOrth
, prefix
, suffix
, group
, join
, substrings
, interps
, map
, shape
, packedShape
, searchDict
, searchAdict
) where

import		 Prelude hiding (map)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.List as List
import           Data.Maybe (maybeToList)
import           Control.Applicative ((<$>))
import           Numeric (showFFloat)
import           Data.ListLike.Text

import Data.Adict

import qualified Text.Levels as L

-- | Observation value.
type Obser = T.Text

-- | Observation extraction rule.
type ObserRule s = s -> Int -> [Obser]

-- | Selection schema.
type Schema s = [ObserRule s]


-- | Is it a first position in a sentence?
beg :: L.Segm s => Int -> ObserRule s
beg shift sent k
    | x < 0 || x >= L.sentLen sent = []
    | x == 0 = ["B"]
    | otherwise = ["I"]
  where
    x = shift + k

-- | Orthographic value.
orth :: L.Segm s => Int -> ObserRule s
orth shift sent k = maybeToList $ L.word sent $ shift + k

map :: L.Segm s => (Obser -> Obser) -> ObserRule s -> ObserRule s
map f rule sent k = do
    x <- rule sent k
    [f x]

lowerOrth :: L.Segm s => Int -> ObserRule s
lowerOrth = map T.toLower . orth

upperOnlyOrth :: L.Segm s => Int -> ObserRule s
upperOnlyOrth shift sent k = do
    form <- orth shift sent k
    case T.find C.isUpper form of
        Just _  -> return form
        Nothing -> []

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

shape :: L.Segm s => ObserRule s -> ObserRule s
shape =
    map (T.map translate)
  where
    translate char
        | C.isLower char = 'l'
        | C.isUpper char = 'u'
        | C.isDigit char = 'd'
        | otherwise      = 'x'

packedShape :: L.Segm s => ObserRule s -> ObserRule s
packedShape rule =
    map (T.pack . List.map T.head . T.group) $ shape rule

join :: L.Segm s => T.Text -> ObserRule s -> ObserRule s -> ObserRule s
join with r1 r2 sent k = do
    x <- r1 sent k
    y <- r2 sent k
    return $ x `T.append` with `T.append` y

-- Dictionary observation types.

type Orth   = T.Text
type NeType = T.Text
type NeDict = M.Map Orth [NeType]

searchDict :: L.Segm s => NeDict -> ObserRule s -> ObserRule s
searchDict dict rule sent k = do
    x <- rule sent k
    case M.lookup x dict of
        Just entry -> entry
        Nothing    -> []

searchAdict :: L.Segm s => Double -> Int -> Adict Char [T.Text]
            -> ObserRule s -> ObserRule s
searchAdict th digits adict rule sent k = fmap glue $ nub $ do
    x <- rule sent k
    (entry, w) <- levenSearch cost th x adict
    y <- info entry
    return (y, w)
  where
    nub = M.toList . M.fromListWith min
    glue (y, w) = y `T.append` T.pack (roundFloat w)
    roundFloat x = take (digits+2) $ showFFloat (Just digits) x ""

-- | Cost function for approximate dictionary searching.
cost :: Cost Char
cost =
    Cost insert delete subst
  where
    insert (k, _) = posMod (fromIntegral k)
    delete (k, _) = posMod (fromIntegral k)
    subst (k, x) (m, y)
        | x  == y               = 0
        | x' == y'              = 0.01
        | otherwise             = posMod (avg k m)
      where
        x' = C.toLower x
        y' = C.toLower y
    posMod k = (k+1) ** (-1)
    avg x y = (fromIntegral x + fromIntegral y) / 2.0
