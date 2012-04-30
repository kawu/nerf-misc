module Data.IOB
( IOB
, encodeForest
, decodeForest
) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.AnnTree

-- | TODO: Implement BILOU encoding method?

-- | Simple IOB data structure: word with corresponding label.
data IOB w a = IOB w (Label a) deriving (Show)
type Label a = [Atom a]
data Atom a  = B a      -- ^ Beginning marker
             | I a      -- ^ Inside marker 
             deriving (Show)

word :: IOB w a -> w
word (IOB w _) = w

label :: IOB w a -> Label a
label (IOB _ e) = e

push :: Atom a -> IOB w a -> IOB w a
push x (IOB word xs) = IOB word (x:xs)

popMaybe :: IOB w a -> Maybe (Atom a, IOB w a)
popMaybe (IOB word (x:xs)) = Just (x, IOB word xs)
popMaybe (IOB word [])     = Nothing

topMaybe :: IOB w a -> Maybe (Atom a)
topMaybe iob = fst <$> popMaybe iob

pop :: IOB w a -> (Atom a, IOB w a)
pop = fromJust . popMaybe

top :: IOB w a -> Atom a
top = fromJust . topMaybe

raw :: Atom a -> a
raw (B x) = x
raw (I x) = x

isB :: Atom a -> Bool
isB (B _) = True
isB _     = False

isI :: Atom a -> Bool
isI (I _) = True
isI _     = False


-- | Encoding.
encodeForest :: Forest a w -> [IOB w a]
encodeForest [] = []
encodeForest (x:xs) = encodeTree x ++ encodeForest xs

encodeTree :: Tree a w -> [IOB w a]
encodeTree (Leaf word) = [IOB word []]
encodeTree (Node label forest) =
    let addLayer e (x:xs) = push (B e) x : map (push $ I e) xs
    in  addLayer label $ encodeForest forest


-- | Decoding.
decodeForest :: Eq a => [IOB w a] -> Forest a w
decodeForest [] = []
decodeForest xs =
    tree : decodeForest xs'
  where
    (chunk, xs') = followTop xs
    tree = case topMaybe $ head chunk of
        Nothing -> Leaf (word $ head chunk)
        Just a  -> Node (raw a) (decodeForest $ map rmTop chunk)
    rmTop = snd . pop

-- | Take iob elements as long as the top label doesn't change.  
-- Return obtained part together with the rest of iob.
followTop :: Eq a => [IOB w a] -> ([IOB w a], [IOB w a])
followTop [] = error "followTop: empty iob"
followTop (x:xs) =
    (x:chunk, rest)
  where
    (chunk, rest) = span (pred (topMaybe x) . topMaybe) xs
    pred (Just a) (Just b) = raw a == raw b && isI b
    pred _ _ = False
