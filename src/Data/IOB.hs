module Data.IOB
( IOB
, encodeForest
, decodeForest
) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Data.Tree

-- | TODO: Implement BILOU encoding method?

-- | Simple IOB data structure: word with corresponding label.
data IOB a   = IOB a (Label a) deriving (Show)
type Label a = [Atom a]
data Atom a  = B a      -- ^ Beginning marker
             | I a      -- ^ Inside marker 
             deriving (Show)

word :: IOB a -> a
word (IOB w _) = w

label :: IOB a -> Label a
label (IOB _ e) = e

push :: Atom a -> IOB a -> IOB a
push x (IOB word xs) = IOB word (x:xs)

popMaybe :: IOB a -> Maybe (Atom a, IOB a)
popMaybe (IOB word (x:xs)) = Just (x, IOB word xs)
popMaybe (IOB word [])     = Nothing

topMaybe :: IOB a -> Maybe (Atom a)
topMaybe iob = fst <$> popMaybe iob

pop :: IOB a -> (Atom a, IOB a)
pop = fromJust . popMaybe

top :: IOB a -> Atom a
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
encodeForest :: Forest a -> [IOB a]
encodeForest [] = []
encodeForest (x:xs) = encodeTree x ++ encodeForest xs

encodeTree :: Tree a -> [IOB a]
encodeTree (Node word []) = [IOB word []]
encodeTree (Node label forest) =
    let addLayer e (x:xs) = push (B e) x : map (push $ I e) xs
    in  addLayer label $ encodeForest forest


-- | Decoding.
decodeForest :: Eq a => [IOB a] -> Forest a
decodeForest [] = []
decodeForest xs =
    tree : decodeForest xs'
  where
    (chunk, xs') = followTop xs
    tree = case topMaybe $ head chunk of
        Nothing -> Node (word $ head chunk) [] 
        Just a  -> Node (raw a) (decodeForest $ map rmTop chunk)
    rmTop = snd . pop

-- | Take iob elements as long as the top label doesn't change.  
-- Return obtained part together with the rest of iob.
followTop :: Eq a => [IOB a] -> ([IOB a], [IOB a])
followTop [] = error "followTop: empty iob"
followTop (x:xs) =
    (x:chunk, rest)
  where
    (chunk, rest) = span (pred (topMaybe x) . topMaybe) xs
    pred (Just a) (Just b) = raw a == raw b && isI b
    pred _ _ = False
