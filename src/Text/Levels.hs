module Text.Levels
( Segm (..)
, Morph (..)
, Tagged (..)
, Labeled (..)
, word
, label
) where

import Data.Text

-- Type classes for different text annotation levels.

-- | Segmentation level
class Segm s where
    -- | Word on given position
    word_ :: s -> Int -> Text    
    -- | Sentence length
    sentLen :: s -> Int

-- | Sentence with labeles 
class Segm s => Labeled s where
    label_ :: s -> Int -> Text

-- | Morfosyntactic level
class Segm s => Morph s where
    -- | Morphosyntactic interpretations for given word
    interps :: s -> Int -> [Text]

-- | Morph disambiguation level
class Morph s => Tagged s where
    -- | Probabilities of morphosyntactic interpretations
    probs :: s -> Int -> [Text]

-- | Function value on given position or Nothing,
-- when position out of boundaries.
tryOn :: Segm s => (s -> Int -> Text) -> s -> Int -> Maybe Text
tryOn f sent k =
    if k < 0 || k >= sentLen sent
        then Nothing
        else Just $ f sent k

word :: Segm s => s -> Int -> Maybe Text
word = tryOn word_

label :: Labeled s => s -> Int -> Maybe Text
label = tryOn label_
