{-# LANGUAGE TypeSynonymInstances #-}

module Format.Plain
( parseSent
, parseDoc
, PlainSent (..)
, Word
, Label
, applyLabels
) where

import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Data.Char (isSpace)
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Vector as V

import Text.Levels

-- import qualified Data.Text.Lazy.IO as L
-- import Control.Monad (mapM_)

-- Parser from Text 
type Parser = Parsec T.Text ()

type Word = T.Text
type Label = T.Text
newtype PlainSent = PlainSent (V.Vector (Word, Label))

applyLabels :: PlainSent -> [Label] -> PlainSent
applyLabels (PlainSent plain) xs =
    PlainSent $ V.fromList $ map apply $ zip (V.toList plain) xs
  where
    apply ((word, _), label) = (word, label)

instance Show PlainSent where
    show (PlainSent sent) = unwords $ map showWL $ V.toList sent
      where
        showWL (word, label) = T.unpack (escape word) ++ "/" ++ T.unpack label
        escape = withQuots . T.replace quot quot2
        withQuots word = quot `T.append` word `T.append` quot
        quot = T.pack "\""
        quot2 = T.pack "\"\""

instance Segm PlainSent where
    word_ (PlainSent sent) k = fst $ sent V.! k
    sentLen (PlainSent sent) = V.length sent

instance Labeled PlainSent where
    label_ (PlainSent sent) k = snd $ sent V.! k

pSent :: Parser PlainSent
pSent = PlainSent . V.fromList <$> many (pWord <* spaces)

pWord :: Parser (Word, Label)
pWord = pack <$> pQuoted <*> (char '/' *> many1 (satisfy $ not . isSpace))
  where pack x y = (T.pack x, T.pack y)

pQuoted :: Parser String
pQuoted = char '"' *> many pQuotedChar <* char '"'
pQuotedChar = noneOf "\"" <|> try (string "\"\"" *> return '"')

parseSent :: SourceName -> T.Text -> Either String PlainSent
parseSent srcName input = onLeft show $ parse pSent srcName input
  where
    onLeft f (Left x) = Left (f x)
    onLeft _ (Right y) = Right y

parseDoc :: SourceName -> L.Text -> [Either String PlainSent]
parseDoc srcName = map (parseSent srcName . L.toStrict) . L.lines

-- main = do
--     contents <- L.getContents
--     mapM_ print $ parseDoc "stdin" contents
