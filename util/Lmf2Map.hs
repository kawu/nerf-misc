{-# LANGUAGE OverloadedStrings
           , BangPatterns #-}

import System (getArgs)
import Data.List (groupBy, foldl', intercalate)
import Data.Maybe (fromJust, listToMaybe)
import Control.Applicative ((<$>))
import Control.Monad (forM_, msum)
import Data.Binary (Binary, put, get, encodeFile)

import qualified Data.Map as M
import qualified Data.Set as S

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Text.XML.PolySoup

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get 

-- LMF parser

type Ne   = T.Text
type Type = T.Text

lmfP :: XmlParser String [(Ne, Type)]
lmfP = true ##> lexEntryP

lexEntryP :: XmlParser String [(Ne, Type)]
lexEntryP = tag "LexicalEntry" `joinR` do
    many_ $ cut $ tag "feat"
    words <- many wordP
    sense <- senseP
    return [(x, sense) | x <- words]

wordP :: XmlParser String Ne
wordP = head <$> (tag "Lemma" <|> tag "WordForm" /> featP "writtenForm")

senseP :: XmlParser String Type
senseP = head <$> (tag "Sense" //> featP "externalReference" <|> featP "label")

featP :: String -> XmlParser String T.Text
featP att = T.pack <$> cut (tag "feat" *> hasAttr "att" att *> getAttr "val")

-- Main program

main = do
    [inPath, outPath] <- getArgs
    entries <- parseXML lmfP <$> readFile inPath
    encodeFile outPath $ mkDict entries
--     forM_ (M.toList $ mkDict entries) $ \(k, x) -> do
--         T.putStr k
--         putStr " => "
--         T.putStrLn $ T.intercalate " " x

mkDict :: [(Ne, Type)] -> M.Map Ne [Type]
mkDict xs
    = fmap S.toList
    $ fromListWith S.union
    $ concatMap process xs
  where
    -- | Key k can be a multiword NE.
    process (k, x) =
        [(k, label) | k <- ks]
      where
        ks = T.words k
        label = S.singleton $ if length ks == 1
            then "e-" `T.append` x
            else "p-" `T.append` x

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs
