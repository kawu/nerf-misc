{-# LANGUAGE OverloadedStrings #-}

-- module Text.NKJP
-- (
-- ) where

import System.Environment (getArgs)
import System.FilePath hiding ((</>))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.List (groupBy, find)
import Data.Function (on)
import Data.Maybe (catMaybes)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar

import Text.XML.PolySoup
import qualified Data.AnnTree as AnnTree

type Tree   = AnnTree.Tree String String -- ByteString?
type Forest = AnnTree.Tree String String

ignoreFail :: Show a => Tar.Entries a -> [Tar.Entry]
ignoreFail Tar.Done = []
ignoreFail (Tar.Next x xs) = x : ignoreFail xs
ignoreFail (Tar.Fail x) = error $ show x

entries :: FilePath -> IO [Tar.Entry]
entries tar = ignoreFail . Tar.read . GZip.decompress <$> BS.readFile tar

-- entryPath :: Tar.Entry -> Path.FilePath
-- entryPath = Path.decodeString . Tar.entryPath

-- | NKJP directory.
data NKJPDir = NKJPDir
    { morph :: Tar.Entry    -- ^ Morphosyntax file
    , named :: Tar.Entry }  -- ^ Named entities file

instance Show NKJPDir where
    show (NKJPDir morph named)
        = Tar.entryPath morph
       ++ ", "
       ++ Tar.entryPath named

getDirs :: [Tar.Entry] -> [NKJPDir]
getDirs
    = catMaybes 
    . map (mkDir <$> getEntry "ann_morphosyntax" <*> getEntry "ann_named")
    . groupBy ((==) `on` takeDirectory . Tar.entryPath)
  where
    getEntry base = find ((==base) . takeBaseName . Tar.entryPath)
    mkDir (Just x) (Just y) = Just $ NKJPDir x y
    mkDir _ _ = Nothing

type ID = String
data MorphSent = MorphSent
    { morphID  :: ID
    , segments :: [MorphSeg] }
    deriving (Show)
data MorphSeg  = MorphSeg
    { segID :: ID 
    , orth  :: String }
    deriving (Show)

morphP :: XmlParser String [MorphSent]
morphP = true //> morphSentP

morphSentP :: XmlParser String MorphSent
morphSentP = uncurry MorphSent <$>
    (tag "s" *> getAttr "xml:id" </> segP)

segP :: XmlParser String MorphSeg
segP =
    uncurry MorphSeg <$> (idP </> head <$> orthP)
  where
    idP = tag "seg" *> getAttr "xml:id"
    orthP = true #> hasAttr "name" "orth" #> tag "string" #> text

parseMorph :: Tar.Entry -> [MorphSent]
parseMorph entry =
    parseXML morphP content
  where
    (Tar.NormalFile binary _) = Tar.entryContent entry
    content = BS.toString binary

-- type ID = String
-- data NESent = NESent
--     { namedID :: ID
--     , names   :: [NE] }
-- data NE  = NE
--     { neID  :: ID 
--     , ne :: String }

main = do
    [tar] <- getArgs
    xs <- entries tar
    -- mapM_ (print . Path.directory . entryPath) $ xs
    forM_ (getDirs xs) $ \(NKJPDir morph named) -> do
        forM_ (parseMorph morph) print
        putStrLn ""
    -- mapM_ print $ getDirs xs
