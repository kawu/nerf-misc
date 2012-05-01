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
import Data.AnnTree

ignoreFail :: Show a => Tar.Entries a -> [Tar.Entry]
ignoreFail Tar.Done = []
ignoreFail (Tar.Next x xs) = x : ignoreFail xs
ignoreFail (Tar.Fail x) = error $ show x

entries :: FilePath -> IO [Tar.Entry]
entries tar = ignoreFail . Tar.read . GZip.decompress <$> BS.readFile tar

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
morphSentP = uncurry MorphSent <$> (tag "s" *> getAttr "xml:id" </> segP)

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


data NeSent = NeSent
    { corresp :: Ptr
    , names   :: [Ne] }
    deriving (Show)
data Ne = Ne
    { neID      :: ID
    , neType    :: String
    , neSubType :: Maybe String
    , nePtrs    :: [Ptr] }
    deriving (Show)
data Ptr = In String
         | Out String
         deriving (Show)

namedP :: XmlParser String [NeSent]
namedP = true //> namedSentP

namedSentP :: XmlParser String NeSent
namedSentP = uncurry NeSent <$> (tag "s" *> correspP </> neP)

correspP :: TagPred String Ptr
correspP = parsePtr <$> getAttr "corresp"

parsePtr :: String -> Ptr
parsePtr ptr = case break (=='#') ptr of
    (x, []) -> In x
    (x, y)  -> Out $ tail y

neP :: XmlParser String Ne
neP = (tag "seg" *> getAttr "xml:id") `join` \neId -> do
    (typ, subTyp) <- tag "fs" `joinR`
        ((,) <$> featP "type" <*> optional (featP "subtype") <* ignore)
    ptrs <- many ptrP
    return $ Ne neId typ subTyp ptrs

featP :: String -> XmlParser String String
featP x = (tag "f" *> hasAttr "name" x) `joinR` cut (getAttr "value")

ptrP :: XmlParser String Ptr
ptrP = parsePtr <$> (cut $ tag "ptr" *> getAttr "target")

parseNamed :: Tar.Entry -> [NeSent]
parseNamed entry =
    parseXML namedP content
  where
    (Tar.NormalFile binary _) = Tar.entryContent entry
    content = BS.toString binary


main = do
    [tar] <- getArgs
    xs <- entries tar
    forM_ (getDirs xs) $ \(NKJPDir morph named) -> do
        let ms = parseMorph morph
            ns = parseNamed named
        forM_ (zip ms ns) print
        putStrLn ""
