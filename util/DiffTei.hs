-- Compare NE annotations between two TEI corpora. Program works under
-- assumption, that last directory component in a TEI corpus path 
-- uniquely identifies document stored in this directory.

import Data.List (isSuffixOf)
import qualified Data.Map as M
import Control.Monad ((>=>), filterM, forM_)
import Control.Applicative ((<$>))
import System.FilePath (combine)
import System.Directory
import System.Environment (getArgs)

import Text.XML.PolySoup

isDODD :: String -> Bool
isDODD x  = "." `isSuffixOf` x || ".." `isSuffixOf` x

listAll :: FilePath -> IO [FilePath]
listAll path = map (combine path)
    <$> filter (not.isDODD)
    <$> getDirectoryContents path

listDirs :: FilePath -> IO [FilePath]
listDirs = listAll >=> filterM doesDirectoryExist

listFiles :: FilePath -> IO [FilePath]
listFiles = listAll >=> filterM doesFileExist

-- | Recursively collect all files satisfying given predicate.
collect :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
collect p path = do
    xs   <- filter p <$> listFiles path
    xs'  <- mapM (collect p) =<< listDirs path
    return $ xs ++ concat xs'

-----------------------------------------------------------------
-- | Ann_named.xml file parser copied from Nerf.Text.NKJP module.
-----------------------------------------------------------------

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
type ID = String
data Ptr = In String
         | Out String
         deriving (Eq, Ord, Show)

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

parseNamed :: FilePath -> IO [NeSent]
parseNamed path = parseXML namedP <$> readFile path 

-- | Replace all Out pointers with In pointers (that is, with lowest
-- level segment IDs). 
followPtrs :: NeSent -> NeSent
followPtrs (NeSent corresp names) =
    NeSent corresp (map getPtrs names)
  where
    getPtrs ne = ne {nePtrs = leafPtrs ne}

    leafPtrs :: Ne -> [Ptr]
    leafPtrs ne = M.fromList
        [ (neID ne, fromChildren ne)
        | ne <- names ] M.! neID ne
    fromChildren ne = concatMap
        (either leafPtrs sing . resolve)
        (nePtrs ne)
    sing x = [x]

    resolve :: Ptr -> Either Ne Ptr
    resolve (In x) = Left $ neMap M.! x
    resolve (Out x) = Right (Out x)

    neMap = M.fromList [(neID ne, ne) | ne <- names]

main = do
    [root] <- getArgs
    xs <- collect (isSuffixOf "ann_named.xml") root
    forM_ xs $ \path -> do
        par <- map followPtrs <$> parseNamed path
        forM_ par $ \sent -> do
            print sent
            print ">=>=>=>=>=>=>=>=>=>=>=>=>=>=>=>=>=>=>=>=>=>"
