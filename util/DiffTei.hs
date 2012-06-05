-- Compare NE annotations between two TEI corpora. Program works under
-- assumption, that last directory component in a TEI corpus path 
-- uniquely identifies document stored in this directory.
-- FIXME: rewrite upper description.

import Data.List (isSuffixOf, sort)
import Control.Monad ((>=>), filterM, forM_)
import Control.Applicative ((<$>))
import System.FilePath (combine)
import System.Directory
import System.Environment (getArgs)
import qualified Data.Map as M
import qualified Data.Set as S

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
-- Ann_named.xml file parser copied from Nerf.Text.NKJP module.
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
    deriving (Eq, Ord, Show)
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

-----------------------------------------------------------------
-- Statistics computation
-----------------------------------------------------------------

data Stats = Stats
    { tp :: Int
    , fp :: Int
    , fn :: Int }
    deriving (Show)

precision (Stats tp fp fn)
    = fromIntegral tp
    / fromIntegral (tp + fp)

recall (Stats tp fp fn)
    = fromIntegral tp
    / fromIntegral (tp + fn)

fmeasure s =
    (2.0 * p * r) / (p + r)
  where
    p = precision s
    r = recall s

add :: Stats -> Stats -> Stats
add s1 s2 = Stats
    { tp = tp s1 + tp s2
    , fp = fp s1 + fp s2
    , fn = fn s1 + fn s2 }

-- | Compute difference between NeSentences.
compareSent :: NeSent -> NeSent -> Stats
compareSent goldPre otherPre
    = assert (corresp goldPre == corresp otherPre) "corresp mismatch"
    $ Stats tp fp fn
  where
    gold = S.fromList $ names $ followPtrs goldPre
    other = S.fromList $ names $ followPtrs otherPre
    tp = S.size $ gold `S.intersection` other 
    fp = S.size $ other `S.difference` gold 
    fn = S.size $ gold `S.difference` other 

assert x msg cont
    | x == True = cont
    | otherwise = error msg

stats :: [NeSent] -> [NeSent] -> Stats
stats ds1 ds2 = foldl1 add [compareSent s1 s2 | (s1, s2) <- zip ds1 ds2]

printStats :: Stats -> IO ()
printStats s = do
    putStr "true positives = "
    print $ tp s
    putStr "false positives = "
    print $ fp s
    putStr "false negatives = "
    print $ fn s
    putStr "precision = "
    print $ precision s
    putStr "recall = "
    print $ recall s
    putStr "f-measure = "
    print $ fmeasure s

main = do
    [goldRoot, otherRoot] <- getArgs
    goldPaths <- sort <$> collect (isSuffixOf "ann_named.xml") goldRoot
    otherPahts <- sort <$> collect (isSuffixOf "ann_named.xml") otherRoot

    forM_ (zip goldPaths otherPahts) $ \(goldPath, otherPath) -> do
        goldData  <- parseNamed goldPath
        otherData <- parseNamed otherPath
        printStats $ stats goldData otherData
