import           System (getArgs)
import           System.IO (hPutStr, stderr)
import           System.IO.Unsafe (unsafeInterleaveIO)
import           Control.Exception (assert)
import           Control.Applicative ((<*), (*>), (<$>), (<*>))
import           Data.Char (isSpace)
import           Data.Maybe (catMaybes)
import           Text.Parsec

o :: String
o = "O"

type Parser = Parsec String ()

type Label = String
type Sent  = [Label]

mapM' f (x:xs) = unsafeInterleaveIO $ do
    y <- f x
    ys <- mapM' f xs
    return (y : ys)
mapM' _ [] = return []

forM' = flip mapM'

pSent :: Parser Sent
pSent = pWord `sepBy` spaces

pWord :: Parser Label
pWord = pQuoted *> char '/' *> many1 (satisfy $ not . isSpace)

pQuoted :: Parser String
pQuoted = char '"' *> many pQuotedChar <* char '"'
pQuotedChar = noneOf "\"" <|> try (string "\"\"" *> return '"')

parseSent :: SourceName -> String -> Either String Sent
parseSent srcName input =
    onLeft show $ parse pSent srcName input
  where
    onLeft f (Left x) = Left (f x)
    onLeft _ (Right y) = Right y

parseDoc :: SourceName -> String -> [Either String Sent]
parseDoc srcName = map (parseSent srcName) . lines

catchErrors :: [Either String Sent] -> IO [Sent]
catchErrors dataSet =
    catMaybes <$> forM' dataSet (either putErr justify)
  where
    putErr err = hPutStr stderr (err ++ "\n") >> return Nothing
    justify = return . Just

data Stats = Stats
    { tp :: Int
    , tn :: Int
    , fp :: Int
    , fn :: Int }
    deriving (Show)

precision (Stats tp tn fp fn)
    = fromIntegral tp
    / fromIntegral (tp + fp)

recall (Stats tp tn fp fn)
    = fromIntegral tp
    / fromIntegral (tp + fn)

accuracy (Stats tp tn fp fn)
    = fromIntegral (tp + tn)
    / fromIntegral (tp + tn + fp + fn)

add :: Stats -> Stats -> Stats
add s1 s2 = Stats
    { tp = tp s1 + tp s2
    , tn = tn s1 + tn s2
    , fp = fp s1 + fp s2
    , fn = fn s1 + fn s2 }

compareSent :: Sent -> Sent -> Stats
compareSent gold other
    = assert (length gold == length other)
    $ Stats (num tp) (num tn) (num fp) (num fn)
  where
    num pr = length $ filter (uncurry pr) $ zip gold other
    tp x y = y /= o && y == x
    tn x y = y == o && y == x
    fp x y = y /= o && y /= x
    fn x y = y == o && y /= x

stats :: [Sent] -> [Sent] -> Stats
stats ds1 ds2 = foldl1 add [compareSent s1 s2 | (s1, s2) <- zip ds1 ds2]

printStats :: Stats -> IO ()
printStats s = do
    putStr "true positives = "
    print $ tp s
    putStr "false positives = "
    print $ fp s
    putStr "true negatives = "
    print $ tn s
    putStr "false negatives = "
    print $ fp s
    putStr "accuracy = "
    print $ accuracy s
    putStr "precision = "
    print $ precision s
    putStr "recall = "
    print $ recall s

main = do
    [path1, path2] <- getArgs
    gold  <- catchErrors . parseDoc path1 =<< readFile path1
    other <- catchErrors . parseDoc path2 =<< readFile path2
    printStats $ stats gold other
