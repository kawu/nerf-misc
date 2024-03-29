module Text.Enamex
( lexForest
, parseForest
, parseEnamex
) where

import qualified Text.ParserCombinators.Poly.Lazy as P
import Text.ParserCombinators.Poly.Lazy hiding (Parser)
import Control.Applicative
import Control.Monad
import Data.Char (isSpace)

import qualified Data.AnnTree as T

-- | Lexer definition.

data Tok = Word String
         | TagStart String
         | TagEnd String
         deriving (Show)

isWord (Word _) = True
isWord _        = False

isTagStart (TagStart _) = True
isTagStart _            = False

isTagEnd (TagEnd _) = True
isTagEnd _          = False

type Lexer a = P.Parser Char a

lToks :: Lexer [Tok]
lToks = spaces *> lTok `sepBy` spaces

lTok :: Lexer Tok
lTok = TagEnd <$> lEnd
   <|> TagStart <$> lStart
   <|> Word <$> lWord

lEnd = string "</" *> lWord <* char '>'
lStart = char '<' *> lWord <* char '>'
lWord = many1 $ satisfy $ \t -> not (isSpace t) && t /= '>'

-- pTok :: Parser String
-- pTok = many1 $ pEscape "><\\ "
-- -- pTok = many1 $ satisfy (not . isSpace)
-- 
-- pEscape :: [Char] -> Parser Char
-- pEscape xs = foldr (<|>) p [const x <$> string ['\\', x] | x <- xs]
--   where p = satisfy $ \c -> not $ elem c xs

string :: String -> Lexer String
string "" = return ""
string (x:xs) = do {char x; string xs; return (x:xs)}

char :: Char -> Lexer Char
char x = satisfy (x==)

spaces :: Lexer String
spaces = many (satisfy isSpace)

-- | Parser definition.

type Tree = T.Tree String String
type Forest = T.Forest String String
type Parser a = P.Parser Tok a

pForest :: Parser Forest
pForest = many pTree

pTree :: Parser Tree
pTree = pLeaf <|> pNode

pLeaf :: Parser Tree
pLeaf = T.Leaf . getWord <$> satisfy isWord
  where getWord (Word x) = x

pNode :: Parser Tree
pNode = do
    (TagStart x)  <- satisfy isTagStart
    commit $ do
        f  <- pForest
        (TagEnd x') <- satisfy isTagEnd
        when (x /= x') (fail "Tag start/end mismatch") 
        return $ T.Node x f

lexForest :: String -> [Tok]
lexForest = fst . runParser (lToks <* eof)

parseForest :: String -> Forest
parseForest = fst . runParser (pForest <* eof) . lexForest

parseEnamex :: String -> [Forest]
parseEnamex = map parseForest . lines
