{-# LANGUAGE OverloadedStrings
           , BangPatterns #-}

import System (getArgs)
import Data.List (groupBy, foldl', intercalate)
import Data.Maybe (fromJust, listToMaybe)
import Control.Applicative ((<$>))
import Control.Monad (forM_, msum)
import Text.HTML.TagSoup
import Text.StringLike
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT

findAttrib :: LT.Text -> [Tag LT.Text] -> [T.Text]
findAttrib x
    = map (LT.toStrict . fromAttrib "val")
    . filter ((==x) . fromAttrib "att")
    . filter (isTagOpenName "feat")

writtenForms = findAttrib "writtenForm"
neType xs = fromJust $ msum $ map listToMaybe
    [ findAttrib "externalReference" xs
    , findAttrib "label" xs ]

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> M.Map k a
fromListWith f xs =
    let update m (!k, !x) = M.insertWith' f k x m
    in  foldl' update M.empty xs

main = do
    [inPath] <- getArgs
    tags <- parseTags <$> LT.readFile inPath
    let groups = tail $ groupBy (\_ t -> isLexTag t) tags
    let dict = fromListWith S.union $ concatMap procGroup groups
    forM_ (M.toList dict) $ \(k, x) -> do
        T.putStr k
        putStr " => "
        T.putStrLn $ T.intercalate " " $ S.toList x
  where
    isLexTag = not . isTagOpenName "LexicalEntry"
    procGroup grp =
        let forms = writtenForms grp
            ne = neType grp
        in  [(form, S.singleton ne) | form <- forms]
