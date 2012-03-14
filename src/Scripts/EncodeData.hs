import System (getArgs)
import System.IO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text as T
import qualified Data.Text as T
import Control.Monad (forM_, mapM_, (>=>))
import Control.Monad.Lazy (forM', mapM')

import CRF.InOut
import CRF.Codec

catchErrors :: [Either String (SentRM T.Text)] -> IO [SentRM T.Text]
catchErrors dataSet = do
    dataSet' <- forM' dataSet $ \sentOrErr ->
      case sentOrErr of
        Left err -> hPutStr stderr (err ++ "\n") >> return []
        Right sent -> return sent
    return $ filter (not . null) dataSet'

showDataSet :: Show s => [SentRM s] -> IO ()
showDataSet = mapM_ $ \sent -> do
    mapM print sent
    putStrLn ""

readDataSet :: FilePath -> IO [SentRM T.Text]
readDataSet = LT.readFile >=> catchErrors . readMarked

main = do
    [inputFile] <- getArgs
    codec <- return . fromWords . concat =<< readDataSet inputFile
    showDataSet . map (encodeSent codec) =<< readDataSet inputFile
