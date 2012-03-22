{-# LANGUAGE DeriveDataTypeable
           , OverloadedStrings
           , ScopedTypeVariables #-}

import System.IO
import System.Console.CmdArgs
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Control.Applicative ((<$>))
import Control.Monad.Lazy (forM', mapM')
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import qualified Data.Binary as Binary
import qualified Data.Vector as V
import qualified Data.ListLike as LL

import qualified SGD
import qualified Data.CRF as CRF

import qualified Format.Plain as Plain
import qualified Observation.Types as Ob
import qualified Observation.Selection as ObSel

-- schema = [lemma 0, substring 0]
-- schema = [orth 0, lowerLemma 0, lowerOrth (-1), lowerOrth 1, substring 0]
schema =
    [ Ob.lowerOrth 0, Ob.lowerOrth (-1)
    , Ob.upperOnlyOrth 0, Ob.upperOnlyOrth (-1)
    , lowerLemma 0, lowerLemma (-1)
    , shape 0, shape (-1)
    , packedShape 0, packedShape (-1)
    , Ob.join "-" (shape 0) (shape (-1))
    , Ob.join "-" (packedShape 0) (packedShape (-1))
    , suffixes 0 ]

lowerLemma k = Ob.group $ map ($ Ob.lowerOrth k)
    [ Ob.prefix 0   
    , Ob.prefix (-1)
    , Ob.prefix (-2)
    , Ob.prefix (-3)
    , Ob.suffix (-1)
    , Ob.suffix (-2)
    , Ob.suffix (-3) ]

suffixes k = Ob.group $ map ($ Ob.orth k)
    [ Ob.suffix 3
    , Ob.suffix 4
    , Ob.suffix 5 ]

shape k = Ob.shape $ Ob.orth k
packedShape k = Ob.packedShape $ Ob.orth k

-- substring k = Ob.group $ map ($ orth k)
--     [ Ob.substrings 1 
--     , Ob.substrings 2 
--     , Ob.substrings 3 ]

data Args
  = TrainMode
    { trainPath :: FilePath
    , evalPath :: FilePath
    , workersNum :: Int
    , iterNum :: Double
    , batchSize :: Int
    , regVar :: Double
    , scale0 :: Double
    , tau :: Double
    , loadModel :: FilePath
    , outModel :: FilePath }
  | TagMode
    { dataPath :: FilePath
    , loadModel :: FilePath }
  deriving (Data, Typeable, Show)

trainMode = TrainMode
    { trainPath = def &= argPos 0 &= typ "TRAIN-FILE"
    , evalPath = def &= typFile &= help "Evaluation data file"
    , workersNum = 1 &= help "Number of gradient-computing workers"
    , iterNum = 10 &= help "Number of SGD iterations"
    , batchSize = 30 &= help "Batch size"
    , regVar = 10.0 &= help "Regularization variance"
    , scale0 = 1.0 &= help "Initial scale parameter"
    , tau = 5.0 &= help "Initial tau parameter"
    , loadModel = def &= typFile &= help "Input model file"
    , outModel = def &= typFile &= help "Output model file" }

tagMode = TagMode
    { loadModel = def &= argPos 0 &= typ "MODEL"
    , dataPath = def &= typFile
        &= help "Input file; if not specified, read from stdin" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, tagMode]

main = do
    args <- cmdArgsRun argModes
    exec args

exec args@TrainMode{} = do
    hSetBuffering stdout NoBuffering

    let readTrain = readData $ trainPath args
    let readEval  = readData $ evalPath args

    inCrf <- if null $ loadModel args
        then return Nothing 
        else Just <$> Binary.decodeFile (loadModel args)

    codec <- case inCrf of
        Just (_, codec) -> return codec
        Nothing         -> CRF.mkCodec "O" <$> readTrain

    trainData <- V.fromList <$> map (CRF.encodeSent' codec) <$> readTrain
    evalData  <- V.fromList <$> map (CRF.encodeSent' codec) <$> readEval
    
    crf <- return $ case inCrf of
        Just (crf, _) -> crf
        Nothing       -> CRF.mkModel $ CRF.presentFeats trainData

    sgdArgs <- return $ SGD.SgdArgs
        { SGD.batchSize = batchSize args
        , SGD.regVar = regVar args
        , SGD.iterNum = iterNum args
        , SGD.scale0 = scale0 args
        , SGD.tau = tau args }
    crf' <- SGD.sgd sgdArgs trainData evalData crf

    if not $ null $ outModel args then do
        putStrLn $ "\nSaving model in " ++ outModel args ++ "..."
        Binary.encodeFile (outModel args) (crf', codec)
    else
        return ()

exec args@TagMode{} = do
    model <- Binary.decodeFile $ loadModel args

    plain <- if null $ dataPath args 
        then readPlain "stdin" =<< L.getContents
        else readPlain (dataPath args) =<< L.readFile (dataPath args)

    tagged <- return $ map (tagSent model) plain
--     tagged <- return
--         ( map (tagSent model) plainData
--           `using` parBuffer 50 (evalList L.evalLincWord) )

    mapM_ print tagged

tagSent :: (CRF.Model, CRF.Codec T.Text) -> Plain.PlainSent -> Plain.PlainSent
tagSent (crf, codec) plain =
    let encoded = CRF.encodeSent codec $ ObSel.mkSent schema plain
        choices = map (CRF.decodeL codec) $ CRF.tag crf encoded
    in  Plain.applyLabels plain choices

-- | FIXME: Serve null path.
readData :: FilePath -> IO [ObSel.Sent]
readData path = do
    plainTrain <- readPlain path =<< L.readFile path
    return $ map (ObSel.mkSent schema) plainTrain

readPlain :: String -> L.Text -> IO [Plain.PlainSent]
readPlain path = catchErrors . Plain.parseDoc path

catchErrors :: [Either String Plain.PlainSent] -> IO [Plain.PlainSent]
catchErrors dataSet =
    forM' dataSet (either putErr justify) >>= return . catMaybes
  where
    putErr err = hPutStr stderr (err ++ "\n") >> return Nothing
    justify = return . Just
