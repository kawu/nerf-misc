{-# LANGUAGE DeriveDataTypeable
           , ScopedTypeVariables #-}

import System.IO
import System.Console.CmdArgs
import System.IO (hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Control.Monad.Lazy (forM', mapM')
import Data.Maybe (catMaybes)
import qualified Data.Binary as Binary
import qualified Data.Vector as V
import qualified Data.ListLike as LL

import qualified Data.CRF.InOut as InOut
import qualified Data.CRF.Codec as Codec
import qualified SGD
import qualified Data.CRF.Model as Model
import           Data.CRF.Gradient ()
import qualified Data.CRF.Feature as Feature
import qualified Data.CRF.XRYs as XRYs
import qualified Format.Plain as Plain
import qualified Observation.Types as Obv
import qualified Observation.Selection as ObvSelect

schema = [lemma 0, substring 0]
-- schema = [orth 0, lowerLemma 0, lowerOrth (-1), lowerOrth 1, substring 0]

orth = Obv.orth
lowerOrth = Obv.map T.toLower . orth

lemma k = Obv.group $ map ($ orth k)
    [ Obv.prefix 0   
    , Obv.prefix (-1)
    , Obv.prefix (-2)
    , Obv.prefix (-3) ]

lowerLemma k = Obv.group $ map ($ lowerOrth k)
    [ Obv.prefix 0   
    , Obv.prefix (-1)
    , Obv.prefix (-2)
    , Obv.prefix (-3) ]

substring k = Obv.group $ map ($ orth k)
    [ Obv.substrings 1 
    , Obv.substrings 2 
    , Obv.substrings 3 ]

suffix k = Obv.suffix 3 $ lowerOrth k

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
    , outModel :: FilePath }
  | TagMode
    { dataPath :: FilePath
    , inModel :: FilePath
    , morphosFormat :: Bool }
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
    , outModel = def &= typFile &= help "Output model file" }

tagMode = TagMode
    { inModel = def &= argPos 0 &= typ "MODEL"
    , dataPath = def &= typFile
        &= help "Input file; if not specified, read from stdin"
    , morphosFormat = False &= help "Is input in Morphos format" }

argModes :: Mode (CmdArgs Args)
argModes = cmdArgsMode $ modes [trainMode, tagMode]

main = do
    args <- cmdArgsRun argModes
    exec args

exec args@TrainMode{} = do
    hSetBuffering stdout NoBuffering

    codec <- return . Codec.fromWords . concat
        =<< readData (trainPath args)
    trainInts <- return . map (Codec.encodeSent codec)
        =<< readData (trainPath args)

    -- TODO: change "config" name to something better ?
    -- Basic data config, e.g. set of labels.
    config <- XRYs.mkConfig $ Codec.fromWords $ concat trainInts

    trainPart <- return $ LL.fromList $ map (XRYs.mkXRYs config) trainInts
    evalPart <- if null $ evalPath args
        then return $ LL.fromList []
        else do
            evalInts <- return . map (Codec.encodeSent codec)
                =<< readData (evalPath args)
            return $ LL.fromList $ map (XRYs.mkXRYs config) evalInts

    -- To enforce type:
    return (trainPart :: V.Vector XRYs.XRYs)
    
    let initCrf = Model.makeModel $ concat
                $ map Feature.featuresIn' $ LL.toList trainPart
    sgdArgs <- return $ SGD.SgdArgs
        { SGD.batchSize = batchSize args
        , SGD.regVar = regVar args
        , SGD.iterNum = iterNum args
        , SGD.scale0 = scale0 args
        , SGD.tau = tau args }
    crf <- SGD.sgd sgdArgs trainPart evalPart initCrf

    if not $ null $ outModel args then do
        putStrLn $ "\nSaving model in " ++ outModel args ++ "..."
        Binary.encodeFile (outModel args) (crf, codec, config)
    else
        return ()

exec args@TagMode{} = do
    -- putStr $ "Loading model from " ++ inModel ++ "..."
    model <- Binary.decodeFile $ inModel args
    --putStr "\n"

    plain <- if null $ dataPath args 
        then readPlain "stdin" =<< L.getContents
        else readPlain (dataPath args) =<< L.readFile (dataPath args)

    tagged <- return $ map (tagSent model) plain
--     tagged <- return
--         ( map (tagSent model) plainData
--           `using` parBuffer 50 (evalList L.evalLincWord) )

    mapM_ print tagged

-- TODO: Uwzglednic mozliwosc wystpowania ograniczen R !
tagSent :: (Model.Model, Codec.Codec T.Text, XRYs.Config)
        -> Plain.PlainSent -> Plain.PlainSent
tagSent (crf, codec, cfg) plain =
    let withObvs = ObvSelect.mkSentRM schema plain
        encoded = XRYs.mkXRYs cfg $ Codec.encodeSent codec withObvs
        choiceIxs = map (LL.index allLabels) $ Model.tag crf encoded
        choices = [Codec.decodeL codec i | i <- choiceIxs]
        applyChoice ((word, _oldLabel), label) = (word, label)
        allLabels = XRYs.allLabels cfg
        (Plain.PlainSent plainSent) = plain
    in  Plain.PlainSent $ LL.fromList $ map applyChoice
                        $ zip (LL.toList plainSent) choices

readData :: FilePath -> IO [InOut.SentRM T.Text]
readData path = do
    plainTrain <- readPlain path =<< L.readFile path
    return $ map (ObvSelect.mkSentRM schema) plainTrain

readPlain :: String -> L.Text -> IO [Plain.PlainSent]
readPlain path = catchErrors . Plain.parseDoc path

catchErrors :: [Either String Plain.PlainSent] -> IO [Plain.PlainSent]
catchErrors dataSet =
    forM' dataSet (either putErr justify) >>= return . catMaybes
  where
    putErr err = hPutStr stderr (err ++ "\n") >> return Nothing
    justify = return . Just

-- readRawData path = catchErrors . I.readMarked =<< L.readFile path
-- catchErrors :: [Either String (I.SentRM Int)] -> IO [I.SentRM Int]
-- catchErrors dataSet = do
--     dataSet' <- forM' dataSet $ \sentOrErr ->
--       case sentOrErr of
--         Left err -> hPutStr stderr (err ++ "\n") >> return []
--         Right sent -> return sent
--     return $ filter (not . null) dataSet'
