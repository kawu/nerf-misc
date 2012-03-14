import System (getArgs)
import System.IO
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Vector as V
-- import Control.Monad ((<=<))
import Control.Monad.Lazy (forM', mapM')

import qualified Format.Plain as P
import qualified Observation.Types as O
import qualified Observation.Selection as O
import qualified CRF.InOut as C
import Text.Levels (Segm, sentLen)

schema = 
    [ O.orth 0
    , O.substrings 3 (O.orth 0) ]

-- schema = 
--     [ O.orth 0
--     , O.group
--         [ O.prefix 0    (O.orth 0)
--         , O.prefix (-1) (O.orth 0)
--         , O.prefix (-2) (O.orth 0) ] ]

catchErrors :: [Either String P.PlainSent] -> IO [P.PlainSent]
catchErrors dataSet =
    forM' dataSet (either putErr justify) >>= return . catMaybes
  where
    putErr err = hPutStr stderr (err ++ "\n") >> return Nothing
    justify = return . Just

printSent :: C.SentRM T.Text -> IO ()
printSent sent = mapM_ print sent >> putStrLn ""

main = do
    contents <- L.getContents
    dataSet <- catchErrors $ P.parseDoc "stdin" contents

    dataSet' <- return $ map (O.mkSentRM schema) dataSet

    mapM_ printSent dataSet'
    -- mapM_ print dataSet
