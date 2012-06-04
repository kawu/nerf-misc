module Data.Rule
( Rule (..)
) where

import Data.Base

-- | Rules consists of three labels: root label, left child label
-- and right child label. 
data Rule = Rule
    { left  :: !Lb
    , root  :: !Lb
    , right :: !Lb }
