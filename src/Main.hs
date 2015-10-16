{-# Language OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BS
import Data.Tuple.Curry(uncurryN)

import Types
import GbmParams as GBM
import RandomForestParams as RF

---------------------------------
-------Auxiliary Functions-------
---------------------------------

-- | Generate infinite list of config filenames like [cfg1.json. cfg2.json,...]
-- | and put into specified dir
filenames :: FilePath -> [FilePath]
filenames dir = map namewrap [1..]
  where 
    namewrap n = dir ++ "cfg" ++ show n ++ ".json" 

-----------------------
-------Constants-------
-----------------------

defaultInput :: InputParams
defaultInput = 
  InputParams { dataFilename = "data/trEmpt.csv"
              }

defaultOutput :: OutputParams
defaultOutput = 
  OutputParams { msePlotFileName    = "plot.png"
               , confMatrixFileName = "confMatr.csv"
               , gbmParamsFileName  = "gbmParams.json"
               }
-----------------------
-------Main Code-------
-----------------------

generateGbmJobs :: [JobParams GbmParams]
generateGbmJobs = map (uncurryN JobParams) $ 
  [(input, gbm, output)
  | input  <- [defaultInput]
  , gbm    <- generateGbmParams
  , output <- [defaultOutput] 
  ]

generateRandomForestJobs :: [JobParams RandomForestParams]
generateRandomForestJobs = map (uncurryN JobParams) $ 
  [(input, rf, output)
  | input  <- [defaultInput]
  , rf    <- generateRandomForestParams
  , output <- [defaultOutput] 
  ]

main = 
  zipWithM_ BS.writeFile 
            (filenames "result/")
            (map encodePretty generateRandomForestJobs)
            --(map encodePretty generateGbmJobs)
