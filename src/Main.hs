{-# Language OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Tuple.Curry(uncurryN)
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BS

import Types
import GbmParamsRanges

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

generateGbmParams :: [GbmParams]
generateGbmParams = map (uncurryN GbmParams) $ 
  [(y, xs, ntrees, max_depth, min_rows, learn_rate, nbins, nbins_cats,
    nfolds, balance_classes, max_after_balance_size, score_each_iteration) 
  | y       <- ["y"]
  , xs                     <- xs_range
  , ntrees                 <- ntrees_range
  , max_depth              <- max_depth_range
  , min_rows               <- min_rows_range
  , learn_rate             <- learn_rate_range                          
  , nbins                  <- nbins_range
  , nbins_cats             <- nbins_cats_range
  , nfolds                 <- nfolds_range
  , balance_classes        <- balance_classes_range
  , max_after_balance_size <- max_after_balance_size_range
  , score_each_iteration   <- score_each_iteration_range
  ]

generateJobParams :: [JobParams]
generateJobParams = map (uncurryN JobParams) $ 
  [(input, gbm, output)
  | input  <- [defaultInput]
  , gbm    <- generateGbmParams
  , output <- [defaultOutput] 
  ]
main = 
  zipWithM_ BS.writeFile 
            (filenames "result/") 
            (map encodePretty generateJobParams)
      