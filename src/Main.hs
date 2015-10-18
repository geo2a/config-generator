{-# Language OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BS
import System.Environment

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
               , paramsFileName     = "params.json"
               }

defaultGbmParamsRanges :: GbmParamsRanges
defaultGbmParamsRanges = 
  GBM.GbmParamsRanges 
    { GBM.y_range                      = ["y"]
    , GBM.xs_range                     = [[0..7] ++ [9..61] ++ [63..69]] -- без x8
    , GBM.ntrees_range                 = [300]
    , GBM.max_depth_range              = [3..7]
    , GBM.min_rows_range               = [1,5,10,20,50,100,200]
    , GBM.learn_rate_range             = [0.04,0.05,0.1]
    , GBM.nbins_range                  = [20]
    , GBM.nbins_cats_range             = [2, 16, 1024]
    , GBM.nfolds_range                 = [0]
    , GBM.balance_classes_range        = [False]
    , GBM.max_after_balance_size_range = [0.1]
    , GBM.score_each_iteration_range   = [False]
    }

defaultRandomForestParamsRanges :: RandomForestParamsRanges
defaultRandomForestParamsRanges = undefined

-----------------------
-------Main Code-------
-----------------------
generateJobs :: MethodParams ranges params => ranges -> [Job params]
generateJobs ranges = 
  Job                           <$>
    [defaultInput]              <*>
    generateMethodParams ranges <*> 
    [defaultOutput]
    

saveJobsGbm :: [Job GbmParams] -> IO ()
saveJobsGbm jobs = 
  zipWithM_ BS.writeFile 
            (filenames "output/") 
            (map encodePretty $ jobs)

--saveJobs :: MethodParams ranges params => [Job params] -> IO ()
--saveJobs jobs = 
--  zipWithM_ BS.writeFile 
--            (filenames "output/") 
--            (map encodePretty $ jobs) 

main = do
  --args <- getArgs
  --guard $ not . null $ args  
  --contents <- BS.readFile $ head args 
  --case decode contents of 
  --  Nothing -> 
  --    print "Error: invalid config file"
  --  Just ranges -> 
  --    saveJobsGbm $ generateJobs ranges
  saveJobsGbm $ generateJobs defaultGbmParamsRanges