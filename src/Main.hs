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
import RgfParams as RGF

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
defaultInputGbm :: GBM.InputParamsGbm
defaultInputGbm = 
  InputParamsGbm { GBM.dataFilename = "data/trEmpt.csv"
                 }

defaultOutputGbm :: GBM.OutputParamsGbm
defaultOutputGbm = 
  OutputParamsGbm { GBM.msePlotFileName    = "plot.png"
                  , GBM.confMatrixFileName = "confMatr.csv"
                  , GBM.paramsFileName     = "params.json"
                  }

defaultGbmParamsRanges :: GBM.GbmParamsRanges
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

defaultInputRgf :: RGF.InputParamsRgf
defaultInputRgf = 
  InputParamsRgf { train_x_fn = "data/train.data.x"
                 , train_y_fn = "data/train.data.y"
                 , test_x_fn  = "data/train.data.x"     
                 , test_y_fn  = "data/train.data.y"    
                 }

defaultOutputRgf :: RGF.OutputParamsRgf
defaultOutputRgf = 
  RGF.OutputParamsRgf { evaluation_fn = "results/sample.train.evaluation"
                      , model_fn_prefix = "results/mtrain"
                      }

defaultRgfParamsRanges :: RGF.RgfParamsRanges
defaultRgfParamsRanges = 
  RGF.RgfParamsRanges { algorithm_range = ["RGF", "RGF_Opt", "RGF_Sib"]
                      , loss_range      = ["LS", "Expo", "Log"]
                      , max_leaf_forest_range = [3000]
                      , test_interval_range = [50]
                      , reg_L2_range = [1, 0.1, 0.01, 0.001]
                      }

-----------------------
-------Main Code-------
-----------------------
generateJobs :: ( InputParams  inp
                , MethodParams ranges params
                , OutputParams out) 
                => inp -> ranges -> out -> [Job inp params out]
generateJobs inp ranges out = 
  Job                           <$>
    [inp]                       <*>
    generateMethodParams ranges <*> 
    [out]
    

saveJobsGbm :: 
  [Job GBM.InputParamsGbm GBM.GbmParams GBM.OutputParamsGbm] -> IO ()
saveJobsGbm jobs = 
  zipWithM_ BS.writeFile 
            (filenames "output/") 
            (map encodePretty $ jobs)

saveJobsRgf :: 
  [Job RGF.InputParamsRgf RGF.RgfParams RGF.OutputParamsRgf] -> IO ()
saveJobsRgf jobs = 
  zipWithM_ BS.writeFile 
            (filenames "output/") 
            (map encodePretty $ jobs)

-- Слишком полиморфна, чтобы работать :(
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
  saveJobsRgf $ 
    generateJobs defaultInputRgf defaultRgfParamsRanges defaultOutputRgf