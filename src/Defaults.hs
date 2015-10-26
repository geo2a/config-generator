module Defaults where

import GbmParams as GBM
import RandomForestParams as RF

defaultInOutGbm :: GBM.InOutParamsGbm
defaultInOutGbm = 
  GBM.InOutParamsGbm { dataFilename       = "data/trEmpt.csv"
                     , msePlotFileName    = "plot.png"
                     , confMatrixFileName = "confMatr.csv"
                     , paramsFileName     = "params.json"
                     }

defaultGbmParamsRanges :: GBM.GbmParamsRanges
defaultGbmParamsRanges = 
  GBM.GbmParamsRanges 
    { GBM.y_range                      = ["y"]
    , GBM.xs_range                     = [[0..61] ++ [63..69]]
    , GBM.ntrees_range                 = [100]
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