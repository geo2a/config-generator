{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

module GbmParams where

import GHC.Generics
import Data.Aeson
import Control.Monad
import Control.Monad.Reader

-- | Params of h2o.gbm procedure
data GbmParams = 
  GbmParams { y                      :: String
            , xs                     :: [Int]
            , ntrees                 :: Int
            , max_depth              :: Int
            , min_rows               :: Int
            , learn_rate             :: Double
            , nbins                  :: Int
            , nbins_cats             :: Int
            , nfolds                 :: Int
            , balance_classes        :: Bool
            , max_after_balance_size :: Double
            , score_each_iteration   :: Bool
            } deriving (Show, Generic)

instance FromJSON GbmParams
instance ToJSON GbmParams

-- | Ranges of params of h2o.gbm procedure
-- Как избавиться от этого типа и обойтись только GbmParams? 
data GbmParamsRanges =
  GbmParamsRanges { y_range                      :: [String]
                  , xs_range                     :: [[Int]]
                  , ntrees_range                 :: [Int]
                  , max_depth_range              :: [Int]
                  , min_rows_range               :: [Int]
                  , learn_rate_range             :: [Double]
                  , nbins_range                  :: [Int]
                  , nbins_cats_range             :: [Int]
                  , nfolds_range                 :: [Int]
                  , balance_classes_range        :: [Bool]
                  , max_after_balance_size_range :: [Double]
                  , score_each_iteration_range   :: [Bool]
                  } deriving (Show, Generic)

-- Как избавиться от лишнего кода в результирующем выражении генератора списка? 
generateGbmParams :: GbmParamsRanges -> [GbmParams]
generateGbmParams cfg =   
  [GbmParams 
    y 
    xs 
    ntrees 
    max_depth
    min_rows
    learn_rate
    nbins
    nbins_cats
    nfolds
    balance_classes
    max_after_balance_size
    score_each_iteration 
  | y                      <- y_range cfg
  , xs                     <- xs_range cfg
  , ntrees                 <- ntrees_range cfg
  , max_depth              <- max_depth_range cfg
  , min_rows               <- min_rows_range cfg
  , learn_rate             <- learn_rate_range cfg                          
  , nbins                  <- nbins_range cfg
  , nbins_cats             <- nbins_cats_range cfg
  , nfolds                 <- nfolds_range cfg
  , balance_classes        <- balance_classes_range cfg
  , max_after_balance_size <- max_after_balance_size_range cfg
  , score_each_iteration   <- score_each_iteration_range cfg
  ]