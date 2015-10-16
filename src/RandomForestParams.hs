{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

module RandomForestParams where

import GHC.Generics
import Data.Aeson

-- | Params of h2o.randomForest procedure
data RandomForestParams = 
  RandomForestParams { y                      :: String
                     , xs                     :: [Int]
                     , mtries                 :: Int
                     , sample_rate            :: Double
                     , ntrees                 :: Int
                     , max_depth              :: Int
                     , min_rows               :: Int
                     , nbins                  :: Int
                     , nbins_cats             :: Int
                     , nfolds                 :: Int
                     , balance_classes        :: Bool
                     , max_after_balance_size :: Double
                     } deriving (Show, Generic)

instance FromJSON RandomForestParams
instance ToJSON RandomForestParams

-- | Ranges of params of h2o.randomForest procedure
data RandomForestParamsRanges = 
  RandomForestParamsRanges { y_range                      :: [String]
                           , xs_range                     :: [[Int]]
                           , mtries_range                 :: [Int]
                           , sample_rate_range            :: [Double]
                           , ntrees_range                 :: [Int]
                           , max_depth_range              :: [Int]
                           , min_rows_range               :: [Int]
                           , nbins_range                  :: [Int]
                           , nbins_cats_range             :: [Int]
                           , nfolds_range                 :: [Int]
                           , balance_classes_range        :: [Bool]
                           , max_after_balance_size_range :: [Double]
                     } deriving (Show, Generic)

generateRandomForestParams :: RandomForestParamsRanges -> [RandomForestParams]
generateRandomForestParams cfg =  
  [RandomForestParams 
    y
    xs
    mtries
    sample_rate
    ntrees                
    max_depth             
    min_rows              
    nbins                 
    nbins_cats            
    nfolds                
    balance_classes       
    max_after_balance_size
  | y                      <- y_range cfg
  , xs                     <- xs_range cfg
  , mtries                 <- mtries_range cfg
  , sample_rate            <- sample_rate_range cfg
  , ntrees                 <- ntrees_range cfg                
  , max_depth              <- max_depth_range cfg             
  , min_rows               <- min_rows_range cfg              
  , nbins                  <- nbins_range cfg                 
  , nbins_cats             <- nbins_cats_range cfg            
  , nfolds                 <- nfolds_range cfg                
  , balance_classes        <- balance_classes_range cfg       
  , max_after_balance_size <- max_after_balance_size_range cfg
  ]