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
  RandomForestParams                 <$> 
    y_range cfg                      <*>
    xs_range cfg                     <*>
    mtries_range cfg                 <*>
    sample_rate_range cfg            <*>
    ntrees_range cfg                 <*>                
    max_depth_range cfg              <*>             
    min_rows_range cfg               <*>              
    nbins_range cfg                  <*>                 
    nbins_cats_range cfg             <*>            
    nfolds_range cfg                 <*>                
    balance_classes_range cfg        <*>       
    max_after_balance_size_range cfg
    