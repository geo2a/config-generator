{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

module GbmParams where

import GHC.Generics as GHC
import Data.Aeson

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
            } deriving (Show, GHC.Generic)

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
                  } deriving (Show, GHC.Generic)

instance FromJSON GbmParamsRanges
instance ToJSON GbmParamsRanges

generateGbmParams :: GbmParamsRanges -> [GbmParams]
generateGbmParams cfg = 
  GbmParams                          <$>
    y_range cfg                      <*>  
    xs_range cfg                     <*>  
    ntrees_range cfg                 <*>  
    max_depth_range cfg              <*>
    min_rows_range cfg               <*> 
    learn_rate_range cfg             <*> 
    nbins_range cfg                  <*> 
    nbins_cats_range cfg             <*> 
    nfolds_range cfg                 <*> 
    balance_classes_range cfg        <*> 
    max_after_balance_size_range cfg <*> 
    score_each_iteration_range cfg 