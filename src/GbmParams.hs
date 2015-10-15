{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

module GbmParams where

import GHC.Generics
import Data.Aeson
import Data.Tuple.Curry(uncurryN)

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

generateGbmParams :: [GbmParams]
generateGbmParams = map (uncurryN GbmParams) $ 
  [(y, xs, ntrees, max_depth, min_rows, learn_rate, nbins, nbins_cats,
    nfolds, balance_classes, max_after_balance_size, score_each_iteration) 
  | y                      <- ["y"]
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

xs_range :: [[Int]]
xs_range =  [[0..61] ++ [63..69]]

ntrees_range :: [Int]
ntrees_range = [200]

max_depth_range :: [Int]
max_depth_range = [4]

min_rows_range :: [Int] 
min_rows_range = [100]

learn_rate_range :: [Double] 
learn_rate_range = [0.05]

nbins_range :: [Int] 
nbins_range = [1024]

nbins_cats_range :: [Int] 
nbins_cats_range = [20]

nfolds_range :: [Int]
nfolds_range = [0]

balance_classes_range :: [Bool]
balance_classes_range = [False]

max_after_balance_size_range :: [Double]
max_after_balance_size_range = [5]

score_each_iteration_range :: [Bool]
score_each_iteration_range = [False]