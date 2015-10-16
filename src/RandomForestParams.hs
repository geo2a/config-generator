{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

module RandomForestParams where

import GHC.Generics
import Data.Aeson
import Data.Tuple.Curry(uncurryN)

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

generateRandomForestParams :: [RandomForestParams]
generateRandomForestParams = map (uncurryN RandomForestParams) $ 
  [( y
   , xs
   , mtries
   , sample_rate
   , ntrees                
   , max_depth             
   , min_rows              
   , nbins                 
   , nbins_cats            
   , nfolds                
   , balance_classes       
   , max_after_balance_size
   ) 
  | y                      <- ["y"]
  , xs                     <- xs_range
  , mtries                 <- mtries_range
  , sample_rate            <- sample_rate_range
  , ntrees                 <- ntrees_range                
  , max_depth              <- max_depth_range             
  , min_rows               <- min_rows_range              
  , nbins                  <- nbins_range                 
  , nbins_cats             <- nbins_cats_range            
  , nfolds                 <- nfolds_range                
  , balance_classes        <- balance_classes_range       
  , max_after_balance_size <- max_after_balance_size_range
  ]

xs_range :: [[Int]]
xs_range =  [[0..61] ++ [63..69]]

mtries_range :: [Int]
mtries_range = [25]

sample_rate_range :: [Double]
sample_rate_range = [0.2]

ntrees_range :: [Int]
ntrees_range = [200]

max_depth_range :: [Int]
max_depth_range = [4]

min_rows_range :: [Int] 
min_rows_range = [100]

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