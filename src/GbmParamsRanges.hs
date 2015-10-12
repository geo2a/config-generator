{-# Language OverloadedStrings #-}

module GbmParamsRanges where

import Types

xs_range :: [[FactorNumber]]
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