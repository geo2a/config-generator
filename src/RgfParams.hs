{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

-- | This module contains data types for params of Regularized Greedy Forests 
-- | (RFG) model
module RgfParams where

import GHC.Generics as GHC
import Data.Aeson

-----------------------------------------
-------Datatypes for method params-------
-----------------------------------------

-- | Params of rgf procedure
data RgfParams = 
  RgfParams { algorithm       :: String
            , loss            :: String
            , max_leaf_forest :: Int
            , test_interval   :: Int
            , reg_L2          :: Double
            } deriving (Show, GHC.Generic)

instance FromJSON RgfParams
instance ToJSON RgfParams

-- | Ranges of params of rgf procedure
data RgfParamsRanges = 
  RgfParamsRanges { algorithm_range       :: [String]
                  , loss_range            :: [String]
                  , max_leaf_forest_range :: [Int]
                  , test_interval_range   :: [Int]
                  , reg_L2_range          :: [Double]
                  } deriving (Show, GHC.Generic)

instance FromJSON RgfParamsRanges
instance ToJSON RgfParamsRanges

generateRgfParams :: RgfParamsRanges -> [RgfParams]
generateRgfParams cfg = 
  RgfParams                   <$>
    algorithm_range cfg       <*>  
    loss_range cfg            <*>  
    max_leaf_forest_range cfg <*>  
    test_interval_range cfg   <*>
    reg_L2_range cfg

--------------------------------------
-------Datatypes for method I/O-------
--------------------------------------

data InputParamsRgf = 
  InputParamsRgf { train_x_fn :: FilePath
                 , train_y_fn :: FilePath
                 , test_x_fn  :: FilePath
                 , test_y_fn  :: FilePath
                 } deriving (Show, Generic)

instance FromJSON InputParamsRgf
instance ToJSON InputParamsRgf

data OutputParamsRgf = 
  OutputParamsRgf { evaluation_fn   :: FilePath
                  , model_fn_prefix :: FilePath
                  } deriving (Show, Generic)

instance FromJSON OutputParamsRgf
instance ToJSON OutputParamsRgf