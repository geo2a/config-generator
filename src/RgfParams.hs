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
  RgfParams { saveLastModelOnly :: Bool
            , algorithm         :: String
            , loss              :: String
            , max_leaf_forest   :: Int
            , test_interval     :: Int
            , reg_L2            :: Double
            , verbose           :: Bool
            } deriving (Show, GHC.Generic)

instance FromJSON RgfParams
instance ToJSON RgfParams

-- | Ranges of params of rgf procedure
data RgfParamsRanges = 
  RgfParamsRanges { saveLastModelOnly_range :: [Bool] 
                  , algorithm_range         :: [String]
                  , loss_range              :: [String]
                  , max_leaf_forest_range   :: [Int]
                  , test_interval_range     :: [Int]
                  , reg_L2_range            :: [Double]
                  , verbose_range           :: [Bool]
                  } deriving (Show, GHC.Generic)

instance FromJSON RgfParamsRanges
instance ToJSON RgfParamsRanges

generateRgfParams :: RgfParamsRanges -> [RgfParams]
generateRgfParams cfg = 
  RgfParams                     <$>
    saveLastModelOnly_range cfg <*>
    algorithm_range cfg         <*>  
    loss_range cfg              <*>  
    max_leaf_forest_range cfg   <*>  
    test_interval_range cfg     <*>
    reg_L2_range cfg            <*>
    verbose_range cfg

--------------------------------------
-------Datatypes for method I/O-------
--------------------------------------

data InOutParamsRgf =
  InOutParamsRgf { train_xy_fn :: (FilePath, FilePath)
                 , test_x_fn   :: FilePath
                 , model_fn_prefix :: FilePath
                 } deriving (Show, Generic)

instance FromJSON InOutParamsRgf
instance ToJSON InOutParamsRgf

--data InputParamsRgf = 
--  InputParamsRgf { train_xy_fn :: (FilePath, FilePath)
--                 , test_x_fn   :: FilePath
--                 } deriving (Show, Generic)

--instance FromJSON InputParamsRgf
--instance ToJSON InputParamsRgf

--data OutputParamsRgf = 
--  OutputParamsRgf { model_fn_prefix :: FilePath
--                  } deriving (Show, Generic)

--instance FromJSON OutputParamsRgf
--instance ToJSON OutputParamsRgf