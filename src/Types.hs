{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}

module Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

import GbmParams          as GBM
import RandomForestParams as RF
import RgfParams          as RGF

-------------------------------
-------Domain Data Types-------
-------------------------------

-- | This typeclass abstracts params generation for various methods, such as 
-- | h2o.gbm, h2o.randomForest etc.  
class MethodParams ranges params | ranges -> params, params -> ranges where
  generateMethodParams :: ranges -> [params]

instance MethodParams GBM.GbmParamsRanges GBM.GbmParams where
  generateMethodParams = GBM.generateGbmParams

instance MethodParams RF.RandomForestParamsRanges RF.RandomForestParams where
  generateMethodParams = RF.generateRandomForestParams  

instance MethodParams RGF.RgfParamsRanges RGF.RgfParams where
  generateMethodParams = RGF.generateRgfParams  

-- | Params of job for h2o-cluster
data Job a = 
  Job { inputParams  :: InputParams
      , methodParams :: a
      , outputParams :: OutputParams 
      } deriving (Show, Generic)

instance FromJSON (Job GBM.GbmParams)
instance ToJSON (Job GBM.GbmParams)

instance FromJSON (Job RF.RandomForestParams)
instance ToJSON (Job RF.RandomForestParams)

instance FromJSON (Job RGF.RgfParams)
instance ToJSON (Job RGF.RgfParams)

data InputParams = 
  InputParams { dataFilename :: FilePath
              } deriving (Show, Generic)

instance FromJSON InputParams
instance ToJSON InputParams

data OutputParams = 
  OutputParams { msePlotFileName    :: FilePath
               , confMatrixFileName :: FilePath
               , paramsFileName     :: FilePath
               } deriving (Show, Generic)

instance FromJSON OutputParams
instance ToJSON OutputParams