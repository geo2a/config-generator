{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}

module Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

import GbmParams as GBM
import RandomForestParams as RF

-------------------------------
-------Domain Data Types-------
-------------------------------

-- | This typeclass abstracts params generation for various methods, such as 
-- | h2o.gbm, h2o.randomForest etc.  
class MethodParams ranges params | ranges -> params where
  generateMethodParams :: ranges -> [params]

instance MethodParams GBM.GbmParamsRanges GBM.GbmParams where
  generateMethodParams = GBM.generateGbmParams

instance MethodParams RF.RandomForestParamsRanges RF.RandomForestParams where
  generateMethodParams = RF.generateRandomForestParams  

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