{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Data.Tuple.Curry(uncurryN)

import GbmParams as GBM
import RandomForestParams as RF

-------------------------------
-------Domain Data Types-------
-------------------------------

-- | This typeclass abstracts params generation for various methods, such as 
-- | h2o.gbm, h2o.randomForest etc.  
class MethodParams a where
  generateMethodParams :: [a]

instance MethodParams GBM.GbmParams where
  generateMethodParams = GBM.generateGbmParams

instance MethodParams RF.RandomForestParams where
  generateMethodParams = RF.generateRandomForestParams  

-- | Params of job for h2o-cluster
data JobParams a = 
  JobParams { inputParams  :: InputParams
            , methodParams :: a
            , outputParams :: OutputParams 
            } deriving (Show, Generic)

instance FromJSON (JobParams GBM.GbmParams)
instance ToJSON (JobParams GBM.GbmParams)

instance FromJSON (JobParams RF.RandomForestParams)
instance ToJSON (JobParams RF.RandomForestParams)

data InputParams = 
  InputParams { dataFilename :: FilePath
              } deriving (Show, Generic)

instance FromJSON InputParams
instance ToJSON InputParams

data OutputParams = 
  OutputParams { msePlotFileName    :: FilePath
               , confMatrixFileName :: FilePath
               , gbmParamsFileName  :: FilePath
               } deriving (Show, Generic)

instance FromJSON OutputParams
instance ToJSON OutputParams