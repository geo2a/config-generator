{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language OverloadedStrings #-}
{-# Language TypeFamilies #-}

module Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

import GbmParams          as GBM
import RandomForestParams as RF

-------------------------------------
-------Method Params Typeclass-------
-------------------------------------

-- | This typeclass abstracts params generation for various methods, such as 
-- | h2o.gbm, h2o.randomForest etc.  
--class MethodParams ranges params | ranges -> params, params -> ranges where
--  generateMethodParams :: ranges -> [params]

class MethodParams params where
  type Ranges params :: *
  generateMethodParams :: (Ranges params) -> [params]

instance MethodParams GBM.GbmParams where
  type Ranges GBM.GbmParams = GBM.GbmParamsRanges
  generateMethodParams = GBM.generateGbmParams

------------------------------------
-------I\O Params Typeclasses-------
------------------------------------

class InOutParams a where

instance InOutParams GBM.InOutParamsGbm

-----------------------------------
-------Resulting Job Params -------
-----------------------------------

-- | Params of job for h2o-cluster
data Job inout method = 
  Job { ioParams     :: inout
      , methodParams :: method
      } deriving (Show, Generic)

instance FromJSON (Job GBM.InOutParamsGbm GBM.GbmParams)
instance ToJSON   (Job GBM.InOutParamsGbm GBM.GbmParams)
