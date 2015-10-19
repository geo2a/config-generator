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

-------------------------------------
-------Method Params Typeclass-------
-------------------------------------

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

------------------------------------
-------I\O Params Typeclasses-------
------------------------------------

class InputParams a where

instance InputParams GBM.InputParamsGbm
instance InputParams RGF.InputParamsRgf

class OutputParams a where

instance OutputParams GBM.OutputParamsGbm
instance OutputParams RGF.OutputParamsRgf

-----------------------------------
-------Resulting Job Params -------
-----------------------------------

-- | Params of job for h2o-cluster
data Job a b c = 
  Job { inputParams  :: a
      , methodParams :: b
      , outputParams :: c 
      } deriving (Show, Generic)

instance FromJSON (Job GBM.InputParamsGbm GBM.GbmParams GBM.OutputParamsGbm)
instance ToJSON   (Job GBM.InputParamsGbm GBM.GbmParams GBM.OutputParamsGbm)

--instance FromJSON (Job RF.RandomForestParams)
--instance ToJSON   (Job RF.RandomForestParams)

instance FromJSON (Job RGF.InputParamsRgf RGF.RgfParams RGF.OutputParamsRgf)
instance ToJSON   (Job RGF.InputParamsRgf RGF.RgfParams RGF.OutputParamsRgf)
