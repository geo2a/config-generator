{-# Language DeriveGeneric #-}
module Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

-------------------------------
-------Domain Data Types-------
-------------------------------

-- | Params of job for h2o-cluster
data JobParams = 
  JobParams { inputParams  :: InputParams
            , gbmParams    :: GbmParams
            , outputParams :: OutputParams 
            } deriving (Show, Generic)

instance FromJSON JobParams
instance ToJSON JobParams

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

type FactorName = T.Text 

type FactorNumber = Int 

-- | Params of h2o.gbm procedure
data GbmParams = 
  GbmParams { y      :: FactorName
            , xs     :: [FactorNumber]
            , ntrees :: Int
            , max_depth :: Int
            , min_rows :: Int
            , learn_rate :: Double
            , nbins :: Int
            , nbins_cats :: Int
            , nfolds :: Int
            , balance_classes :: Bool
            , max_after_balance_size :: Double
            , score_each_iteration :: Bool
            } deriving (Show, Generic)

instance FromJSON GbmParams
instance ToJSON GbmParams