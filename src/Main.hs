{-# Language OverloadedStrings #-}
{-# Language DeriveGeneric #-}

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Tuple.Curry(uncurryN)
import qualified Data.Text as T 
import GHC.Generics
import qualified Data.ByteString.Lazy as BS

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

data GbmParams = 
  GbmParams { y      :: FactorName
            , xs     :: [FactorNumber]
            , ntrees :: Int
            } deriving (Show, Generic)

instance FromJSON GbmParams
instance ToJSON GbmParams

---------------------------------
-------Auxiliary Functions-------
---------------------------------

-- | Generate infinite list of config filenames like [cfg1.json. cfg2.json,...]
-- | and put into specified dir
filenames :: FilePath -> [FilePath]
filenames dir = map namewrap [1..]
  where 
    namewrap n = dir ++ "cfg" ++ show n ++ ".json" 

-----------------------
-------Constants-------
-----------------------

inp1 :: InputParams
inp1 = 
  InputParams { dataFilename = "data.csv"
              }

out1 :: OutputParams
out1 = 
  OutputParams { msePlotFileName    = "plot.png"
               , confMatrixFileName = "confMatr.csv"
               , gbmParamsFileName  = "gbmParams.json"
               }
-----------------------
-------Main Code-------
-----------------------

generateGbmParams :: [GbmParams]
generateGbmParams = map (uncurryN GbmParams) $ 
  [(y, xs, ntrees) 
  | y       <- ["responce"]
  , xs      <- [[1..3]]
  , ntrees  <- [1,2,3]
  ]

generateJobParams :: [JobParams]
generateJobParams = map (uncurryN JobParams) $ 
  [(input, gbm, output)
  | input  <- [inp1]
  , gbm    <- generateGbmParams
  , output <- [out1] 
  ]
main = 
  zipWithM_ BS.writeFile 
            (filenames "input/config/") 
            (map encodePretty generateJobParams)
      