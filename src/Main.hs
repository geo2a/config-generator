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
-------Main Code-------
-----------------------

generateGbmParams :: [GbmParams]
generateGbmParams = map (uncurryN GbmParams) $ 
  [(y, xs, ntrees) 
  | y       <- ["responce"]
  , xs      <- [[1..3]]
  , ntrees  <- [1,2,3]
  ]

main = 
  zipWithM_ BS.writeFile 
            (filenames "input/config/") 
            (map encodePretty generateGbmParams)
      