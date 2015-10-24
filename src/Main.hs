{-# Language OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Text as T 
import qualified Data.ByteString.Lazy as BS
import System.Environment

import Types
import RgfParams as RGF

---------------------------------
-------Auxiliary Functions-------
---------------------------------

-- | Generate infinite list of config filenames like [cfg1.json. cfg2.json,...],
-- | prefixed by specified dir
filenames :: FilePath -> [FilePath]
filenames dir = map namewrap [1..]
  where 
    namewrap n = dir ++ "cfg" ++ show n ++ ".json" 

-----------------------
-------Constants-------
-----------------------
-- Символ * в имени файла -- место для подстановки номера y
--        # -- для номера конфигурационного файла  
ioRgfValidateOnTest :: RGF.InOutParamsRgf 
ioRgfValidateOnTest = 
  RGF.InOutParamsRgf 
      { RGF.train_xy_fn = ("data/train.data.x", "data/train.data*.y")
      , RGF.test_x_fn   = "data/test.data.x"    
      , RGF.model_fn_prefix = "results/m*_test#"
      }

-- Символ * в имени файла -- место для подстановки номера y
--        # -- для номера конфигурационного файла
ioRgfValidateOnTrain :: RGF.InOutParamsRgf 
ioRgfValidateOnTrain = 
  RGF.InOutParamsRgf 
      { RGF.train_xy_fn = ("data/train.data.x", "data/train.data*.y")
      , RGF.test_x_fn   = "data/train.data.x"    
      , RGF.model_fn_prefix = "results/m*_train#"
      }

defaultRgfParamsRanges :: RGF.RgfParamsRanges
defaultRgfParamsRanges = 
  RGF.RgfParamsRanges { saveLastModelOnly_range = [True]
                      , algorithm_range         = ["RGF"]
                      , loss_range              = ["LS"]
                      , max_leaf_forest_range   = [3000]
                      , test_interval_range     = [50]
                      , reg_L2_range            = [1]
                      , verbose_range           = [True]
                      }

-----------------------
-------Main Code-------
-----------------------
generateJobs :: ( InOutParams  inout
                , MethodParams ranges params
                ) => [inout] -> ranges -> [Job inout params]
generateJobs io ranges = 
  Job                           <$>
    io                          <*>
    generateMethodParams ranges 
    
saveJobsRgf :: 
  [Job RGF.InOutParamsRgf RGF.RgfParams] -> IO ()
saveJobsRgf jobs = 
  zipWithM_ BS.writeFile 
            (filenames "output/") 
            (map encodePretty $ jobs)  

specifyYInIoParams :: InOutParamsRgf -> Int -> InOutParamsRgf
specifyYInIoParams ioparams y =
  let train_x_fn' = fst . train_xy_fn $ ioparams
      train_y_fn' = substituteY $ T.pack . snd . train_xy_fn $ ioparams
      model_fn_prefix' = substituteY $ T.pack . model_fn_prefix $ ioparams
  in ioparams { train_xy_fn = (train_x_fn', T.unpack train_y_fn')
              , model_fn_prefix = T.unpack model_fn_prefix'
              }
  where 
    substituteY = T.replace "*" (T.pack $ show y)

specifyCfgNumberInIoParams :: InOutParamsRgf -> Int -> InOutParamsRgf
specifyCfgNumberInIoParams ioparams n =
  let model_fn_prefix' = T.replace "#" (T.pack $ show n) $ 
                           T.pack . model_fn_prefix $ ioparams
  in ioparams { model_fn_prefix = T.unpack model_fn_prefix'
              }

main = do
  saveJobsRgf $ 
    generateJobs
      (zipWith specifyCfgNumberInIoParams 
        (concat [ map (specifyYInIoParams ioRgfValidateOnTest) [0..7]
                , map (specifyYInIoParams ioRgfValidateOnTrain) [0..7]]
        )
        [1..]
      )
      defaultRgfParamsRanges