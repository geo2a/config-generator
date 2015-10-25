{-# Language OverloadedStrings #-}

module Main where

import Data.Char (isDigit)
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

-- | Generate infinite list of config filenames like [cfg1.inp. cfg2.inp,...],
-- | prefixed by specified dir
filenames :: FilePath -> [FilePath]
filenames dir = map namewrap [1..]
  where 
    namewrap n = dir ++ "cfg" ++ show n ++ ".inp" 

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
  RGF.RgfParamsRanges { y_range                 = [0..6]
                      , saveLastModelOnly_range = [True]
                      , algorithm_range         = ["RGF", "RGF_Opt", "RGF_Sib"]
                      , loss_range              = ["LS", "Expo", "Log"]
                      , max_leaf_forest_range   = [3000]
                      , test_interval_range     = [50]
                      , reg_L2_range            = [1, 0.1, 0.01, 0.001]
                      , verbose_range           = [True]
                      }

--defaultRgfParamsRanges :: RGF.RgfParamsRanges
--defaultRgfParamsRanges = 
--  RGF.RgfParamsRanges { y_range                 = [0..6]
--                      , saveLastModelOnly_range = [True]
--                      , algorithm_range         = ["RGF"]
--                      , loss_range              = ["LS", "Expo"]
--                      , max_leaf_forest_range   = [3000]
--                      , test_interval_range     = [50]
--                      , reg_L2_range            = [1]
--                      , verbose_range           = [True]
--                      }

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

saveJobsRgf :: [Job RGF.InOutParamsRgf RGF.RgfParams] 
            -> IO ()
saveJobsRgf jobs =
  let fnames = map constructFileName jobs
  in zipWithM_ writeFile 
               (map ("output/" ++) fnames) 
               (map encodeRgf $ jobs)  

constructFileName :: Job RGF.InOutParamsRgf RGF.RgfParams -> FilePath 
constructFileName (Job ioparams params) =
  let prefix = if T.isInfixOf "test" $ T.pack . test_x_fn $ ioparams
               then "test"
               else "train"
      n = reverse . takeWhile isDigit . reverse . model_fn_prefix $ ioparams
  in prefix ++ "_predict_" ++ n ++ "_" ++ (show $ y params)

encodeRgf :: Job RGF.InOutParamsRgf RGF.RgfParams -> String
encodeRgf (Job ioparams params) = unlines $ 
  [ "train_x_fn="      ++ (show . fst . train_xy_fn $ ioparams)
  , "train_y_fn="      ++ (show . specifyY (y $ params) . snd . train_xy_fn $ ioparams)
  , ""
  , "test_x_fn="       ++ (show . test_x_fn $ ioparams)
  , ""
  , "model_fn_prefix=" ++ (show . specifyY (y $ params) . model_fn_prefix $ ioparams)
  , ""
  , if saveLastModelOnly params then "SaveLastModelOnly" else ""
  , ""
  , "#--- training parameters"
  , "algorithm="       ++ (show . algorithm $ params)
  , "reg_L2="          ++ (show . reg_L2 $ params) 
  , "loss="            ++ (show . loss $ params) 
  , "test_interval="   ++ (show . test_interval $ params)
  , "max_leaf_forest=" ++ (show . max_leaf_forest $ params)
  , if verbose params then "Verbose" else ""
  ]
  where
    specifyY :: Int -> FilePath -> FilePath
    specifyY y = T.unpack . T.replace "*" (T.pack $ show y) . T.pack

specifyCfgNumber :: Int 
                 -> Job RGF.InOutParamsRgf RGF.RgfParams 
                 -> Job RGF.InOutParamsRgf RGF.RgfParams
specifyCfgNumber n (Job ioparams params) = 
  let model_fn_prefix' = T.unpack . T.replace "#" (T.pack $ show n) . 
                         T.pack . model_fn_prefix $ ioparams
  in Job (ioparams {model_fn_prefix = model_fn_prefix'}) params

changeY :: RGF.RgfParamsRanges -> Int -> RGF.RgfParamsRanges 
changeY params y = 
  params {y_range = [y]}
main = 
  let jobsOnTest  = concat $ 
                    map (zipWith specifyCfgNumber [1..]) $ 
                    map (generateJobs [ioRgfValidateOnTest]) $ 
                                 map (changeY defaultRgfParamsRanges) [0..6]
      jobsOnTrain = concat $ 
                    map (zipWith specifyCfgNumber [1..]) $ 
                    map (generateJobs [ioRgfValidateOnTrain]) $ 
                                 map (changeY defaultRgfParamsRanges) [0..6]
  in saveJobsRgf $  
     jobsOnTest ++ jobsOnTrain