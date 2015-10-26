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
import GbmParams as GBM
import RandomForestParams as RF
import Defaults

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
generateJobs :: ( InOutParams  inout
                , MethodParams ranges params
                ) => [inout] -> ranges -> [Job inout params]
generateJobs inout ranges = 
  Job                           <$>
    inout                       <*>
    generateMethodParams ranges 
    

saveJobsGbm :: 
  [Job GBM.InOutParamsGbm GBM.GbmParams] -> IO ()
saveJobsGbm jobs = 
  zipWithM_ BS.writeFile 
            (filenames "output/") 
            (map encodePretty $ jobs)

-- Слишком полиморфна, чтобы работать :(
--saveJobs :: MethodParams ranges params => [Job params] -> IO ()
--saveJobs jobs = 
--  zipWithM_ BS.writeFile 
--            (filenames "output/") 
--            (map encodePretty $ jobs) 

main = do
  --args <- getArgs
  --guard $ not . null $ args  
  --contents <- BS.readFile $ head args 
  --case decode contents of 
  --  Nothing -> 
  --    print "Error: invalid config file"
  --  Just ranges -> 
  --    saveJobsGbm $ generateJobs ranges
  saveJobsGbm $ generateJobs [defaultInOutGbm] defaultGbmParamsRanges
