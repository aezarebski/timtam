{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Types
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as L
import Epidemic.Types.Parameter
import GHC.Generics
import System.Environment (getArgs)
-- import BDSCOD.Conditioning
-- import BDSCOD.Llhd
-- import BDSCOD.Types
-- import BDSCOD.Utility
-- import qualified Data.Aeson as Json
-- import qualified Data.ByteString.Builder as BBuilder
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.Csv as Csv
-- import Data.List (intercalate, intersperse)
-- import Data.Maybe
-- import Data.Map.Strict (Map(),fromList,toList)
-- import qualified Epidemic.BDSCOD as SimBDSCOD
-- import Epidemic.Types.Events
-- import Epidemic.Types.Parameter
-- import Epidemic.Types.Population
-- import qualified Epidemic.Utility as SimUtil
-- import GHC.Generics
-- import System.Environment (getArgs)

data Configuration =
  Configuration
    { simulatedEventsOutputFile :: FilePath
    , simulationParameters :: Parameters
    , simulationDuration :: Time
    }
  deriving (Show, Generic)

instance Json.FromJSON Configuration

type Simulation x = ReaderT Configuration (ExceptT String IO) x

simulationStudy :: Simulation ()
simulationStudy = do
  params <- asks simulationParameters
  liftIO $ print params
  return ()

runSimulationStudy :: Configuration -> IO ()
runSimulationStudy config = do
  result <- runExceptT (runReaderT simulationStudy config)
  case result of
    Left errMsg -> putStrLn errMsg
    Right _ -> return ()

-- Demonstration values to assist while prototyping.
demoConfig = getConfiguration "/home/aez/projects/bdscod/examples/simulation-study-time-series/ts-config.json"
-- (Just config) <- demoConfig

main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  maybeConfig <- getConfiguration configFilePath
  case maybeConfig of
    Nothing ->
      putStrLn $ "Could not get configuration from file: " ++ configFilePath
    Just config -> runSimulationStudy config

getConfiguration :: FilePath -> IO (Maybe Configuration)
getConfiguration fp = Json.decode <$> L.readFile fp
