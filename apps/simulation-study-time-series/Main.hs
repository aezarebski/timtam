{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Conditioning
import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import Control.Monad (liftM, zipWithM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import Data.List (intercalate, intersperse)
import Data.Maybe (fromJust)
import qualified Epidemic.BDSCOD as SimBDSCOD
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , asNewickString
  , eventTime
  , maybeEpidemicTree
  , maybeReconstructedTree
  )
import Epidemic.Types.Parameter
import Epidemic.Types.Population (Person(..))
import qualified Epidemic.Utility as SimUtil
import GHC.Generics
import System.Environment (getArgs)
-- import Data.Map.Strict (Map(),fromList,toList)

-- | These objects define the specifics for an inference evaluation which can be
-- run at differing points in the simulation to understand how differing amounts
-- of data influences the results.
data InferenceConfiguration =
  InferenceConfiguration
    { inferenceTime :: Time
    , reconstructedTreeOutputFiles :: (FilePath, FilePath)
    , observationsOutputCsv :: FilePath
    , llhdOutputCsv :: FilePath
    , negBinomCsv :: FilePath
    }
  deriving (Show, Generic)

-- | This object configures the whole evaluation of this program and is to be
-- read in from a suitable JSON file.
data Configuration =
  Configuration
    { simulatedEventsOutputCsv :: FilePath
    , simulationParameters :: Parameters
    , simulationDuration :: Time
    , simulationSizeBounds :: (Int,Int)
    , inferenceConfigurations :: [InferenceConfiguration]
    , evaluationParameters :: [Parameters]
    , partialEvaluationOutputCsv :: FilePath
    }
  deriving (Show, Generic)

instance Json.FromJSON Configuration

instance Json.FromJSON InferenceConfiguration

type Simulation x = ReaderT Configuration (ExceptT String IO) x

-- A BDSCOD simulation configuration based on the parameters in the environment.
bdscodConfiguration = do
  simParams <- asks simulationParameters
  simDur <- asks simulationDuration
  let bdscodConfig = SimBDSCOD.configuration simDur (unpackParameters simParams)
  case bdscodConfig of
    Nothing -> throwError "Could not construct BDSCOD configuration"
    (Just config) -> return config

partialSimulatedEpidemic bdscodConfig =
  do
    -- simulate the epidemic without conditioning because the conditioning code
    -- assumes there will be psi-samples. Use the system random to get a better
    -- idea of robustness of the simulation.
    simEvents <- liftIO $ SimUtil.simulationWithSystemRandom False bdscodConfig SimBDSCOD.allEvents
    (sizeLowerBound,sizeUpperBound) <- asks simulationSizeBounds
    if length simEvents > sizeLowerBound && length simEvents < sizeUpperBound
      then do infTimes <- map inferenceTime <$> asks inferenceConfigurations
              simEventsCsv <- asks simulatedEventsOutputCsv
              liftIO $ L.writeFile simEventsCsv (Csv.encode simEvents)
              return [filter (\e -> eventTime e <= infTime) simEvents | infTime <- infTimes]
      else do liftIO $ putStrLn "Repeating epidemic simulation..."
              partialSimulatedEpidemic bdscodConfig

-- Run the actual simulation and record the results before returning the dataset
-- of observations generated by this epidemic.
simulatedObservations :: InferenceConfiguration
                      -> [EpidemicEvent]
                      -> Simulation (InferenceConfiguration,[Observation])
simulatedObservations infConfig@InferenceConfiguration{..} simEvents = do
  let Just (newickBuilder,newickMetaData) = asNewickString (0, Person 1) =<< maybeReconstructedTree =<< maybeEpidemicTree simEvents
      maybeObs = eventsAsObservations <$> SimBDSCOD.observedEvents simEvents
      (reconNewickTxt,reconNewickCsv) = reconstructedTreeOutputFiles
  case maybeObs of
    (Just obs) -> do liftIO $ L.writeFile reconNewickTxt (BBuilder.toLazyByteString newickBuilder)
                     liftIO $ L.writeFile reconNewickCsv (Csv.encode newickMetaData)
                     liftIO $ L.writeFile observationsOutputCsv (Csv.encode obs)
                     return (infConfig,obs)
    Nothing -> throwError "Failed to simulate observations."



-- Run the evaluation of the log-likelihood profiles on a given set of
-- observations and write the result to file.
evaluateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
evaluateLLHD InferenceConfiguration{..} obs = do
  simParams <- asks simulationParameters
  evalParams <- asks evaluationParameters
  let comma = BBuilder.charUtf8 ','
      llhdVals = [fst $ llhdAndNB obs p initLlhdState | p <- evalParams]
      nBVal = pure . snd $ llhdAndNB obs simParams initLlhdState
      doublesAsString = BBuilder.toLazyByteString . mconcat . intersperse comma . map BBuilder.doubleDec
  liftIO $ L.writeFile llhdOutputCsv (doublesAsString llhdVals)
  liftIO $ L.writeFile negBinomCsv (Csv.encode nBVal)

-- | Record the partial results of the LLHD and NB to a CSV at the parameters
-- used in the simulation.
partialEvaluations :: [Observation] -> Simulation ()
partialEvaluations obs = do
  simParams <- asks simulationParameters
  partialEvalsCsv <- asks partialEvaluationOutputCsv
  let partialResults = snd $ verboseLlhdAndNB obs simParams initLlhdState
      records = [show l ++ "," ++ show nb | (l,nb) <- partialResults]
  liftIO $ Prelude.writeFile partialEvalsCsv (intercalate "\n" records)

-- Definition of the complete simulation study.
simulationStudy :: Simulation ()
simulationStudy = do
  bdscodConfig <- bdscodConfiguration
  pEpi <- partialSimulatedEpidemic bdscodConfig
  infConfigs <- asks inferenceConfigurations
  pObs <- zipWithM simulatedObservations infConfigs pEpi
  mapM_ (uncurry evaluateLLHD) pObs
  partialEvaluations (snd $ head pObs)
  return ()

main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  maybeConfig <- getConfiguration configFilePath
  case maybeConfig of
    Nothing ->
      putStrLn $ "Could not get configuration from file: " ++ configFilePath
    Just config -> do
      result <- runExceptT (runReaderT simulationStudy config)
      case result of
        Left errMsg -> putStrLn errMsg
        Right _ -> return ()

getConfiguration :: FilePath -> IO (Maybe Configuration)
getConfiguration fp = Json.decode <$> L.readFile fp
