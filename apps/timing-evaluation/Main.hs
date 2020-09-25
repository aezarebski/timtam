{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility

-- import Control.Monad (zipWithM)
import Criterion.Main

import Data.Aeson as JSON
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as CSV
import Data.List (find, intercalate)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Epidemic.BDSCOD as BDSCOD
import Epidemic.Types.Parameter hiding (Parameters(..))
import Epidemic.Utility (simulationWithSystemRandom)
-- import Criterion.Types
import GHC.Generics (Generic)
import Text.Printf (printf)

-- | This is the parameters of the likelihood function and the duration of the
-- simulation.
data ModelParameters = ModelParameters Parameters Time
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | The record of a computation including the size of the data set processed,
-- the computed value, the time taken and the path to the file where the input
-- data is stored.
data LlhdAndData =
  LlhdAndData
    { crSimulationSize :: Int
    , crLlhd :: LogLikelihood
    , crObservationFile :: FilePath
    }
  deriving (Show, Eq, Generic)

instance CSV.ToRecord LlhdAndData

type Simulation = ([Observation], Int)

-- | Return the number of observations in a simulated data set.
simulationSize :: Simulation -> Int
simulationSize = length . fst

-- TODO Clean up the `recordSimulationOutput` and `getObservations` actions to
-- ensure a predictable number of resulting simulations for analysis.

-- | Record the simulations and return a record of the details.
recordSimulationOutput :: ModelParameters
                       -> Simulation
                       -> IO LlhdAndData
recordSimulationOutput (ModelParameters params _) sim@(obs,simNum) =
  let obsJson = observationsJsonFilePath simNum
      obsLlhd = fst $ llhdAndNB obs params initLlhdState
      in do L.writeFile obsJson $ JSON.encode obs
            return $ LlhdAndData (simulationSize sim) obsLlhd obsJson

-- | Generate a random simulation of the observations and return it along with
-- an identification integer.
getObservations :: ModelParameters -> Int -> IO Simulation
getObservations (ModelParameters params simDuration) simId =
  let simConfig = BDSCOD.configuration simDuration (unpackParameters params)
    in if isJust simConfig
       then do simEvents <- simulationWithSystemRandom True (fromJust simConfig) BDSCOD.allEvents
               let maybeObs = eventsAsObservations <$> BDSCOD.observedEvents simEvents
               return (fromMaybe [] maybeObs, simId)
       else return ([],simId)


-- | Return a benchmark based on the given set of observations. The name of the
-- benchmark is the same as the JSON file into which the observations have been
-- written.
benchmarkableLlhdEvaluations :: ModelParameters
                             -> Simulation
                             -> Benchmark
benchmarkableLlhdEvaluations (ModelParameters params _) (obs,simId) =
  let simName = observationsJsonFilePath simId
  in bench simName $ nf (\o -> fst (llhdAndNB o params initLlhdState)) obs

-- | Return a list of the first element of a list satisfying each predicate
-- allowing duplicates.
multipleFinds :: [(a -> Bool)] -- ^ predicates
              -> [a]
              -> [a]
multipleFinds predicates values =
  catMaybes $ map (\p -> find p values) predicates


-- | The name of the file to write the simulation observations to.
observationsJsonFilePath :: Int -> FilePath
observationsJsonFilePath = printf "out/simulated-observations-%05d.json"




-- getMaybeAppConfig :: FilePath -> IO (Maybe AppConfig)
-- getMaybeAppConfig fp = JSON.decode <$> L.readFile fp

-- TODO This should read in a JSON file to configure the program, it will
-- suffice to take in a fixed configuration file.

-- main :: IO ()
-- main =
--   let duration = 6.0
--       modelParams = ModelParameters (Parameters (1.5,0.3,0.3,Timed [(duration - 1e-6,0.5)],0.3, Timed [])) duration -- lambda, mu, psi, rho, omega, nu
--       simIds = [1..1000] :: [Int] -- the indicies of the simulations
--       binWidth = 10
--       simulationPredicates = [\s -> let n = simulationSize s in n > binWidth * i && n <= binWidth * (i + 1) | i <- [1..20]]
--       outputCsvFilePath = "out/simulation-sizes-and-llhds.csv" :: FilePath
--     in do randomSimulations <- mapM (getObservations modelParams) simIds
--           let selectedSimulations = multipleFinds simulationPredicates randomSimulations
--           putStrLn $ "There are " ++ show (length selectedSimulations) ++ " simulations that will be used."
--           records <- mapM (recordSimulationOutput modelParams) selectedSimulations
--           L.writeFile outputCsvFilePath $ CSV.encode records
--           defaultMain $ map (benchmarkableLlhdEvaluations modelParams) selectedSimulations

-- | This type respresents a configuration of the running of this application.
-- The application will look for a file it can read this from.
data AppConfig =
  AppConfig
    { acDuration :: Time
    , acSimParams :: ModelParameters
    , acNumSims :: Int
    , acBinWidth :: Int
    , acOutputCsv :: FilePath
    }
  deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main =
  let configJson = "app-config.json"
   in do
    maybeAppConfig <- JSON.decode <$> L.readFile configJson
    case maybeAppConfig of
      (Just appConfig) -> runSimulationAndProfiling appConfig
      Nothing -> putStrLn $ "Could not parse application configuration from " ++ configJson

runSimulationAndProfiling :: AppConfig -> IO ()
runSimulationAndProfiling AppConfig {..} =
  let simulationPredicates = [\s -> let n = simulationSize s in n > acBinWidth * i && n <= acBinWidth * (i + 1) | i <- [1..20]]
    in do randomSimulations <- mapM (getObservations acSimParams) [1..acNumSims]
          let selectedSimulations = multipleFinds simulationPredicates randomSimulations
          putStrLn $ "There are " ++ show (length selectedSimulations) ++ " simulations that will be used."
          records <- mapM (recordSimulationOutput acSimParams) selectedSimulations
          L.writeFile acOutputCsv $ CSV.encode records
          defaultMain $ map (benchmarkableLlhdEvaluations acSimParams) selectedSimulations

foobar :: AppConfig
foobar =
  let duration = 6.0
      modelParams = ModelParameters (Parameters (1.5,0.3,0.3,Timed [(duration - 1e-6,0.5)],0.3, Timed [])) duration -- lambda, mu, psi, rho, omega, nu
      simIds = [1..1000] :: [Int] -- the indicies of the simulations
      binWidth = 10
      outputCsvFilePath = "out/simulation-sizes-and-llhds.csv" :: FilePath
    in AppConfig duration modelParams (length simIds) binWidth outputCsvFilePath
