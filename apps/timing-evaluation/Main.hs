{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
-- import Control.Monad (zipWithM)
import Criterion.Main
-- import Criterion.Types
import GHC.Generics (Generic)
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as B
import Data.List (intercalate,find)
import Data.Maybe (fromJust,isJust,fromMaybe,catMaybes)
import qualified Data.Csv as CSV
import qualified Epidemic.BDSCOD as BDSCOD
import Epidemic.Types.Parameter
import Epidemic.Utility (simulationWithSystemRandom)

appMessage :: String
appMessage =
  intercalate
    "\n"
    [""
    , "Time Complexity Application"
    , "---------------------------"
    , ""
    , "This application simulates several data sets of varying size and then evaluates"
    , "the LLHD function on them while measuring how long it takes. There is then an R"
    , "script to generate a visualisation and estimate the time complexity of the LLHD"
    , "function."
    , ""
    ]

-- | This is the parameters of the likelihood function and the duration of the
-- simulation.
data ModelParameters = ModelParameters (Rate, Rate, Rate, [(Time, Probability)], Rate, [(Time, Probability)]) Time
  deriving (Show, Eq, Generic)

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

-- | Record the simulations and return a record of the details.
recordSimulationOutput :: ModelParameters
                       -> Simulation
                       -> IO LlhdAndData
recordSimulationOutput (ModelParameters params _) sim@(obs,simNum) =
  let obsJson = observationsJsonFilePath simNum
      obsLlhd = fst $ llhdAndNB obs params initLlhdState
      in do B.writeFile obsJson $ JSON.encode obs
            return $ LlhdAndData (simulationSize sim) obsLlhd obsJson



-- | Generate a random simulation of the observations and return it along with
-- an identification integer.
getObservations :: ModelParameters -> Int -> IO Simulation
getObservations (ModelParameters params simDuration) simId =
  let simConfig = BDSCOD.configuration simDuration params
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


-- | Simple left padd function.
leftPad :: Char -> Int -> String -> Maybe String
leftPad c l x
  | length x == l = Just x
  | length x < l = leftPad c l . (c:) $ x
  | otherwise = Nothing

-- | Return a list of the first element of a list satisfying each predicate
-- allowing duplicates.
multipleFinds :: [(a -> Bool)] -- ^ predicates
              -> [a]
              -> [a]
multipleFinds predicates values =
  catMaybes $ map (\p -> find p values) predicates


-- | The name of the file to write the simulation observations to.
observationsJsonFilePath :: Int -> FilePath
observationsJsonFilePath n =
  let paddedLength = 4 :: Int
      idString = fromMaybe (show n) (leftPad '0' paddedLength (show n))
   in "out/simulated-observations-" ++ idString ++ ".json"


main :: IO ()
main =
  let modelParams = ModelParameters (1.5,0.3,0.3,[],0.3,[]) 5 -- lambda, mu, psi, rho, omega, nu
      simIds = [1..20] :: [Int] -- the indicies of the simulations
      simulationPredicates = [\s -> let n = simulationSize s in n > 10 * i && n <= 11 * i | i <- [1..30]]
      outputCsvFilePath = "out/simulation-sizes-and-llhds.csv" :: FilePath
    in do putStrLn appMessage
          randomSimulations <- mapM (getObservations modelParams) simIds
          let selectedSimulations = multipleFinds simulationPredicates randomSimulations
          putStrLn $ "There are " ++ (show $ length selectedSimulations) ++ " simulations that will be used."
          records <- mapM (recordSimulationOutput modelParams) selectedSimulations
          B.writeFile outputCsvFilePath $ CSV.encode records
          defaultMain $ map (benchmarkableLlhdEvaluations modelParams) selectedSimulations

