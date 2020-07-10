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
import Data.List (intercalate)
import Data.Maybe (fromJust,isJust,fromMaybe)
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



-- | Record the simulations and return a record of the details.
recordSimulationOutput :: ModelParameters
                       -> ([Observation], Int) -- ^ observations and identifier for output
                       -> IO LlhdAndData
recordSimulationOutput (ModelParameters params _) (obs,simNum) =
  let obsJson = observationsJsonFilePath simNum
      simSize = length obs
      obsLlhd = fst $ llhdAndNB obs params initLlhdState
      in do B.writeFile obsJson $ JSON.encode obs
            return $ LlhdAndData simSize obsLlhd obsJson



-- | Generate a random simulation of the observations and return it along with
-- an identification integer.
getObservations :: ModelParameters -> Int -> IO ([Observation],Int)
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
                             -> ([Observation], Int)
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

-- | The name of the file to write the simulation observations to.
observationsJsonFilePath :: Int -> FilePath
observationsJsonFilePath n =
  let paddedLength = 4 :: Int
      idString = fromMaybe (show n) (leftPad '0' paddedLength (show n))
   in "out/simulated-observations-" ++ idString ++ ".json"


main :: IO ()
main =
  let modelParams = ModelParameters (1.5,0.3,0.3,[],0.3,[]) 5
      simIds = [1..20] :: [Int]
      outputCsvFilePath = "out/simulation-sizes-and-llhds.csv" :: FilePath
    in do putStrLn appMessage
          simObsWithId <- mapM (getObservations modelParams) simIds
          records <- mapM (recordSimulationOutput modelParams) simObsWithId
          B.writeFile outputCsvFilePath $ CSV.encode records
          defaultMain $ map (benchmarkableLlhdEvaluations modelParams) simObsWithId

