{-# LANGUAGE DeriveGeneric #-}

module Main where

import BDSCOD.Llhd (llhdAndNB)
import BDSCOD.Types
  ( LlhdCalcState
  , LogLikelihood
  , MCMCConfiguration(..)
  , NegativeBinomial(..)
  , Observation(..)
  , ObservedEvent(..)
  , Parameters(..)
  )
import BDSCOD.Utility (eventsAsObservations, nbFromMAndV)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack, singleton)
import qualified Data.Csv as Csv
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as Unboxed
import Epidemic.BDSCOD (allEvents, configuration, observedEvents)
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , asNewickString
  , maybeEpidemicTree
  , maybeReconstructedTree
  )
import Epidemic.Types.Parameter
  ( AbsoluteTime(..)
  , Probability
  , Rate
  , TimeDelta(..)
  , Timed(..)
  , timeAfterDelta
  )
import Epidemic.Types.Population
  ( Identifier(..)
  , People(..)
  , Person(..)
  , numPeople
  )
import Epidemic.Utility (simulation')
import GHC.Generics (Generic)
import Numeric.MCMC.Metropolis (Chain(..), chain)
import System.Environment (getArgs)
import System.Random.MWC (initialize)
import Text.Printf (printf)


-- | Configuration of the @prevalence-at-tmrca@ program. This information is to
-- be specified by a file supplied at the command line.
data AppConfiguration =
  AppConfiguration
    { -- | Filepath for the CSV output of the whole epidemic simulation
      acEpiEventsCsv :: FilePath
      -- | Filepath for the CSV output of the observed data
    , acObservationsCsv :: FilePath
      -- | The configuration of the MCMC
    , acMCMCConfig :: MCMCConfiguration
    }
  deriving (Show, Generic)

instance Json.FromJSON AppConfiguration

-- | An informative error message
type ErrorMessage = String

-- | A result or an error message if the computation failed.
type Result a = Either ErrorMessage a

main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  maybeConfig <-
    Json.decode <$> L.readFile configFilePath :: IO (Maybe AppConfiguration)
  case maybeConfig of
    Nothing ->
      printf
        "Could not read the application configuration from %s\n"
        configFilePath
    (Just appConfig) -> do
      printf "Successfully read configuration file: %s\n" configFilePath
      epidemicEvents <- simulateEpidemic
      case epidemicEvents of
        (Right epiEvents) -> do
          let epiEventsCsv = acEpiEventsCsv appConfig
          printf "Writing epidemic events to %s\n" epiEventsCsv
          L.writeFile epiEventsCsv (Csv.encode epiEvents)
          let allObs = observationsOfEpidemic =<< epidemicEvents
              (Right obsFromTmrca) =
                restartObservationsAtTmrca (AbsoluteTime 0) =<< allObs
              llhdFun = llhdFunc obsFromTmrca
              mcmcConfig = acMCMCConfig appConfig
              numMcmcIters = mcmcNumIters mcmcConfig
              stepSd = mcmcStepSD mcmcConfig
              mcmcOutputCsv = mcmcOutputCSV mcmcConfig
              variableNames = ["nbMean", "nbVar", "lambda"]
              obsCsv = acObservationsCsv appConfig
           in do printf "Writing observations to %s\n" obsCsv
                 L.writeFile obsCsv (Csv.encode obsFromTmrca)
                 genIO <- prngGen (mcmcSeed mcmcConfig)
                 chainVals <-
                   chain numMcmcIters stepSd [5.0, 7.0, 2.0] llhdFun genIO
                 printf "Writing MCMC samples to %s\n" mcmcOutputCsv
                 L.writeFile
                   mcmcOutputCsv
                   (chainAsByteString variableNames chainVals)
        (Left errMessage) -> putStrLn errMessage

-- | A bytestring representation of the MCMC samples.
chainAsByteString ::
     [String] -- ^ the names of the elements of the chain
  -> [Chain [Double] b] -- ^ the samples in the chain
  -> L.ByteString
chainAsByteString varNames chainVals =
  let header = pack $ intercalate "," ("llhd" : varNames)
      records = Csv.encode [chainScore cv : chainPosition cv | cv <- chainVals]
      linebreak = singleton '\n'
   in mconcat [header, linebreak, records]

-- | A generator for random numbers from a seed.
prngGen seed = initialize (Unboxed.fromList [seed])

-- | Either an error message of a simulation configuration.
simulationConfiguration =
  case configuration (AbsoluteTime 11.0) (2.0, 0.5, 0.3, [], 0.5, []) of
    Just config -> Right config
    Nothing -> Left "configuration failed to construct simulation configuration"

-- | Simulate an epidemic and return all the events.
simulateEpidemic :: IO (Result [EpidemicEvent])
simulateEpidemic =
  let eitherSimConfig = simulationConfiguration
   in do genIO <- prngGen 7
         case eitherSimConfig of
           Right simConfig -> do
             sim <- simulation' simConfig allEvents genIO
             return $ Right sim
           Left errMsg -> return $ Left errMsg

-- | The observations made during the epidemic.
observationsOfEpidemic :: [EpidemicEvent] -> Result [Observation]
observationsOfEpidemic epiEvents =
  case observedEvents epiEvents of
    Just observedEpiEvents -> Right $ eventsAsObservations observedEpiEvents
    Nothing -> Left "observedEvents failed to extract observed events"

-- | The TMRCA of the observations of the epidemic given an origin time.
tmrcaOfObservations ::
     AbsoluteTime -- ^ origin time
  -> [Observation] -- ^ events observed during the epidemic
  -> Result AbsoluteTime
tmrcaOfObservations _ [] = Left "tmrcaOfObservations failed to find TMRCA"
tmrcaOfObservations currAbsTime ((td, observedEvent):os) =
  let nextAbsTime = timeAfterDelta currAbsTime td
   in case observedEvent of
        OBirth -> Right nextAbsTime
        _ -> tmrcaOfObservations nextAbsTime os

-- | The observations that occurred /after/ the absolute time.
dropObservationsBefore ::
     AbsoluteTime -- ^ origin time
  -> AbsoluteTime -- ^ threshold to drop prior to
  -> [Observation] -- ^ events observed during epidemic
  -> [Observation]
dropObservationsBefore _ _ [] = []
dropObservationsBefore originTime threshTime obs =
  let timeDeltas = map fst obs
      absTimes = tail $ scanl timeAfterDelta originTime timeDeltas
   in [o | (at, o) <- zip absTimes obs, at >= threshTime]

-- | The observations not before the TMRCA with the first time delta set to
-- zero.
restartObservationsAtTmrca ::
     AbsoluteTime -- ^ origin time
  -> [Observation] -- ^ events observed during epidemic
  -> Result [Observation]
restartObservationsAtTmrca originTime obs = do
  tmrca <- tmrcaOfObservations originTime obs
  Right $ zeroFirstDelay (dropObservationsBefore originTime tmrca obs)
  where
    zeroFirstDelay os =
      case os of
        [] -> []
        (_, e):oss -> (TimeDelta 0, e) : oss

-- | The likelihood of the parameters having given rise to the given
-- observations which start from the TMRCA of the reconstructed tree.
llhdFunc :: [Observation] -> [Double] -> LogLikelihood
llhdFunc obsFromTmrca [m, v, l] =
  let params = Parameters (l, 0.5, 0.3, Timed [], 0.5, Timed [])
      llhdState = ((0, nbFromMAndV (m, v)), AbsoluteTime 0, 2)
   in fst $ llhdAndNB obsFromTmrca params llhdState
