module Main where

import BDSCOD.Types (Observation(..), ObservedEvent(..))
import BDSCOD.Utility (eventsAsObservations)
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
import System.Random.MWC (initialize)

-- | An informative error message
type ErrorMessage = String

-- | A result or an error message if the computation failed.
type Result a = Either ErrorMessage a

-- | A generator for random numbers from a seed.
prngGen seed = initialize (Unboxed.fromList [seed])

-- | Either an error message of a simulation configuration.
simulationConfiguration =
  case configuration (AbsoluteTime 0.8) (2.0, 0.5, 0.5, [], 0.5, []) of
    Just config -> Right config
    Nothing -> Left "configuration failed to construct simulation configuration"

main :: IO ()
main = do
  x <- simulateEpidemic
  let allObs = observationsOfEpidemic =<< x
      obsFromTmrca = restartObservationsAtTmrca (AbsoluteTime 0) =<< allObs
  print obsFromTmrca

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
