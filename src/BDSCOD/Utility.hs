module BDSCOD.Utility
  ( eventsAsObservations
  ) where

import qualified Data.Vector as V
import qualified Epidemic as EpiSim
import Epidemic.Types
import BDSCOD.Llhd

-- | Convert simulation events to observation events
eventsAsObservations :: [EpiSim.Event] -> [Observation]
eventsAsObservations epiSimEvents =
  drop 1 . map fst $ scanl processEvent' ((0, Birth), 0) epiSimEvents

processEvent' :: (Observation, Time) -> EpiSim.Event -> (Observation, Time)
processEvent' (_, currTime) epiSimEvent =
  case epiSimEvent of
    (EpiSim.InfectionEvent absTime _ _) ->
      ((absTime - currTime, Birth), absTime)
    (EpiSim.SamplingEvent absTime _) -> ((absTime - currTime, Sample), absTime)
    (EpiSim.CatastropheEvent absTime (EpiSim.People persons)) -> ((absTime - currTime, Catastrophe . fromIntegral $ V.length persons), absTime)
    (EpiSim.OccurrenceEvent absTime _) ->
      ((absTime - currTime, Occurrence), absTime)
    (EpiSim.DisasterEvent absTime (EpiSim.People persons)) -> ((absTime - currTime, Disaster . fromIntegral $ V.length persons), absTime)
    _ -> error "Cannot handle epi-sim event"
