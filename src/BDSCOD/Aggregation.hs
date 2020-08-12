
module BDSCOD.Aggregation
  ( aggregateUnscheduledObservations
  ) where

import BDSCOD.Types
import Epidemic.Types.Parameter

-- | The absolute time and the observation at that time.
type AnnotatedObservation = (Time, Observation)

-- | The observations where unscheduled observations have been rounded up to the
-- first aggregation time after they occurred. If there are unscheduled
-- observations after the final aggregation time the resut is nothing.
aggregateUnscheduledObservations :: AggregationTimes
                                 -> [Observation]
                                 -> Maybe AggregatedObservations
aggregateUnscheduledObservations aggTimes@(AggregationTimes ats) obs =
  do
    let annObs = annotatedObs obs
    aggAnnObs <- aggregatedObs ats ([],annObs)
    let aggObs = withCorrectedDelays aggAnnObs
    return $ AggregatedObservations aggTimes aggObs

-- | The observations with the delays corrected by the annotations.
withCorrectedDelays :: [AnnotatedObservation] -> [Observation]
withCorrectedDelays annObs =
  let absTimes = map fst annObs
      obs = map snd annObs
      collectDelay (currTime,_) nextTime = (nextTime,nextTime-currTime)
      delays = map snd . tail . scanl collectDelay (0,0) $ absTimes
    in zipWith updateDelay obs delays


-- | Aggregated all unscheduled observations into scheduled observations at the
-- given aggregation times and return those and any unscheduled observations
-- that occurred after the last aggregation time.
aggregatedObs :: [Time] -- ^ the aggregation times
              -> ([AnnotatedObservation],[AnnotatedObservation])
              -> Maybe [AnnotatedObservation]
aggregatedObs = undefined


-- | Add the absolute time of each observation as an annotation to the
-- observation.
annotatedObs :: [Observation] -> [AnnotatedObservation]
annotatedObs obs =
  let absTimes = tail . scanl (+) 0 . map fst $ obs
  in zip absTimes obs

