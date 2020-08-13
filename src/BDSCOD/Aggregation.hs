
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
aggregateUnscheduledObservations aggTimes obs =
  do
    let annObs = annotatedObs obs
    aggAnnObs <- aggregatedObs aggTimes annObs
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
aggregatedObs :: AggregationTimes
              -> [AnnotatedObservation]
              -> Maybe [AnnotatedObservation]
aggregatedObs aggTimes annObs
  | nullAggregationTimes aggTimes =
    if or [isSample o || isOccurrence o | (_, o) <- annObs]
      then Nothing
      else Just annObs
  | otherwise = do
    (aggTime, remAggTimes) <- extractFirstAggregationTime aggTimes
    partiallyAggAnnObs <- _aggregatedObs annObs aggTime
    aggregatedObs remAggTimes partiallyAggAnnObs

-- | Join all unscheduled observations before a given time into a single
-- scheduled observation.
_aggregatedObs :: [AnnotatedObservation] -> Time -> Maybe [AnnotatedObservation]
_aggregatedObs annObs aggTime =
  let beforeObs = filter (\(absTime, obs) -> absTime <= aggTime && (not . isSample) obs) annObs
      needAggObs = filter (\(absTime, obs) -> absTime <= aggTime && isSample obs) annObs
      afterObs = filter (\(absTime, _) -> absTime > aggTime) annObs
      in do aggObs <- _aggregateSamples needAggObs aggTime
            return $ mconcat [beforeObs,[aggObs],afterObs]

-- | Join a list of observed samples into a single catastrophe at the given
-- time.
_aggregateSamples :: [AnnotatedObservation] -> Time -> Maybe AnnotatedObservation
_aggregateSamples annSamples aggTime =
  if all (isSample . snd) annSamples
  then Just (aggTime,(undefined,OCatastrophe (fromIntegral $ length annSamples)))
  else Nothing

-- | Add the absolute time of each observation as an annotation to the
-- observation.
annotatedObs :: [Observation] -> [AnnotatedObservation]
annotatedObs obs =
  let absTimes = tail . scanl (+) 0 . map fst $ obs
  in zip absTimes obs

