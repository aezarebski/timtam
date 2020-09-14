
module BDSCOD.Aggregation
  ( aggregateUnscheduledObservations
  ) where

import BDSCOD.Types
import Epidemic.Types.Parameter


-- | The absolute time and the observation at that time. Recall that
-- `Observation` pairs only contain the time delay since the previous observable
-- event, not their absolute time.
type AnnotatedObservation = (Time, Observation)

-- | The observations where unscheduled observations have been rounded up to the
-- first aggregation time after they occurred. If there are unscheduled
-- observations after the final aggregation time the resut is nothing.
--
-- __NOTE__ that if there are any unscheduled observations remaining after the
-- last aggregation time then this will fail and return `Nothing`.
--
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
--
-- __NOTE__ that if there are any unscheduled observations remaining after the
-- last aggregation time then this will fail and return `Nothing`.
--
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
-- scheduled observation. This is in the maybe monad because aggregation is a
-- partial function.
_aggregatedObs :: [AnnotatedObservation]
               -> Time
               -> Maybe [AnnotatedObservation]
_aggregatedObs annObs aggTime =
  let beforeObs = filter (\(absTime, obs) -> absTime <= aggTime && (not . isSample) obs) annObs
      needAggObs = filter (\(absTime, obs) -> absTime <= aggTime && isSample obs) annObs
      afterObs = filter (\(absTime, _) -> absTime > aggTime) annObs
      in do aggObs <- _aggregateSamples needAggObs aggTime
            return $ mconcat [beforeObs,[aggObs],afterObs]

-- | Join a list of observed samples into a single catastrophe at the given
-- time. The delay on the returned catastrophe is `-Infinity`to make it clear
-- that this is not a real value since the true value depends on the preceeding
-- observation. If there are events that are not samples then this will return
-- nothing.
--
_aggregateSamples :: [AnnotatedObservation] -> Time -> Maybe AnnotatedObservation
_aggregateSamples annSamples aggTime =
  if all (isSample . snd) annSamples
    then Just
           (aggTime, (-1/0, OCatastrophe (fromIntegral $ length annSamples)))
    else Nothing

-- | Add the absolute time of each observation as an annotation to the
-- observation.
annotatedObs :: [Observation] -> [AnnotatedObservation]
annotatedObs obs =
  let absTimes = tail . scanl (+) 0 . map fst $ obs
  in zip absTimes obs

