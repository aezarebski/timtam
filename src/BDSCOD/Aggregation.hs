{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module BDSCOD.Aggregation
  ( aggregateUnscheduledObservations
  , AggregationTimes()
  , pattern AggTimes
  , maybeAggregationTimes
  ,extractFirstAggregationTime
  ,nullAggregationTimes
  , AggregatedObservations(..)
  ) where

import BDSCOD.Types
import Data.List (sort,sortBy)
import GHC.Generics (Generic)
import Epidemic.Types.Parameter

-- | The times at which unscheduled samples are aggregated up to as part of the
-- observation process.
--
-- This does allow for a case in which there are no such times. The times are
-- ordered from smallest to largest and refer to /forward/ times, i.e., positive
-- from the origin time. The additional 'ObservedEvent' is there to describe
-- whether the aggregation refers to the aggregation of sequenced or unsequenced
-- samples.
newtype AggregationTimes =
  AggregationTimes_ [(Time,ObservedEvent)]
  deriving (Show, Eq, Generic)

-- | A smart constructor which only creates an `AggregationTimes` if the
-- provided `Time`s are sorted and non-negative, since these represent absolute
-- times. If the given list of times is empty, then this returns an empty list
-- of `AggregationTimes`.
maybeAggregationTimes :: [Time] -> [Time] -> Maybe AggregationTimes
maybeAggregationTimes seqAggTimes unseqAggTimes
  | null seqAggTimes && null unseqAggTimes = Just (AggregationTimes_ [])
  | null seqAggTimes && validAggTimes unseqAggTimes = Just (AggregationTimes_ (pairUpUnseq unseqAggTimes))
  | validAggTimes seqAggTimes && null unseqAggTimes = Just (AggregationTimes_ (pairUpSeq seqAggTimes))
  | validAggTimes seqAggTimes && validAggTimes unseqAggTimes = Just (AggregationTimes_ (pairUpBoth seqAggTimes unseqAggTimes))
  | otherwise = Nothing
  where
    validAggTimes aggTimes = sort aggTimes == aggTimes && minimum aggTimes >= 0
    pairUp oe ats  = [(t,oe) | t <- ats]
    pairUpUnseq = pairUp OOccurrence
    pairUpSeq = pairUp ObsUnscheduledSequenced
    pairUpBoth seqATs unseqATs = sortBy (\(a,_) (b,_) -> compare a b) (pairUpSeq seqATs ++ pairUpUnseq unseqATs)

-- | Use a pattern so we can force the construction via 'maybeAggregationTimes'
-- so that we can assume that they have sorted and non-negative times.
pattern AggTimes :: [(Time,ObservedEvent)] -> AggregationTimes
pattern AggTimes ts <- AggregationTimes_ ts

-- | Aggregated observations which contains aggregation times and the
-- observations which fall on those times. This is the result of adjusting the
-- delays in the times of unscheduled events up to the `AggregationTimes`. Note
-- that this includes the observations that are not aggregated, such as the
-- birth events.
data AggregatedObservations =
  AggregatedObservations AggregationTimes [Observation]
  deriving (Show, Eq)

-- | Return the first aggregation time and the type of unscheduled observation
-- to aggregate in it, along with a new set of aggregation times with the first
-- one removed. Since there is a smart constructor, we assume that the initial
-- @AggregationTimes@ value is valid.
extractFirstAggregationTime :: AggregationTimes -> Maybe ((Time,ObservedEvent),AggregationTimes)
extractFirstAggregationTime (AggregationTimes_ ts) = case ts of
  [] -> Nothing
  [t] -> Just (t, AggregationTimes_ [])
  (t:ts') -> Just (t, AggregationTimes_ ts')

-- | Predicate for there being no aggregation times in the @AggregationTimes@.
nullAggregationTimes :: AggregationTimes -> Bool
nullAggregationTimes (AggregationTimes_ ts) = null ts

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
    if or [isUnscheduledSequenced o || isOccurrence o | (_, o) <- annObs]
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
  let beforeObs = filter (\(absTime, obs) -> absTime <= aggTime && (not . isUnscheduledSequenced) obs) annObs
      needAggObs = filter (\(absTime, obs) -> absTime <= aggTime && isUnscheduledSequenced obs) annObs
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
  if all (isUnscheduledSequenced . snd) annSamples
    then Just
           (aggTime, (-1/0, OCatastrophe (fromIntegral $ length annSamples)))
    else Nothing

-- | Add the absolute time of each observation as an annotation to the
-- observation.
annotatedObs :: [Observation] -> [AnnotatedObservation]
annotatedObs obs =
  let absTimes = tail . scanl (+) 0 . map fst $ obs
  in zip absTimes obs

