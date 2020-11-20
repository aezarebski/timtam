{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module BDSCOD.Aggregation
  ( aggregateUnscheduledObservations
  , AggregationTimes()
  , pattern AggTimes
  , maybeAggregationTimes
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
                                 -> AggregatedObservations
aggregateUnscheduledObservations aggTimes obs =
  let annObs = annotatedObs obs
      aggAnnObs = aggregatedObs aggTimes annObs
      aggObs = withDelaysNoAnnotations aggAnnObs
    in AggregatedObservations aggTimes aggObs

-- | The observations with the new delays computed from the annotations which
-- have been removed.
withDelaysNoAnnotations :: [AnnotatedObservation] -> [Observation]
withDelaysNoAnnotations annObs =
  let absTimes = map fst annObs
      obs = map snd annObs
      collectDelay (currTime,_) newTime = (newTime,newTime-currTime)
      delays = map snd . tail . scanl collectDelay (0,0) $ absTimes
    in zipWith updateDelay obs delays

-- | Add the absolute time of each observation as an annotation to the
-- observation assuming that the first delay is since the origin time, zero.
annotatedObs :: [Observation] -> [AnnotatedObservation]
annotatedObs obs =
  let delayVals = map fst obs
      absTimes = (tail . scanl (+) 0) delayVals
  in zip absTimes obs

-- | Aggregate all annotated unscheduled observations into annotated scheduled
-- observations at the given aggregation times. If there are annotated
-- unscheduled observation occurs after the last aggregation time then it is
-- left as it is.
--
-- NOTE that this function will return an undefined value if the type of
-- unscheduled event to aggregate does not make sense. The variable @bogusDelay@
-- is because without already having computed the result the correct delay
-- values cannot be computed.
aggregatedObs :: AggregationTimes
              -> [AnnotatedObservation]
              -> [AnnotatedObservation]
aggregatedObs (AggregationTimes_ []) annObs = annObs
aggregatedObs (AggregationTimes_ ((aggTime,obsType):aTs)) annObs =
  let earlierAnnObs = filter (\(absT,(_,o)) -> absT < aggTime && o /= obsType) annObs
      numAggregated = fromIntegral . length $ filter (\(absT,(_,o)) -> absT < aggTime && o == obsType) annObs
      bogusDelay = -1/0
      aggAnnObs
        | obsType == ObsUnscheduledSequenced = (aggTime,(bogusDelay,OCatastrophe numAggregated))
        | obsType == OOccurrence = (aggTime,(bogusDelay,ODisaster numAggregated))
        | otherwise = undefined
      laterAnnObs = filter (\(absT,_) -> absT > aggTime) annObs
      annObs' = earlierAnnObs ++ [aggAnnObs] ++ laterAnnObs
    in aggregatedObs (AggregationTimes_ aTs) annObs'
