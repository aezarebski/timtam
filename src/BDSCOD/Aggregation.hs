
module BDSCOD.Aggregation where

import BDSCOD.Types

-- | If all of the observations are unscheduled and occur before the last
-- aggregation time this will return the aggregated observations.
aggregateUnscheduledObservations :: AggregationTimes
                                 -> [Observation]
                                 -> Maybe AggregatedObservations
aggregateUnscheduledObservations = undefined


