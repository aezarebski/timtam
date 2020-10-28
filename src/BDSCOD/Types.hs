{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module BDSCOD.Types
  ( Parameters(..)
  , putLambda
  , putMu
  , putPsi
  , putRhos
  , putOmega
  , putNus
  , getLambda
  , getMu
  , getPsi
  , getRhos
  , getOmega
  , getNus
  , UnpackedParameters
  , unpackParameters
  , packParameters
  , scheduledTimes
  , NumLineages
  , ObservedEvent(..)
  , strictByteString
  , Observation
  , updateDelay
  , isBirth
  , isSample
  , isOccurrence
  , NegativeBinomial(..)
  , PDESolution(..)
  , LogLikelihood
  , LlhdAndNB
  , LlhdCalcState
  , AggregationTimes()
  , pattern AggTimes
  , maybeAggregationTimes
  ,extractFirstAggregationTime
  ,nullAggregationTimes
  , AggregatedObservations(..)) where

import Control.DeepSeq
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.List (intersperse,sort)
import Epidemic.Types.Parameter
import Foreign.Storable
import GHC.Generics (Generic)

-- | The parameters of the constant rate BDSCOD are the birth rate, the natural
-- removal rate, the sampling rate, the timing and probability of catastrophic
-- removal, the occurrence rate, and the timing the probability of removal due
-- to disaster.
newtype Parameters =
  Parameters (Rate, Rate, Rate, Timed Probability, Rate, Timed Probability)
  deriving (Show, Eq, Generic)

instance ToJSON Parameters

instance FromJSON Parameters

-- | The putLambda function returns a new parameter vector with the lambda rate
-- updated.
putLambda :: Parameters -> Rate -> Parameters
putLambda (Parameters (_, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)) pLambda =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)

-- | The getLambda function returns the lambda rate from a parameter vector.
getLambda :: Parameters -> Rate
getLambda (Parameters (l, _, _, _, _, _)) = l

putMu :: Parameters -> Rate -> Parameters
putMu (Parameters (pLambda, _, pPsi, Timed pRhos, pOmega, Timed pNus)) pMu =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)

getMu :: Parameters -> Rate
getMu (Parameters (_, m, _, _, _, _)) = m

putPsi :: Parameters -> Rate -> Parameters
putPsi (Parameters (pLambda, pMu, _, Timed pRhos, pOmega, Timed pNus)) pPsi =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)

getPsi :: Parameters -> Rate
getPsi (Parameters (_, _, p, _, _, _)) = p

putRhos :: Parameters -> Timed Probability -> Parameters
putRhos (Parameters (pLambda, pMu, pPsi, _, pOmega, Timed pNus)) (Timed pRhos) =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)

getRhos :: Parameters -> Timed Probability
getRhos (Parameters (_, _, _, trs, _, _)) = trs

putOmega :: Parameters -> Rate -> Parameters
putOmega (Parameters (pLambda, pMu, pPsi, Timed pRhos, _, Timed pNus)) pOmega =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)

getOmega :: Parameters -> Rate
getOmega (Parameters (_, _, _, _, o, _)) = o

putNus :: Parameters -> Timed Probability -> Parameters
putNus (Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, _)) (Timed pNus) =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)

getNus :: Parameters -> Timed Probability
getNus (Parameters (_, _, _, _, _, tns)) = tns

type UnpackedParameters
   = (Rate, Rate, Rate, [(Time, Probability)], Rate, [(Time, Probability)])

unpackParameters :: Parameters -> UnpackedParameters
unpackParameters (Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)) =
  (pLambda, pMu, pPsi, pRhos, pOmega, pNus)

packParameters :: UnpackedParameters -> Parameters
packParameters (pLambda, pMu, pPsi, pRhos, pOmega, pNus) =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)


-- | Return the times of scheduled events: catastrophes and disasters.
scheduledTimes :: Parameters -> ([Time],[Time])
scheduledTimes (Parameters (_, _, _, Timed pRhos, _, Timed pNus)) =
  let times = map fst
  in (times pRhos, times pNus)

-- | The number of lineages that exist in a phylogeny
type NumLineages = Double

-- | The type of events that can be observed under the BDSCOD.
data ObservedEvent
  = OBirth
  | OSample
  | OOccurrence
  | OCatastrophe NumLineages
  | ODisaster NumLineages
  deriving (Show, Eq, Generic)

instance NFData ObservedEvent

instance ToJSON ObservedEvent

instance FromJSON ObservedEvent

instance Csv.ToRecord ObservedEvent

strictByteString :: BL.ByteString -> B.ByteString
strictByteString = B.concat . BL.toChunks

instance Csv.ToField ObservedEvent where
  toField OBirth = "obirth"
  toField OSample = "osample"
  toField OOccurrence = "ooccurrence"
  toField (OCatastrophe nl) =
    strictByteString . BBuilder.toLazyByteString $
    BBuilder.stringUtf8 "ocatastrophe:" <> BBuilder.doubleDec nl
  toField (ODisaster nl) =
    strictByteString . BBuilder.toLazyByteString $
    BBuilder.stringUtf8 "odisaster:" <> BBuilder.doubleDec nl

-- | An observation contains the time since the last observation and the actual
-- event that was observed. This should not be confused with an epidemic event
-- which has holds the absolute time of the event. This is used because the
-- likelihood is defined in terms of intervals of time between events rather
-- than their abolute times.
type Observation = (Time, ObservedEvent)

-- | A setter function to return a new observation with a different delay.
updateDelay :: Observation -> Time -> Observation
updateDelay (_, oEvent) delay = (delay, oEvent)

-- | Predicate for the observation referring to a birth.
isBirth :: Observation -> Bool
isBirth = (==OBirth) . snd

-- | Predicate for the observation referring to a sampling.
isSample :: Observation -> Bool
isSample = (==OSample) . snd

-- | Predicate for the observation referring to an occurrence.
isOccurrence :: Observation -> Bool
isOccurrence = (==OOccurrence) . snd

-- | The negative binomial distribution extended to include the limiting case of
-- a point mass at zero. The parameterisation is in terms of a positive
-- parameter /r/, the size, and a probability /p/, the mean of the distribution
-- is /p * r \/ (1 - p)/.
--
-- __WARNING__ The parameterisation used is the same as the one on Wikipedia,
-- but not the same as the one used by R which uses /1-p/ as the probability.
data NegativeBinomial
  = Zero -- ^ A point mass at zero
  | NegBinom Double Probability
  deriving (Show, Generic)

instance Csv.ToField NegativeBinomial where
  toField Zero = "Zero"
  toField (NegBinom r p) =
    strictByteString . BBuilder.toLazyByteString . mconcat $
    intersperse
      del
      [BBuilder.stringUtf8 "NB", BBuilder.doubleDec r, BBuilder.doubleDec p]
    where
      del = BBuilder.charUtf8 ' '

instance Csv.ToRecord NegativeBinomial

data PDESolution = PDESol NegativeBinomial NumLineages

type LogLikelihood = Double

type LlhdAndNB = (LogLikelihood,NegativeBinomial)

type LlhdCalcState = (LlhdAndNB
                     ,Time
                     ,NumLineages)


-- | The times at which unscheduled event times are adjusted up to under the
-- aggregation process. This does allow for a case in which there are no such
-- times.
newtype AggregationTimes =
  AggregationTimes_ [Time]
  deriving (Show, Eq)

-- | A smart constructor which only creates an `AggregationTimes` if the
-- provided `Time`s are sorted and non-negative since these represent absolute
-- times. If the given list of times is empty, then this returns an empty list
-- of `AggregationTimes`.
maybeAggregationTimes :: [Time] -> Maybe AggregationTimes
maybeAggregationTimes ts
  | null ts = Just (AggregationTimes_ ts)
  | sort ts == ts && minimum ts >= 0 = Just (AggregationTimes_ ts)
  | otherwise = Nothing

pattern AggTimes ts <- AggregationTimes_ ts

-- | Aggregated observations which contains aggregation times and the
-- observations which fall on those times. This is the result of adjusting the
-- delays in the times of unscheduled events up to the `AggregationTimes`. Note
-- that this includes the observations that are not aggregated, such as the
-- birth events.
data AggregatedObservations =
  AggregatedObservations AggregationTimes [Observation]
  deriving (Show, Eq)

-- | Return the first time aggregation time and a new set of aggregation times
-- with the first one removed. Since there is a smart constructor, we assume
-- that the initial object has sorted times.
extractFirstAggregationTime :: AggregationTimes -> Maybe (Time,AggregationTimes)
extractFirstAggregationTime (AggregationTimes_ ts) = case ts of
  [] -> Nothing
  [t] -> Just (t, AggregationTimes_ [])
  (t:ts') -> Just (t, AggregationTimes_ ts')

-- | Predicate for there being no aggregation times in the `AggregationTimes`
-- object.
nullAggregationTimes :: AggregationTimes -> Bool
nullAggregationTimes (AggregationTimes_ ts) = null ts
