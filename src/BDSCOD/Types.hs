{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , isUnscheduledSequenced
  , isOccurrence
  , nbMV2SP
  , numUnsequenced
  , numSequenced
  , NegativeBinomial(..)
  , PDESolution(..)
  , LogLikelihood
  , LlhdAndNB
  , LlhdCalcState
  , MCMCConfiguration(..)
  , MWCSeed
  ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Aeson               as Json
import qualified Data.ByteString          as B
import qualified Data.ByteString.Builder  as BBuilder
import           Data.ByteString.Char8    (pack)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Csv                 as Csv
import           Data.List                (intersperse)
import           Epidemic.Types.Parameter
import           GHC.Generics             (Generic)
import           GHC.Word                 (Word32 (..))

-- | TODO Remove this once the version of @epi-sim@ has been updated to the
-- latest one on github.
instance Ord TimeDelta where
  (TimeDelta a) <= (TimeDelta b) = a <= b

-- | Alias for the type used to seed the MWC PRNG.
type MWCSeed = Word32

-- | These objects configure an MCMC run.
data MCMCConfiguration =
  MCMCConfiguration
    { -- | Where to write MCMC samples
      mcmcOutputCSV :: FilePath
      -- | The number of samples to generate
    , mcmcNumIters  :: Int
      -- | The standard deviation of the step size
    , mcmcStepSD    :: Double
      -- | The seed for the PRNG
    , mcmcSeed      :: MWCSeed
    }
  deriving (Show, Generic)

instance Json.FromJSON MCMCConfiguration

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
   = ( Rate
     , Rate
     , Rate
     , [(AbsoluteTime, Probability)]
     , Rate
     , [(AbsoluteTime, Probability)])

unpackParameters :: Parameters -> UnpackedParameters
unpackParameters (Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)) =
  (pLambda, pMu, pPsi, pRhos, pOmega, pNus)

packParameters :: UnpackedParameters -> Parameters
packParameters (pLambda, pMu, pPsi, pRhos, pOmega, pNus) =
  Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)

-- | Return the times of scheduled events: catastrophes and disasters.
scheduledTimes :: Parameters -> ([AbsoluteTime], [AbsoluteTime])
scheduledTimes (Parameters (_, _, _, Timed pRhos, _, Timed pNus)) =
  (fst <$> pRhos, fst <$> pNus)

-- | The number of lineages that exist in a phylogeny
type NumLineages = Double

-- | The type of events that can be observed under the BDSCOD.
--
--   * @OBirth@ event is an observed birth event.
--   * @ObsUnscheduledSequenced@ is an unscheduled sequenced removal.
--   * @OOccurrence@ is an unscheduled unsequenced removal.
--   * @OCatastrophe@ is an scheduled sequenced removal.
--   * @ODisaster@ is an scheduled unsequenced removal.
--
data ObservedEvent
  = OBirth
  | ObsUnscheduledSequenced
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
  toField ObsUnscheduledSequenced = "osample"
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
type Observation = (TimeDelta, ObservedEvent)

-- | A setter function to return a new observation with a different delay.
updateDelay :: Observation -> TimeDelta -> Observation
updateDelay (_, oEvent) delay = (delay, oEvent)

-- | Predicate for the observation referring to a birth.
isBirth :: Observation -> Bool
isBirth = (== OBirth) . snd

-- | Predicate for the observation referring to an unscheduled and sequenced
-- observation.
isUnscheduledSequenced :: Observation -> Bool
isUnscheduledSequenced = (== ObsUnscheduledSequenced) . snd

-- | Predicate for the observation referring to an occurrence.
isOccurrence :: Observation -> Bool
isOccurrence = (== OOccurrence) . snd

-- | The number of /unsequenced/ lineages that were observed.
numUnsequenced :: Observation -> NumLineages
numUnsequenced obs =
  case snd obs of
    OBirth                  -> 0
    ObsUnscheduledSequenced -> 0
    OOccurrence             -> 1
    (OCatastrophe _)        -> 0
    (ODisaster n)           -> n

-- | The number of /sequenced/ lineages that were observed.
numSequenced :: Observation -> NumLineages
numSequenced obs =
  case snd obs of
    OBirth                  -> 0
    ObsUnscheduledSequenced -> 1
    OOccurrence             -> 0
    (OCatastrophe n)        -> n
    (ODisaster _)           -> 0

-- | The negative binomial distribution extended to include the limiting case of
-- a point mass at zero. The parameterisation is in terms of a positive
-- parameter /r/, the size, and a probability /p/, the mean of the distribution
-- is /p * r \/ (1 - p)/.
--
-- __WARNING__ The parameterisation used is the same as the one on Wikipedia,
-- but not the same as the one used by R which uses /1-p/ as the probability.
data NegativeBinomial
  = Zero -- ^ A point mass at zero
  | NegBinomSizeProb Double Probability
  | NegBinomMeanVar Double Double
  deriving (Show, Generic)

-- | Being able to convert the mean and variance parameterisation into the
-- standard size and probability parameterisation is useful but this is a
-- partial function so we reserve the option of an error string.
nbMV2SP :: NegativeBinomial -> Either String NegativeBinomial
nbMV2SP nb =
  case nb of
    (NegBinomMeanVar 0 _) -> Right Zero
    (NegBinomMeanVar m v) ->
      if m > 0 && v >= m
        then Right $ NegBinomSizeProb r p
        else Left $ "nbMV2SP received bad input data: " ++ show (m, v)
      where r = (m ** 2) / (v - m)
            p = (v - m) / v
    _ -> Right nb

instance Csv.ToField NegativeBinomial where
  toField nb =
    case nb of
      Zero -> "Zero"
      (NegBinomSizeProb r p) ->
        strictByteString . BBuilder.toLazyByteString . mconcat $
        intersperse
          del
          [ BBuilder.stringUtf8 "NBSizeProb"
          , BBuilder.doubleDec r
          , BBuilder.doubleDec p
          ]
        where del = BBuilder.charUtf8 ' '
      (NegBinomMeanVar _ _) ->
        case nbMV2SP nb of
          Right negbinom -> Csv.toField negbinom
          Left errMssg   -> pack errMssg

data PDESolution =
  PDESol NegativeBinomial NumLineages

type LogLikelihood = Double

type LlhdAndNB = (LogLikelihood, NegativeBinomial)

type LlhdCalcState = (LlhdAndNB, AbsoluteTime, NumLineages)
