{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BDSCOD.Types where

import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.Csv as Csv
import Data.Aeson
import Epidemic.Types.Parameter
import GHC.Generics (Generic)

-- | The parameters of the constant rate BDSCOD are the birth rate, the natural
-- removal rate, the sampling rate, the timing and probability of catastrophic
-- removal, the occurrence rate, and the timing the probability of removal due
-- to disaster.
type Parameters
   = (Rate, Rate, Rate, [(Time, Probability)], Rate, [(Time, Probability)])

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


type Observation = (Time, ObservedEvent)

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

instance Csv.ToRecord NegativeBinomial

data PDESolution = PDESol NegativeBinomial NumLineages

type LogLikelihood = Double

type LlhdAndNB = (LogLikelihood,NegativeBinomial)

type LlhdCalcState = (LlhdAndNB
                     ,Time
                     ,NumLineages)

