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

type NumLineages = Double

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
  toField (OCatastrophe nl) = strictByteString . BBuilder.toLazyByteString $ mconcat [BBuilder.stringUtf8 "ocatastrophe:", BBuilder.doubleDec nl]
  toField (ODisaster nl) = strictByteString . BBuilder.toLazyByteString $ mconcat [BBuilder.stringUtf8 "odisaster:", BBuilder.doubleDec nl]


type Observation = (Time, ObservedEvent)

data NegativeBinomial
  = Zero
  | NegBinom Double Probability
  deriving (Show)

data PDESolution = PDESol NegativeBinomial NumLineages

type LogLikelihood = Double

type LlhdAndNB = (LogLikelihood,NegativeBinomial)

type LlhdCalcState = (LlhdAndNB
                     ,Time
                     ,NumLineages)

