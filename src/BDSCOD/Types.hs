{-# LANGUAGE DeriveGeneric #-}

module BDSCOD.Types where

import Control.DeepSeq
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

type Observation = (Time, ObservedEvent)

data NegativeBinomial
  = Zero
  | NegBinom Double Probability
  deriving (Show)

data PDESolution = PDESol NegativeBinomial NumLineages

type LogLikelihood = Double
type LlhdCalcState = (LogLikelihood
                     ,Time
                     ,NumLineages
                     ,NegativeBinomial)
