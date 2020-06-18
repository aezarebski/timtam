{-# LANGUAGE DeriveGeneric #-}

module BDSCOD.Types where

import Control.DeepSeq
import GHC.Generics (Generic)
import Epidemic.Types

type NumLineages = Double

data Event
  = Birth
  | Sample
  | Occurrence
  | Catastrophe NumLineages
  | Disaster NumLineages
  deriving (Show, Eq, Generic)

instance NFData Event

type Observation = (Time, Event)

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
