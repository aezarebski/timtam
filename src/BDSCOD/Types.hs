{-# LANGUAGE DeriveGeneric #-}

module BDSCOD.Types where

-- import Control.DeepSeq
-- import Data.Aeson
-- import Epidemic.Types
-- import GHC.Generics (Generic)


-- instance NFData Event

-- instance ToJSON Event

-- instance FromJSON Event

import Epidemic.Types.Parameter

type NumLineages = Double

data ObservedEvent
  = OBirth
  | OSample
  | OOccurrence
  | OCatastrophe NumLineages
  | ODisaster NumLineages
  deriving (Show, Eq)
>>>>>>> master

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
