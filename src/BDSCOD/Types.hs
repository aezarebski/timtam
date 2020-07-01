module BDSCOD.Types where


import Epidemic.Types.Parameter

type NumLineages = Double

data ObservedEvent
  = OBirth
  | OSample
  | OOccurrence
  | OCatastrophe NumLineages
  | ODisaster NumLineages
  deriving (Show, Eq)

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
