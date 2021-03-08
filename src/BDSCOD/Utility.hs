{-# LANGUAGE OverloadedStrings #-}

module BDSCOD.Utility where

import qualified Data.Vector as V

import qualified Data.ByteString as B
import qualified Data.Csv as Csv
import Epidemic.Types.Parameter
import Epidemic.Types.Time
import Epidemic.Types.Events
import Epidemic.Types.Population
import qualified Epidemic.Types.Observations as EpiObs
import BDSCOD.Types

instance Csv.ToRecord EpidemicEvent where
  toRecord e = case e of
    (Infection t p1 p2) -> Csv.record ["infection", Csv.toField t, Csv.toField p1, Csv.toField p2]
    (Removal time person) -> Csv.record ["removal", Csv.toField time, Csv.toField person, "NA"]
    (IndividualSample t p isSeq) -> Csv.record [l, Csv.toField t, Csv.toField p, "NA"]
      where l = if isSeq then "sampling" else "occurrence"
    (PopulationSample t ps isSeq) -> Csv.record [l, Csv.toField t, Csv.toField ps, "NA"]
      where l = if isSeq then "catastrophe" else "disaster"
    Extinction -> Csv.record ["extinction", "NA", "NA", "NA"]
    StoppingTime -> Csv.record ["stop", "NA", "NA", "NA"]

instance Csv.ToField People where
  toField (People persons) =
    B.intercalate ":" $ V.toList $ V.map Csv.toField persons

instance Csv.ToField Identifier where
  toField (Identifier idNum) = Csv.toField idNum

instance Csv.ToField Person where
  toField (Person pid) = Csv.toField pid

instance Csv.ToField AbsoluteTime where
  toField (AbsoluteTime td) = Csv.toField td

instance Csv.ToField TimeDelta where
  toField (TimeDelta td) = Csv.toField td

-- | Check if two absolute times differ by such a small amount that they are
-- likely identical.
timesWithinEpsilon :: AbsoluteTime -> AbsoluteTime -> Bool
timesWithinEpsilon (AbsoluteTime a) (AbsoluteTime b) = abs (a - b) < 1e-13

-- | Convert @epi-sim@ observations events into 'Observation's from @timtam@,
-- this assumes that the epidemic events have already been filtered by to only
-- include the observable events. Since this is model specific functions such as
-- `observedEvents` are provided to do this.
eventsAsObservations :: [EpiObs.Observation] -> [Observation]
eventsAsObservations epiSimEvents =
  drop 1 . map fst $ scanl processEvent' ((TimeDelta 0, OBirth), AbsoluteTime 0) epiSimEvents

processEvent' :: (Observation, AbsoluteTime) -> EpiObs.Observation -> (Observation, AbsoluteTime)
processEvent' (_, currTime) (EpiObs.Observation epiSimEvent) =
  case epiSimEvent of
    (Infection absTime _ _) -> ((timeDelta currTime absTime, OBirth), absTime)
    (IndividualSample absTime _ True) -> ((timeDelta currTime absTime, ObsUnscheduledSequenced), absTime)
    (PopulationSample absTime people True) -> ((timeDelta currTime absTime, OCatastrophe . fromIntegral . numPeople $ people), absTime)
    (IndividualSample absTime _ False) -> ((timeDelta currTime absTime, OOccurrence), absTime)
    (PopulationSample absTime people False) -> ((timeDelta currTime absTime, ODisaster . fromIntegral . numPeople $ people), absTime)
    (Removal _ _) -> error "A removal event encountered in processEvent', this should never happen!"
    Extinction -> error "A extinction event encountered in processEvent', this should never happen!"
    StoppingTime -> error "A stopping time event encountered in processEvent', this should never happen!"

nbFromMAndV :: (Double, Double) -> NegativeBinomial
nbFromMAndV (0, 0) = Zero
nbFromMAndV (m, v) =
  if m > 0 && v >= m
    then NegBinomSizeProb r p
    else error $ "nbFromMAndV received bad values: " ++ show (m,v)
  where
    r = (m ** 2) / (v - m)
    p = (v - m) / v

mAndVFromNb :: NegativeBinomial -> (Double, Double)
mAndVFromNb (NegBinomSizeProb r p) = (m, v)
  where
    m = p * r / (1 - p)
    v = m / (1 - p)
mAndVFromNb Zero = (0,0)

-- | The PGF of the negative binomial distribution
--
-- __WARNING__ It is easy to get infinite values so try to use the @logNbPGF@
-- function instead if possible.
--
nbPGF :: NegativeBinomial -> Double -> Double
nbPGF nb z = case nb of
  Zero -> 1
  (NegBinomSizeProb r p) -> ((1 - p) / (1 - p * z)) ** r

nbPGF' :: NegativeBinomial -> Double -> Double
nbPGF' nb z = case nb of
  Zero -> 0
  (NegBinomSizeProb r p) -> (r * p / (1 - p)) * nbPGF (NegBinomSizeProb (r+1) p) z

nbPGF'' :: NegativeBinomial -> Double -> Double
nbPGF'' nb z = case nb of
  Zero -> 0
  (NegBinomSizeProb r p) -> (r * (r + 1) * (p / (1 - p)) ** 2.0) *
                      nbPGF (NegBinomSizeProb (r+2) p) z

-- | The log of the PGF of the negative binomial distribution.
logNbPGF :: NegativeBinomial -> Double -> Double
logNbPGF nb z = case nb of
  Zero -> 0
  (NegBinomSizeProb r p) -> r * (log (1 - p) - log (1 - p * z))

logNbPGF' :: NegativeBinomial -> Double -> Double
logNbPGF' nb z =
  case nb of
    Zero -> log 0
    (NegBinomSizeProb r p) ->
      log (r * p) - log (1 - p) + logNbPGF (NegBinomSizeProb (r + 1) p) z

logNbPGF'' :: NegativeBinomial -> Double -> Double
logNbPGF'' nb z =
  case nb of
    Zero -> log 0
    (NegBinomSizeProb r p) ->
      log (r * (r + 1)) + 2 * log (p / (1 - p)) +
      logNbPGF (NegBinomSizeProb (r + 2) p) z

-- | The jth derivative of the negative binomial PGF.
--
-- __WARNING__ It is easy to get an Infinite value out of this.
--
nbPGFdash :: Double -> NegativeBinomial -> Double -> Double
nbPGFdash j nb z =
  case nb of
    Zero -> 0
    (NegBinomSizeProb r p) ->
      pochhammer r j * (p / (1 - p)) ** j *
        nbPGF (NegBinomSizeProb (r + j) p) z

-- | The log of the jth derivative of the negative binomial PGF.
logNbPGFdash :: Double -> NegativeBinomial -> Double -> Double
logNbPGFdash j nb z =
  case nb of
    Zero -> log 0
    (NegBinomSizeProb r p) ->
      logPochhammer r j + j * log (p / (1 - p)) +
      logNbPGF (NegBinomSizeProb (r + j) p) z

pochhammer :: (Eq p, Num p) => p -> p -> p
pochhammer _ 0 = 1
pochhammer a i = (a + i - 1) * pochhammer a (i - 1)

logPochhammer :: (Eq p, Floating p) => p -> p -> p
logPochhammer _ 0 = 0
logPochhammer a i = log (a + i - 1) + logPochhammer a (i - 1)


-- | Return the probability from the log-odds
invLogit :: Double -> Probability
invLogit a = 1 / (1 + exp (- a))

-- | Return the log-odds from the probability
logit :: Probability -> Double
logit p = log (p / (1 - p))


-- | The log-sum-exp function
logSumExp :: (Floating a, Ord a) => [a] -> a
logSumExp xs = x' + log (sum [exp (x - x') | x <- xs])
               where x' = maximum xs
