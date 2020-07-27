module BDSCOD.Utility where

import qualified Data.Vector as V

import Epidemic.Types.Parameter
import Epidemic.Types.Events
import Epidemic.Types.Population
import BDSCOD.Types
-- import BDSCOD.Llhd --

-- | Convert simulation events to observation events
eventsAsObservations :: [EpidemicEvent] -> [Observation]
eventsAsObservations epiSimEvents =
  drop 1 . map fst $ scanl processEvent' ((0, OBirth), 0) epiSimEvents

processEvent' :: (Observation, Time) -> EpidemicEvent -> (Observation, Time)
processEvent' (_, currTime) epiSimEvent =
  case epiSimEvent of
    (Infection absTime _ _) ->
      ((absTime - currTime, OBirth), absTime)
    (Sampling absTime _) -> ((absTime - currTime, OSample), absTime)
    (Catastrophe absTime (People persons)) -> ((absTime - currTime, OCatastrophe . fromIntegral $ V.length persons), absTime)
    (Occurrence absTime _) ->
      ((absTime - currTime, OOccurrence), absTime)
    (Disaster absTime (People persons)) -> ((absTime - currTime, ODisaster . fromIntegral $ V.length persons), absTime)
    (Removal _ _) -> error "A removal event has been passed to processEvent', this should never happen!"

-- | Predicate for the observation referring to a birth.
isBirth :: Observation -> Bool
isBirth (_,e) = e == OBirth

-- | Predicate for the observation referring to a sampling.
isSample :: Observation -> Bool
isSample (_,e) = e == OSample

nbFromMAndV :: (Double, Double) -> NegativeBinomial
nbFromMAndV (0, 0) = Zero
nbFromMAndV (m, v) =
  if m > 0 && v >= m
    then NegBinom r p
    else error $ "nbFromMAndV received bad values: " ++ show (m,v)
  where
    r = (m ** 2) / (v - m)
    p = (v - m) / v

mAndVFromNb :: NegativeBinomial -> (Double, Double)
mAndVFromNb (NegBinom r p) = (m, v)
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
  (NegBinom r p) -> ((1 - p) / (1 - p * z)) ** r

nbPGF' :: NegativeBinomial -> Double -> Double
nbPGF' nb z = case nb of
  Zero -> 0
  (NegBinom r p) -> (r * p / (1 - p)) * nbPGF (NegBinom (r+1) p) z

nbPGF'' :: NegativeBinomial -> Double -> Double
nbPGF'' nb z = case nb of
  Zero -> 0
  (NegBinom r p) -> (r * (r + 1) * (p / (1 - p)) ** 2.0) *
                      nbPGF (NegBinom (r+2) p) z

-- | The log of the PGF of the negative binomial distribution.
logNbPGF :: NegativeBinomial -> Double -> Double
logNbPGF nb z = case nb of
  Zero -> 0
  (NegBinom r p) -> r * (log (1 - p) - log (1 - p * z))

-- | The jth derivative of the negative binomial PGF.
--
-- __WARNING__ It is easy to get an Infinite value out of this.
--
nbPGFdash :: Double -> NegativeBinomial -> Double -> Double
nbPGFdash j nb z =
  case nb of
    Zero -> 0
    (NegBinom r p) ->
      pochhammer r j * (p / (1 - p)) ** j *
        nbPGF (NegBinom (r + j) p) z

-- | The log of the jth derivative of the negative binomial PGF.
logNbPGFdash :: Double -> NegativeBinomial -> Double -> Double
logNbPGFdash j nb z =
  case nb of
    Zero -> log 0
    (NegBinom r p) ->
      logPochhammer r j + j * log (p / (1 - p)) +
      log (nbPGF (NegBinom (r + j) p) z)

pochhammer :: (Eq p, Num p) => p -> p -> p
pochhammer _ 0 = 1
pochhammer a i = (a + i - 1) * pochhammer a (i - 1)

logPochhammer :: (Eq p, Floating p) => p -> p -> p
logPochhammer _ 0 = 0
logPochhammer a i = log (a + i - 1) + logPochhammer a (i - 1)
