module BDSCOD.Utility where

import qualified Data.Vector as V

import qualified Data.Csv as Csv
import Epidemic.Types.Parameter
import Epidemic.Types.Events
import Epidemic.Types.Population
import BDSCOD.Types
import Data.List (scanl')
-- import BDSCOD.Llhd --


instance Csv.ToField TimeDelta where
  toField (TimeDelta td) = Csv.toField td

-- | Check if two absolute times differ by such a small amount that they are
-- likely identical.
timesWithinEpsilon :: AbsoluteTime -> AbsoluteTime -> Bool
timesWithinEpsilon (AbsoluteTime a) (AbsoluteTime b) = abs (a - b) < 1e-6

-- | Convert simulation events to observation events, this assumes that the
-- epidemic events have already been filtered by to only include the observable
-- events. Since this is model specific functions such as `observedEvents` are
-- provided to do this.
eventsAsObservations :: [EpidemicEvent] -> [Observation]
eventsAsObservations epiSimEvents =
  drop 1 . map fst $ scanl processEvent' ((TimeDelta 0, OBirth), AbsoluteTime 0) epiSimEvents

processEvent' :: (Observation, AbsoluteTime) -> EpidemicEvent -> (Observation, AbsoluteTime)
processEvent' (_, currTime) epiSimEvent =
  case epiSimEvent of
    (Infection absTime _ _) ->
      ((timeDelta currTime absTime, OBirth), absTime)
    (Sampling absTime _) -> ((timeDelta currTime absTime, ObsUnscheduledSequenced), absTime)
    (Catastrophe absTime (People persons)) -> ((timeDelta currTime absTime, OCatastrophe . fromIntegral $ V.length persons), absTime)
    (Occurrence absTime _) ->
      ((timeDelta currTime absTime, OOccurrence), absTime)
    (Disaster absTime (People persons)) -> ((timeDelta currTime absTime, ODisaster . fromIntegral $ V.length persons), absTime)
    (Removal _ _) -> error "A removal event has been passed to processEvent', this should never happen!"

-- | When possible return a negative binomial distribution with the specified
-- mean and variance. If the result is likely to lead to under/overflow then
-- this returns an error message as a string.
nbFromMAndV :: (Double, Double) -> Either String NegativeBinomial
nbFromMAndV (0, 0) = Right Zero
nbFromMAndV (m, v) =
  let
    r = (m ** 2) / (v - m)
    p = (v - m) / v
    postCond = m > 0 && v >= m && p > 1e-14 && p < 1 - 1e-14
  in if postCond
     then Right $ NegBinomSizeProb r p
     else Left $ "nbFromMAndV received bad input: (m,v) = " ++ show (m,v)

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

-- | Monadic (left) scan over the elements of a list.
scanlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanlM f z0 xs = sequence $ scanl' g (return z0) xs
  where
    g my x = my >>= \y -> f y x

