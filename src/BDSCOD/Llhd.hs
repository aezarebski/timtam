module BDSCOD.Llhd where

import Data.List (find)
import Data.Maybe (fromJust)
import BDSCOD.Types
import BDSCOD.Utility
import Epidemic.Types.Parameter


-- | Predicate for whether the parameters could possibly have given rise to the
-- observations.
arePlausible :: [Observation]
             -> Parameters
             -> Bool
arePlausible obs (Parameters (l, m, r, _, o, _))
  | minimum [l, m, r, o] < 0 = False
  | any isBirth obs && l == 0 = False
  | any isSample obs && r == 0 = False
  | otherwise = True


-- | The total event rate
--
-- In Zarebski /et al/ (2020) the net event rate is \(\gamma\).
--
eventRate :: Parameters -> Double
eventRate (Parameters (lam, mu, psi, _, om, _)) = lam + mu + psi + om

discriminant :: Parameters -> Double
discriminant params@(Parameters (lam, mu, _, _, _, _)) =
  eventRate params ** 2.0 - 4.0 * lam * mu

x1and2 :: Parameters -> (Double, Double)
x1and2 params@(Parameters (lam, _, _, _, _, _)) =
  let gam = eventRate params
      sqrtDisc = sqrt $ discriminant params
   in ((gam - sqrtDisc) / (2 * lam), (gam + sqrtDisc) / (2 * lam))

odeHelpers :: Parameters -> Time -> (Double,Double,Double,Double)
odeHelpers params delay = (x1,x2,disc,expFact)
  where
    (x1,x2) = x1and2 params
    disc = discriminant params
    expFact = exp ((- sqrt disc) * delay)





-- | The partial derivative of @p0@ with respect to its final argument @z@.
p0' :: Parameters -> Time -> Probability -> Double
p0' params delay z =
  (expFact * x2 - x1) / (x2 - expFact * (x1 - z) - z) -
  ((expFact - 1) * (x1 * (x2 - z) - expFact * x2 * (x1 - z))) /
  (x2 - expFact * (x1 - z) - z) ** 2.0
  where
    (x1, x2, _, expFact) = odeHelpers params delay

-- | The second partial derivative of @p0@ with respect to its final argument
-- @z@.
p0'' :: Parameters -> Time -> Probability -> Double
p0'' params delay z =
  (2 * (expFact - 1) ** 2.0 * (x1 * (x2 - z) -
  expFact * x2 * (x1 - z))) /
  (x2 - expFact * (x1 - z) - z) ** 3.0 -
  (2 * (expFact - 1) * (expFact * x2 - x1))
  / (x2 - expFact * (x1 - z) - z) ** 2.0
  where
    (x1, x2, _, expFact) = odeHelpers params delay

rr' :: Parameters -> Time -> Probability -> Double
rr' params@(Parameters (lam, _, _, _, _, _)) delay z =
  (2 * (1 - expFact) * expFact * disc) /
  (lam ** 2.0 * (x2 - expFact * (x1 - z) - z) ** 3.0)
  where
    (x1, x2, disc, expFact) = odeHelpers params delay

rr'' :: Parameters -> Time -> Probability -> Double
rr'' params@(Parameters (lam, _, _, _, _, _)) delay z =
  (6 * (expFact - 1) ** 2.0 * expFact * disc) /
  (lam ** 2.0 * (x2 - expFact * (x1 - z) - z) ** 4.0)
  where
    (x1, x2, disc, expFact) = odeHelpers params delay





-- | The /probability/ an individual does not give rise to a \(\psi\) or
-- \(\omega\) sampled observation during a period of time of length @duration@
-- nor are \(\rho\) sampled at the end of this period which happens with
-- probability @1-z@.
p0 :: Parameters -> Time -> Probability -> Probability
p0 params delay z =
  (x1 * (x2 - z) - x2 * (x1 - z) * expFact) /
  ((x2 - z) - (x1 - z) * expFact)
  where
    (x1, x2, _, expFact) = odeHelpers params delay

-- | The probability an individual has one extant lineage at present /given/
-- there is a \(\rho\) sampling at present and it does not get sampled at this
-- instant. In Zarebski /et al/ (2020) this is the function \(R(u,z)\).
rr :: Parameters -> Time -> Probability -> Probability
rr params@(Parameters (lam, _, _, _, _, _)) delay z =
  disc * expFact /
  ((lam ** 2.0) * (((x2 - z) - (x1 - z) * expFact) ** 2.0))
  where
    (x1, x2, disc, expFact) = odeHelpers params delay












pdeGF :: Parameters -> Time -> PDESolution -> Double -> Double
pdeGF params delay (PDESol Zero 1) z = rz
  where
    rz = rr params delay z
pdeGF params delay (PDESol nb k) z = f p0z * (rz ** k)
  where
    f = nbPGF nb
    p0z = p0 params delay z
    rz = rr params delay z

pdeGF' :: Parameters -> Time -> PDESolution -> Double -> Double
pdeGF' params delay (PDESol Zero 1) z = rdashz
  where
    rdashz = rr' params delay z
pdeGF' params delay (PDESol nb k) z =
  fdash p0z * p0dashz * rz ** k +
  k * rz ** ( k - 1 ) * rdashz * f p0z
  where
    f = nbPGF nb
    fdash = nbPGF' nb
    p0z = p0 params delay z
    p0dashz = p0' params delay z
    rz = rr params delay z
    rdashz = rr' params delay z

pdeGF'' :: Parameters -> Time -> PDESolution -> Double -> Double
pdeGF'' params delay (PDESol Zero 1) z = rdashdashz
  where
    rdashdashz = rr'' params delay z
pdeGF'' params delay (PDESol nb k) z =
  fdashdash p0z * p0dashz ** 2 * rz ** k +
  fdash p0z * p0dashdashz * rz ** k +
  2 * fdash p0z * p0dashz * k * rz ** (k-1) * rdashz +
  f p0z * k * (k-1) * rz ** (k-2) * rdashz ** 2 +
  f p0z * k * rz ** (k-1) * rdashdashz
  where
    f = nbPGF nb
    fdash = nbPGF' nb
    fdashdash = nbPGF'' nb
    p0z = p0 params delay z
    p0dashz = p0' params delay z
    p0dashdashz = p0'' params delay z
    rz = rr params delay z
    rdashz = rr' params delay z
    rdashdashz = rr'' params delay z






pdeStatistics :: Parameters
              -> Time
              -> PDESolution
              -> (Probability, Double, Double)
pdeStatistics params delay pdeSol@PDESol{} =
  if c > 1e-300
    then (c, m, v)
    else error $ "pdeStatistics had a c: " ++ show c
  where
    mGF = pdeGF params delay pdeSol
    mGF' = pdeGF' params delay pdeSol
    mGF'' = pdeGF'' params delay pdeSol
    c = mGF 1
    m = (1 / c) * mGF' 1
    v = (1 / c) * mGF'' 1 + m * (1 - m)






intervalLlhd :: Parameters
             -> Double
             -> Double
             -> NegativeBinomial
             -> (Probability, NegativeBinomial)
intervalLlhd params delay k nb =
  let (c, m, v) = pdeStatistics params delay (PDESol nb k)
   in if isInfinite (log c)
      then error "infinite log(c) in intervalLlhd function..."
      else (log c, nbFromMAndV (m, v))





eventLlhd :: Time -- ^ Absolute time used to look up the parameter in the case of a scheduled event
          -> Parameters
          -> ObservedEvent
          -> NumLineages -- ^ The number of lineages in the reconstructed tree prior to the event
          -> NegativeBinomial
          -> (LogLikelihood, NumLineages, NegativeBinomial)
eventLlhd _ (Parameters (lam, _, _, _, _, _)) OBirth k nb = (log lam, k + 1, nb)
eventLlhd _ (Parameters (_, _, psi, _, _, _)) OSample k nb = (log psi, k - 1, nb)
eventLlhd _ (Parameters (_, _, _, _, om, _)) OOccurrence k nb@(NegBinom r p) =
  (log om + logNbPGF' nb 1, k, NegBinom (r + 1) p)
eventLlhd t (Parameters (_, _, _, Timed rhs, _, _)) (OCatastrophe n) k nb@(NegBinom r p) =
  let rh = snd . fromJust $ find ((== t) . fst) rhs
      logL = n * log rh + logNbPGF nb (1 - rh) + (k - n) * log (1 - rh)
   in if isInfinite logL
      then error "numerical error: infinite logL in eventLlhd function while processing catastrophe"
      else (logL, k - n, NegBinom r ((1 - rh) * p))
eventLlhd t (Parameters (_, _, _, _, _, Timed nus)) (ODisaster n) k nb@(NegBinom r p) =
  let nu = snd . fromJust $ find ((== t) . fst) nus
      logL = n * log nu + logNbPGFdash n nb (1 - nu) + k * log (1 - nu)
   in if isInfinite logL
      then error "numerical error: infinite logL in eventLlhd function while processing disaster"
      else (logL, k, NegBinom (r + n) ((1 - nu) * p))
eventLlhd _ (Parameters (_, _, _, _, _, _)) OOccurrence k Zero = (log 0, k, Zero)
eventLlhd _ (Parameters (_, _, _, _, _, _)) (ODisaster _) k Zero = (log 0, k, Zero)
eventLlhd _ (Parameters (_, _, _, _, _, _)) (OCatastrophe _) _ Zero = undefined



-- | This is the state of the likelihood calculation: llhd, time, LTT, NB of
-- hidden lineages
initLlhdState :: LlhdCalcState
initLlhdState = ((0,Zero),0,1)

-- | The log-likelihood and the distribution of prevalence.
llhdAndNB :: [Observation]  -- ^ The observed events
          -> Parameters     -- ^ The parameters
          -> LlhdCalcState  -- ^ The initial state of the calculation: @initLlhdState@
          -> LlhdAndNB
llhdAndNB obs params state0 = fst $ verboseLlhdAndNB obs params state0

-- | The log-likelihood and the distribution of the prevalence along with their
-- partial values.
verboseLlhdAndNB :: [Observation]  -- ^ The observed events
                 -> Parameters     -- ^ The parameters
                 -> LlhdCalcState  -- ^ The initial state of the calculation: @initLlhdState@
                 -> (LlhdAndNB,[LlhdAndNB])
verboseLlhdAndNB obs params state0 =
  if arePlausible obs params
  then verboseLlhdAndNB' obs params state0 mempty
  else ((-1 / 0, Zero),mempty)

updatedLlhdCalcState :: Parameters
                     -> Observation
                     -> LlhdCalcState
                     -> LlhdCalcState
updatedLlhdCalcState params (delay,event) ((l,nb), t, k) =
  ((l+l'+l'',nb''),t',k'')
  where
    t' = t + delay
    (l',nb') = intervalLlhd params delay k nb
    (l'',k'',nb'') = eventLlhd t' params event k nb'

-- | Compute the log-likelihood and distribution of the prevalence along with
-- their partial values assuming plausible parameters.
verboseLlhdAndNB' :: [Observation]  -- ^ The observed events
                  -> Parameters     -- ^ The parameters
                  -> LlhdCalcState  -- ^ The initial state of the calculation: @initLlhdState@
                  -> [LlhdAndNB]    -- ^ The accumulator of the partial results
                  -> (LlhdAndNB,[LlhdAndNB])
verboseLlhdAndNB' [] _ (lnb,_,_) partialResult = (lnb,lnb:partialResult)
verboseLlhdAndNB' (o:obs) params calcState partialResult =
  let calcState'@(lnb,_,_) = updatedLlhdCalcState params o calcState
  in verboseLlhdAndNB' obs params calcState' (lnb:partialResult)

-- | Compute the log-likelihood and distribution of prevalence assuming
-- plausible parameters.
--
-- __WARNING__ This function is deprecated in favour of @llhdAndNB@.
--
llhdAndNB' :: [Observation]
           -> Parameters
           -> LlhdCalcState
           -> LlhdAndNB
llhdAndNB' [] _ (lnb,_,_) = lnb
llhdAndNB' ((delay,event):events) params ((l,nb),t,k) =
  llhdAndNB' events params (updatedLlhdCalcState params (delay,event) ((l,nb),t,k))
