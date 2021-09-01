{-# LANGUAGE MultiWayIf #-}
module BDSCOD.Llhd (llhdAndNB
                   ,intervalLlhd
                   ,eventLlhd
                   ,initLlhdState
                   ,logPdeStatistics
                   ,pdeStatistics
                   , logPdeGF
                   , logPdeGF'
                   , logPdeGF''
                   , odeHelpers
                   , p0
                   , logP0'
                   , p0'
                   , logP0''
                   , p0''
                   ,pdeGF
                   ,pdeGF'
                   ,pdeGF''
                   ,rr
                   ,rr'
                   ,rr''
                   ) where

import           BDSCOD.Types
import           BDSCOD.Utility
import           Data.List                (find, scanl')
import           Epidemic.Types.Parameter


-- | Predicate for whether the parameters could possibly have given rise to the
-- observations.
--
-- __WARNING__ we assume that all rates must be less than 100 to prevent
-- numerical errors.
--
arePlausible :: [Observation]
             -> Parameters
             -> Bool
arePlausible obs (Parameters (l, m, r, _, o, _))
  | minimum [l, m, r, o] < 0 = False
  | maximum [l, m, r, o] > 100 = False
  | any isBirth obs && l == 0 = False
  | any isUnscheduledSequenced obs && r == 0 = False
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

odeHelpers :: Parameters -> TimeDelta -> (Double,Double,Double,Double)
odeHelpers params (TimeDelta delay) = (x1,x2,disc,expFact)
  where
    (x1,x2) = x1and2 params
    disc = discriminant params
    expFact = exp ((- sqrt disc) * delay)





-- | The partial derivative of @p0@ with respect to its final argument @z@.
--
-- NOTE __do not__ use this function use @logP0'@ instead
p0' :: Parameters -> TimeDelta -> Probability -> Double
p0' params delay z =
  (expFact * x2 - x1) / (x2 - expFact * (x1 - z) - z) -
  ((expFact - 1) * (x1 * (x2 - z) - expFact * x2 * (x1 - z))) /
  (x2 - expFact * (x1 - z) - z) ** 2.0
  where
    (x1, x2, _, expFact) = odeHelpers params delay

-- | The log of the partial derivative of @p0@ with respect to @z@.
--
-- NOTE that this should avoid the numerical problems with @p0'@
logP0' :: Parameters -> TimeDelta -> Probability -> Double
logP0' params delay z =
  log (cc * aa + x1 * x2 * (bb ** 2.0)) - 2 * log (aa - bb * z)
  where
    (x1, x2, _, expFact) = odeHelpers params delay
    aa = x2 - x1 * expFact
    bb = 1 - expFact
    cc = x2 * expFact - x1

-- | The second partial derivative of @p0@ with respect to its final argument
-- @z@.
--
-- NOTE __do not__ use this function use @logP0''@ instead
p0'' :: Parameters -> TimeDelta -> Probability -> Double
p0'' params delay z =
  (2 * (expFact - 1) ** 2.0 * (x1 * (x2 - z) -
  expFact * x2 * (x1 - z))) /
  (x2 - expFact * (x1 - z) - z) ** 3.0 -
  (2 * (expFact - 1) * (expFact * x2 - x1))
  / (x2 - expFact * (x1 - z) - z) ** 2.0
  where
    (x1, x2, _, expFact) = odeHelpers params delay

-- | The second partial derivative of @p0@ with respect to its final argument
-- @z@.
--
-- NOTE that this should avoid the numerical problems with @p0''@
logP0'' :: Parameters -> TimeDelta -> Probability -> Double
logP0'' params delay z =
  log 2.0 + log bb + log (cc * aa + x1 * x2 * (bb ** 2.0)) - 3.0 * log (aa - bb * z)
  where
    (x1, x2, _, expFact) = odeHelpers params delay
    aa = x2 - x1 * expFact
    bb = 1 - expFact
    cc = x2 * expFact - x1

rr' :: Parameters -> TimeDelta -> Probability -> Double
rr' params@(Parameters (lam, _, _, _, _, _)) delay z =
  (2 * (1 - expFact) * expFact * disc) /
  (lam ** 2.0 * (x2 - expFact * (x1 - z) - z) ** 3.0)
  where
    (x1, x2, disc, expFact) = odeHelpers params delay

rr'' :: Parameters -> TimeDelta -> Probability -> Double
rr'' params@(Parameters (lam, _, _, _, _, _)) delay z =
  (6 * (expFact - 1) ** 2.0 * expFact * disc) /
  (lam ** 2.0 * (x2 - expFact * (x1 - z) - z) ** 4.0)
  where
    (x1, x2, disc, expFact) = odeHelpers params delay





-- | The /probability/ an individual does not give rise to a \(\psi\) or
-- \(\omega\) sampled observation during a period of time of length @duration@
-- nor are \(\rho\) sampled at the end of this period which happens with
-- probability @1-z@.
p0 :: Parameters -> TimeDelta -> Probability -> Probability
p0 params delay z =
  (x1 * (x2 - z) - x2 * (x1 - z) * expFact) /
  ((x2 - z) - (x1 - z) * expFact)
  where
    (x1, x2, _, expFact) = odeHelpers params delay

-- | The probability an individual has one extant lineage at present /given/
-- there is a \(\rho\) sampling at present and it does not get sampled at this
-- instant. In Zarebski /et al/ (2020) this is the function \(R(u,z)\).
rr :: Parameters -> TimeDelta -> Probability -> Probability
rr params@(Parameters (lam, _, _, _, _, _)) delay z =
  disc * expFact /
  ((lam ** 2.0) * (((x2 - z) - (x1 - z) * expFact) ** 2.0))
  where
    (x1, x2, disc, expFact) = odeHelpers params delay





-- | The generating function solution to the PDE.
--
-- __WARNING__ It is easy to get infinite values so try to use the @logPdeGF@
-- function instead if possible.
--
pdeGF :: Parameters -> TimeDelta -> PDESolution -> Double -> Double
pdeGF params delay (PDESol Zero 1) z = rz
  where
    rz = rr params delay z
pdeGF params delay (PDESol nb k) z =
  f p0z * (rz ** k)
  where
    f = nbPGF nb
    p0z = p0 params delay z
    rz = rr params delay z

-- | The log of the generating function solution to the PDE.
logPdeGF :: Parameters -> TimeDelta -> PDESolution -> Double -> Double
logPdeGF params delay (PDESol Zero 1) z = log rz
  where
    rz = rr params delay z
logPdeGF params delay (PDESol nb k) z =
  let
    logF = logNbPGF nb
    p0z = p0 params delay z
    rz = rr params delay z
  in if | k > 0     -> logF p0z + k * log rz
        | k == 0    -> logF p0z
        | otherwise -> error "negative k in logPdeGF"

-- | The partial derivative of the generating function solution to the PDE.
--
-- __WARNING__ It is easy to get infinite values so try to use the @logPdeGF'@
-- function instead if possible.
--
pdeGF' :: Parameters -> TimeDelta -> PDESolution -> Double -> Double
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

-- | The log of the partial derivative of the generating function solution to
-- the PDE.
logPdeGF' :: Parameters -> TimeDelta -> PDESolution -> Double -> Double
logPdeGF' params delay (PDESol Zero 1) z = log rdashz
  where
    rdashz = rr' params delay z
logPdeGF' params delay (PDESol nb k) z =
  if | k > 0 -> let p0z = p0 params delay z
                    logP0DashZ = logP0' params delay z
                    rz = rr params delay z
                    rdashz = rr' params delay z
                    firstTerm = logNbPGF' nb p0z +
                      logP0DashZ +
                      k * log rz
                    secondTerm = log k +
                      (k-1) * log rz +
                      log rdashz +
                      logNbPGF nb p0z
                in logSumExp [firstTerm,secondTerm]
     | k == 0 -> let p0z = p0 params delay z
                     logP0DashZ = logP0' params delay z
                     in logNbPGF' nb p0z + logP0DashZ
     | otherwise -> error "negative k in logPdeGF'"

-- | The second partial derivative of the generating function solution to the
-- PDE.
--
-- __WARNING__ It is easy to get infinite values so try to use the @logPdeGF''@
-- function instead if possible.
--
pdeGF'' :: Parameters -> TimeDelta -> PDESolution -> Double -> Double
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

-- | The log of the second partial derivative of the generating function solution to
-- the PDE.
logPdeGF'' :: Parameters -> TimeDelta -> PDESolution -> Double -> Double
logPdeGF'' params delay (PDESol Zero 1) z = log rdashdashz
  where
    rdashdashz = rr'' params delay z
logPdeGF'' params delay (PDESol nb k) z =
  if | k > 0 -> let -- f = nbPGF nb
                    -- fdash = nbPGF' nb
                    -- fdashdash = nbPGF'' nb
                    p0z = p0 params delay z
                    logP0DashZ = logP0' params delay z
                    logP0DashDashZ = logP0'' params delay z
                    rz = rr params delay z
                    rdashz = rr' params delay z
                    rdashdashz = rr'' params delay z
                    term1 = logNbPGF'' nb p0z + 2 * logP0DashZ + k * log rz
                    term2 = logNbPGF' nb p0z + logP0DashDashZ + k * log rz
                    term3 = log 2 + logNbPGF' nb p0z + logP0DashZ + log k + (k-1) * log rz + log rdashz
                    term4 = logNbPGF nb p0z + log k + log (k-1) + (k-2) * log rz + 2 * log rdashz
                    term5 = logNbPGF nb p0z + log k + (k-1) * log rz + log rdashdashz
                in logSumExp [term1, term2, term3, term4, term5]
     | k == 0 -> let p0z = p0 params delay z
                     logP0DashZ = logP0' params delay z
                     logP0DashDashZ = logP0'' params delay z
                     term1 = logNbPGF'' nb p0z + 2 * logP0DashZ
                     term2 = logNbPGF' nb p0z + logP0DashDashZ
                 in logSumExp [term1, term2]
     | otherwise -> error "negative k in logPdeGF''"


-- | The PDE statistics: the normalisation factor and the mean and variance of
-- the corresponding distribution.
--
-- __WARNING__ It is easy to get infinite values so try to use the
-- @logPdeStatistics@ function instead if possible.
--
pdeStatistics :: Parameters
              -> TimeDelta
              -> PDESolution
              -> (Probability, Double, Double)
pdeStatistics params delay pdeSol@PDESol{}
  | c > 0 && not (isNaN m) = (c, m, v)
  | c <= 0 = error "not the case that c > 0"
  | otherwise = error "not the case that m is not NaN"
  where mGF = pdeGF params delay pdeSol
        mGF' = pdeGF' params delay pdeSol
        mGF'' = pdeGF'' params delay pdeSol
        c = mGF 1
        m = (1 / c) * mGF' 1
        v = (1 / c) * mGF'' 1 + m * (1 - m)

-- | The logarithm of the PDE statistics
logPdeStatistics :: Parameters
              -> TimeDelta
              -> PDESolution
              -> (Probability, Double, Double)
logPdeStatistics params delay pdeSol@PDESol{} =
  (logC, logm, logV)
  where logmGF = logPdeGF params delay pdeSol
        logmGF' = logPdeGF' params delay pdeSol
        logmGF'' = logPdeGF'' params delay pdeSol
        logC = logmGF 1
        logm = logmGF' 1 - logC
        -- v = (1 / c) * mGF'' 1 + m * (1 - m)
        logV = log $ exp (logmGF'' 1 - logC) + exp logm * (1 - exp logm)






intervalLlhd :: Parameters
             -> TimeDelta
             -> NumLineages
             -> NegativeBinomial
             -> (LogLikelihood, NegativeBinomial)
intervalLlhd params delay k nb =
  let (logC, logm, logV) = logPdeStatistics params delay (PDESol nb k)
      nb' = nbFromMAndV (exp logm, exp logV)
    in if not (isNaN (exp logm) && isNaN (exp logV))
       then (logC, nb')
       else error $ "numerical error in intervalLlhd: " ++ show (params, delay, k, nb)





eventLlhd :: AbsoluteTime -- ^ Absolute time used to look up the parameter in the case of a scheduled event
          -> Parameters
          -> ObservedEvent
          -> NumLineages -- ^ The number of lineages in the reconstructed tree prior to the event
          -> NegativeBinomial
          -> (LogLikelihood, NumLineages, NegativeBinomial)
eventLlhd _ (Parameters (lam, _, _, _, _, _)) OBirth k nb = (log lam, k + 1, nb)
eventLlhd _ (Parameters (_, _, psi, _, _, _)) ObsUnscheduledSequenced k nb = (log psi, k - 1, nb)
eventLlhd _ (Parameters (_, _, _, _, om, _)) OOccurrence k nb@(NegBinomSizeProb r p) =
  (log om + logNbPGF' nb 1, k, NegBinomSizeProb (r + 1) p)
eventLlhd t (Parameters (_, _, _, Timed rhs, _, _)) (OCatastrophe n) k nb@(NegBinomSizeProb r p) =
  let maybeTRh = find ((timesWithinEpsilon t) . fst) rhs
   in case maybeTRh of
        (Just (_,rh)) -> let logL = n * log rh + logNbPGF nb (1 - rh) + (k - n) * log (1 - rh)
                             in if isInfinite logL
                                then error "numerical error: infinite logL in eventLlhd function while processing catastrophe"
                                else (logL, k - n, NegBinomSizeProb r ((1 - rh) * p))
        Nothing -> error ("could not find a scheduled sequenced observation at time: " ++ show t)
eventLlhd t (Parameters (_, _, _, _, _, Timed nus)) (ODisaster n) k nb@(NegBinomSizeProb r p) =
  let maybeTNu = find ((timesWithinEpsilon t) . fst) nus
   in case maybeTNu of
        (Just (_,nu)) -> let logL = n * log nu + logNbPGFdash n nb (1 - nu) + k * log (1 - nu)
                         in if isInfinite logL
                            then error "numerical error: infinite logL in eventLlhd function while processing disaster"
                            else (logL, k, NegBinomSizeProb (r + n) ((1 - nu) * p))
        Nothing -> error ("\nThe nus are:\n\n" ++
                          show nus ++
                          "\nThe t is:\n" ++
                          show t ++
                          "\n\ncould not find a scheduled unsequenced observation at time: " ++
                          show t)
eventLlhd _ (Parameters (_, _, _, _, _, _)) OOccurrence k Zero = (log 0, k, Zero)
eventLlhd _ (Parameters (_, _, _, _, _, _)) (ODisaster _) k Zero = (log 0, k, Zero)
eventLlhd _ (Parameters (_, _, _, _, _, _)) (OCatastrophe _) _ Zero = undefined



-- | This is the state of the likelihood calculation: llhd and NB of hidden
-- lineages, absolute time, and the LTT of the reconstructed tree
initLlhdState :: LlhdCalcState
initLlhdState = ((0, Zero), AbsoluteTime 0, 1)

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
  then let fstTrpl (a,_,_) = a
           go cs o = updatedLlhdCalcState params o cs
           results = fstTrpl <$> scanl' go state0 obs
       in (last results, results)
  else ((-1 / 0, Zero),[])

updatedLlhdCalcState :: Parameters
                     -> Observation
                     -> LlhdCalcState
                     -> LlhdCalcState
updatedLlhdCalcState params (delay,event) ((l,nb), t, k) =
  if k >= 0
  then ((l+l'+l'',nb''),t',k'')
  else error $ "bad k in updatedLlhdCalcState: k = " ++ show k
  where
    t' = timeAfterDelta t delay
    (l',nb') = intervalLlhd params delay k nb
    (l'',k'',nb'') = eventLlhd t' params event k nb'
