module BDSCOD.Llhd where

import Data.List (find)
import Data.Maybe (fromJust)




type NumLineages = Double

data Event
  = Birth
  | Sample
  | Occurrence
  | Catastrophe NumLineages
  | Disaster NumLineages
  deriving (Show, Eq)

type Observation = (Time, Event)






type Rate = Double
type Probability = Double
type Time = Double

type Parameters
   = (Rate
     , Rate
     , Rate
     , [(Time, Probability)]
     , Rate
     , [(Time, Probability)])






data NegativeBinomial
  = Zero
  | NegBinom Double Probability
  deriving (Show)

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






nbPGF :: NegativeBinomial -> Double -> Double
nbPGF nb z = case nb of
  Zero -> 1
  (NegBinom r p) -> ((1 - p) / (1 - p * z)) ** r

nbPGF' nb z = case nb of
  Zero -> 0
  (NegBinom r p) -> (r * p / (1 - p)) * nbPGF (NegBinom (r+1) p) z

nbPGF'' nb z = case nb of
  Zero -> 0
  (NegBinom r p) -> (r * (r + 1) * (p / (1 - p)) ** 2.0) *
                      nbPGF (NegBinom (r+2) p) z






nbPGFdash j nb z =
  case nb of
    Zero -> 0
    (NegBinom r p) ->
      pochhammer r j * (p / (1 - p)) ** j *
        nbPGF (NegBinom (r + j) p) z

pochhammer _ 0 = 1
pochhammer a i = (a + i - 1) * pochhammer a (i - 1)






eventRate :: Parameters -> Double
eventRate (lam, mu, psi, _, om, _) = lam + mu + psi + om

discriminant :: Parameters -> Double
discriminant params@(lam, mu, _, _, _, _) =
  (eventRate params) ^ 2 - 4.0 * lam * mu

x1and2 :: Parameters -> (Double, Double)
x1and2 params@(lam, _, _, _, _, _) =
  let gam = eventRate params
      sqrtDisc = sqrt $ discriminant params
   in ((gam - sqrtDisc) / (2 * lam), (gam + sqrtDisc) / (2 * lam))

odeHelpers params delay = (x1,x2,disc,expFact)
  where
    (x1,x2) = x1and2 params
    disc = discriminant params 
    expFact = exp ((- sqrt disc) * delay)






p0' params delay z =
  (expFact * x2 - x1) / (x2 - expFact * (x1 - z) - z) -
  ((expFact - 1) * (x1 * (x2 - z) - expFact * x2 * (x1 - z))) /
  (x2 - expFact * (x1 - z) - z) ^ 2
  where
    (x1, x2, _, expFact) = odeHelpers params delay

p0'' params delay z =
  (2 * (expFact - 1) ^ 2 * (x1 * (x2 - z) -
  expFact * x2 * (x1 - z))) /
  (x2 - expFact * (x1 - z) - z) ^ 3 -
  (2 * (expFact - 1) * (expFact * x2 - x1))
  / (x2 - expFact * (x1 - z) - z) ^ 2
  where
    (x1, x2, _, expFact) = odeHelpers params delay

rr' params@(lam, _, _, _, _, _) delay z =
  (2 * (1 - expFact) * expFact * disc) /
  (lam ^ 2 * (x2 - expFact * (x1 - z) - z) ^ 3)
  where
    (x1, x2, disc, expFact) = odeHelpers params delay

rr'' params@(lam, _, _, _, _, _) delay z =
  (6 * (expFact - 1) ^ 2 * expFact * disc) /
  (lam ^ 2 * (x2 - expFact * (x1 - z) - z) ^ 4)
  where
    (x1, x2, disc, expFact) = odeHelpers params delay






p0 params delay z =
  (x1 * (x2 - z) - x2 * (x1 - z) * expFact) /
  ((x2 - z) - (x1 - z) * expFact)
  where
    (x1, x2, _, expFact) = odeHelpers params delay

rr params@(lam, _, _, _, _, _) delay z =
  disc * expFact /
  ((lam ^ 2) * (((x2 - z) - (x1 - z) * expFact) ^ 2))
  where
    (x1, x2, disc, expFact) = odeHelpers params delay






data PDESolution = PDESol NegativeBinomial NumLineages






pdeGF :: Parameters -> Time -> PDESolution -> Double -> Double
pdeGF params delay (PDESol Zero 1) z = rz
  where
    rz = rr params delay z
pdeGF params delay (PDESol nb k) z = f ( p0z ) * (rz ** k)
  where
    f z = nbPGF nb z
    p0z = p0 params delay z
    rz = rr params delay z

pdeGF' :: Parameters -> Time -> PDESolution -> Double -> Double
pdeGF' params delay (PDESol Zero 1) z = rdashz
  where
    rdashz = rr' params delay z
pdeGF' params delay (PDESol nb k) z =
  (fdash ( p0z )) * p0dashz * rz ** k +
  k * rz ** ( k - 1 ) * rdashz * f ( p0z )
  where
    f z = nbPGF nb z
    fdash z = nbPGF' nb z
    p0z = p0 params delay z
    p0dashz = p0' params delay z
    rz = rr params delay z
    rdashz = rr' params delay z

pdeGF'' :: Parameters -> Time -> PDESolution -> Double -> Double
pdeGF'' params delay (PDESol Zero 1) z = rdashdashz
  where
    rdashdashz = rr'' params delay z
pdeGF'' params delay (PDESol nb k) z =
  fdashdash ( p0z ) * p0dashz ** 2 * rz ** k +
  fdash ( p0z ) * p0dashdashz * rz ** k +
  2 * (fdash ( p0z )) * p0dashz * k * rz ** (k-1) * rdashz +
  f ( p0z ) * k * (k-1) * rz ** (k-2) * rdashz ** 2 +
  f ( p0z ) * k * rz ** (k-1) * rdashdashz
  where
    f z = nbPGF nb z
    fdash z = nbPGF' nb z
    fdashdash z = nbPGF'' nb z
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
pdeStatistics params delay pdeSol@(PDESol nb k) =
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






intervalLlhd params delay k nb =
  let (c,m,v) = pdeStatistics params delay (PDESol nb k)
   in (log c,nbFromMAndV (m,v))






eventLlhd _ (lam, _, _, _, _, _) Birth k nb = (log lam, k + 1, nb)
eventLlhd _ (_, _, psi, _, _, _) Sample k nb = (log psi, k - 1, nb)
eventLlhd _ (_, _, _, _, om, _) Occurrence k nb@(NegBinom r p) =
  (log om + log (nbPGF' nb 1), k, NegBinom (r + 1) p)
eventLlhd t (_, _, _, rhs, _, _) (Catastrophe n) k nb@(NegBinom r p) =
  let rh = snd . fromJust $ find ((== t) . fst) rhs
      logL = n * log rh + log (nbPGF nb (1 - rh))
   in (logL, k - n, NegBinom r ((1 - rh) * p))
eventLlhd t (_, _, _, _, _, nus) (Disaster n) k nb@(NegBinom r p) =
  let nu = snd . fromJust $ find ((== t) . fst) nus
      logL = n * log nu + log (nbPGFdash n nb (1 - nu))
   in (logL, k, NegBinom (r + n) ((1 - nu) * p))






type LogLikelihood = Double
type LlhdCalcState = (LogLikelihood
                     ,Time
                     ,NumLineages
                     ,NegativeBinomial)

initLlhdState :: LlhdCalcState
initLlhdState = (0,0,1,Zero)






llhdAndNB :: [Observation]
     -> Parameters
     -> LlhdCalcState
     -> (LogLikelihood,NegativeBinomial)






llhdAndNB [] _ (l,_,_,nb) = (l,nb)
llhdAndNB ((delay,event):events) params (l,t,k,nb) =
  llhdAndNB events params (l+l'+l'',t',k'',nb'')
    where
      t' = t + delay
      (l',nb') = intervalLlhd params delay k nb
      (l'',k'',nb'') = eventLlhd t' params event k nb'




