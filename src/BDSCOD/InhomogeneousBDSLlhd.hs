module BDSCOD.InhomogeneousBDSLlhd where

import Data.Maybe (fromJust)
import BDSCOD.Types
import BDSCOD.Llhd hiding (llhdAndNB')
import Epidemic.Types



-- | The parameters of the inhomogeneous BDS are the inhomogeneous birth rate,
-- the natural removal rate and the sampling rate.
type InhomParams
   = (Timed Rate, Rate, Rate)

-- | Compute the log-likelihood and distribution of prevalence under the
-- inhomogenoues birth-death-sampling model assuming plausible parameters.
--
-- This unpacks the constant rate portions of the process and uses the functions
-- in @BDSCOD.Llhd@ to compute the corresponding interval and event likelihoods.
llhdAndNB' :: [Observation]
           -> InhomParams
           -> LlhdCalcState
           -> Maybe (LogLikelihood,NegativeBinomial)
llhdAndNB' [] _ (l,_,_,nb) = Just (l,nb)
llhdAndNB' ((delay,event):events) inhomParams@(tlams,mu,psi) (l,t,k,nb) =
  do
    rateChangeTime <- nextTime tlams t
    if rateChangeTime - t > delay
      then let bdscodParams = (fromJust $ cadlagValue tlams t, mu, psi, [], 0.0, [])
               t' = t + delay
               (l',nb') = intervalLlhd bdscodParams delay k nb
               (l'',k'',nb'') = eventLlhd t' bdscodParams event k nb'
             in llhdAndNB' events inhomParams (l+l'+l'',t',k'',nb'')
      else let bdscodParams = (fromJust $ cadlagValue tlams t, mu, psi, [], 0.0, [])
               t' = rateChangeTime
               (l',nb') = intervalLlhd bdscodParams (rateChangeTime-t) k nb
             in llhdAndNB' ((delay-(rateChangeTime-t),event):events) inhomParams (l+l',t',k,nb')
