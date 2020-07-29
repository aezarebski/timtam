{-# LANGUAGE DeriveGeneric #-}
module BDSCOD.InhomogeneousBDSLlhd where

import BDSCOD.Llhd hiding (initLlhdState, llhdAndNB')
import BDSCOD.Types
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Epidemic.Types.Parameter
import qualified Data.Aeson as Json
import GHC.Generics



-- | The parameters of the inhomogeneous BDS are the inhomogeneous birth rate,
-- the natural removal rate and the sampling rate.
newtype InhomParams
   = InhomParams (Timed Rate, Rate, Rate) deriving (Generic,Show)


instance Json.FromJSON InhomParams

-- | Compute the log-likelihood and distribution of prevalence under the
-- inhomogenoues birth-death-sampling model assuming plausible parameters.
--
-- This unpacks the constant rate portions of the process and uses the functions
-- in @BDSCOD.Llhd@ to compute the corresponding interval and event likelihoods.
inhomLlhdAndNB' :: [Observation]
           -> InhomParams
           -> InhomLlhdCalcState
           -> Maybe (LogLikelihood,NegativeBinomial)
inhomLlhdAndNB' [] _ (l,_,_,nb) = Just (l,nb)
inhomLlhdAndNB' ((delay,event):events) inhomParams@(InhomParams (tlams,mu,psi)) (l,t,k,nb) =
  do
    rateChangeTime <- nextTime tlams t
    if rateChangeTime - t > delay
      then let bdscodParams = (fromJust $ cadlagValue tlams t, mu, psi, Timed [], 0.0, Timed [])
               t' = t + delay
               (l',nb') = intervalLlhd bdscodParams delay k nb
               (l'',k'',nb'') = eventLlhd t' bdscodParams event k nb'
             in inhomLlhdAndNB' events inhomParams (l+l'+l'',t',k'',nb'')
      else let bdscodParams = (fromJust $ cadlagValue tlams t, mu, psi, Timed [], 0.0, Timed [])
               t' = rateChangeTime
               (l',nb') = intervalLlhd bdscodParams (rateChangeTime-t) k nb
             in inhomLlhdAndNB' ((delay-(rateChangeTime-t),event):events) inhomParams (l+l',t',k,nb')

-- | The log-likelihood and the distribution of prevalence of the inhomogenoues BDS.
inhomLlhdAndNB :: [Observation]  -- ^ The observed events
          -> InhomParams    -- ^ The parameters
          -> InhomLlhdCalcState  -- ^ The initial state of the calculation: @initLlhdState@
          -> (LogLikelihood,NegativeBinomial)
inhomLlhdAndNB obs params state0 =
  fromMaybe (-1 / 0, Zero) $ inhomLlhdAndNB' obs params state0

type InhomLlhdCalcState = (LogLikelihood,Time,NumLineages,NegativeBinomial)
initLlhdState :: InhomLlhdCalcState
initLlhdState = (0,0,1,Zero)
