{-# LANGUAGE DeriveGeneric #-}
module BDSCOD.InhomogeneousBDSLlhd where

import BDSCOD.Llhd hiding (initLlhdState)
import BDSCOD.Types
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import Epidemic.Types.Parameter
import qualified Data.Aeson as Json
import GHC.Generics
import Data.Either.Combinators (rightToMaybe)



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
inhomLlhdAndNB' [] _ (l,_,_,nb) = pure (l,nb)
inhomLlhdAndNB' ((delay,event):events) inhomParams@(InhomParams (tlams,mu,psi)) (l,t,k,nb) =
  do
    rateChangeTime <- nextTime tlams t
    if timeDelta t rateChangeTime > delay
      then let bdscodParams = Parameters (fromJust $ cadlagValue tlams t, mu, psi, Timed [], 0.0, Timed [])
               t' = timeAfterDelta t delay
             in do (l',nb') <- rightToMaybe $ intervalLlhd bdscodParams delay k nb
                   let (l'',k'',nb'') = eventLlhd t' bdscodParams event k nb'
                   inhomLlhdAndNB' events inhomParams (l+l'+l'',t',k'',nb'')
      else let bdscodParams = Parameters (fromJust $ cadlagValue tlams t, mu, psi, Timed [], 0.0, Timed [])
               t' = rateChangeTime
               timeDeltaSubtraction (TimeDelta a) (TimeDelta b) = TimeDelta (a - b)
               remainingTime = timeDelta t rateChangeTime
            in do (l',nb') <- rightToMaybe $ intervalLlhd bdscodParams (timeDelta t rateChangeTime) k nb
                  inhomLlhdAndNB' ((timeDeltaSubtraction delay remainingTime, event):events) inhomParams (l+l',t',k,nb')

-- | The log-likelihood and the distribution of prevalence of the inhomogenoues BDS.
inhomLlhdAndNB :: [Observation]  -- ^ The observed events
          -> InhomParams    -- ^ The parameters
          -> InhomLlhdCalcState  -- ^ The initial state of the calculation: @initLlhdState@
          -> (LogLikelihood,NegativeBinomial)
inhomLlhdAndNB obs params state0 =
  fromMaybe (-1 / 0, Zero) $ inhomLlhdAndNB' obs params state0

type InhomLlhdCalcState = (LogLikelihood,AbsoluteTime,NumLineages,NegativeBinomial)
initLlhdState :: InhomLlhdCalcState
initLlhdState = (0,AbsoluteTime 0,1,Zero)
