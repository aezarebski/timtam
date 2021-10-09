{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

-- |
-- Module    : Statistics.Distribution.Normal
-- Copyright : (c) 2021 Alexander E. Zarebski
-- License   : MIT
--
-- Maintainer  : aezarebski@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- The lognormal distribution.

module Statistics.Distribution.Lognormal
   (
     LognormalDistribution
   -- * constructors
   , lognormalDistr
   , lognormalDistrE
   ) where

import           Data.Data                       (Data, Typeable)
import           GHC.Generics                    (Generic)
import           Numeric.MathFunctions.Constants (m_sqrt_2, m_sqrt_2_pi)
import           Numeric.SpecFunctions           (erf, invErf)

import qualified Statistics.Distribution         as D


-- | The lognormal distribution.
data LognormalDistribution = LND {
      meanlog :: {-# UNPACK #-} !Double
    , sdlog   :: {-# UNPACK #-} !Double
    } deriving (Eq, Typeable, Data, Generic)

instance D.Distribution LognormalDistribution where
    cumulative      = cumulative

instance D.ContDistr LognormalDistribution where
    logDensity    = logDensity
    quantile      = quantile

instance D.MaybeMean LognormalDistribution where
    maybeMean = Just . D.mean

instance D.Mean LognormalDistribution where
    mean LND {..} = exp (meanlog + 0.5 * sdlog ** 2.0)

instance D.MaybeVariance LognormalDistribution where
    maybeStdDev   = Just . D.stdDev
    maybeVariance = Just . D.variance

instance D.Variance LognormalDistribution where
    variance LND {..} = exp (2.0 * meanlog + sdlog ** 2.0) * (exp (sdlog ** 2.0) - 1)

-- | Create lognormal distribution from parameters.
lognormalDistr :: Double            -- ^ Mean of the logarithm
            -> Double            -- ^ Standard deviation of the logarithm
            -> LognormalDistribution
lognormalDistr m sd = maybe (error $ errMsg m sd) id $ lognormalDistrE m sd

-- | Create normal distribution from parameters.
lognormalDistrE :: Double            -- ^ Mean of logarithm
             -> Double            -- ^ Standard deviation of logarithm
             -> Maybe LognormalDistribution
lognormalDistrE mln sdln
  | sdln > 0    = Just LND { meanlog       = mln
                           , sdlog = sdln
                           }
  | otherwise = Nothing

errMsg :: Double -> Double -> String
errMsg _ sd = "Statistics.Distribution.Lognormal.lognormalDistr: standard deviation must be positive. Got " ++ show sd

logDensity :: LognormalDistribution -> Double -> Double
logDensity LND {..} x = - (log x - meanlog) ** 2.0 / (2 * sdlog ** 2.0) - log (x * sdlog * m_sqrt_2_pi)

cumulative :: LognormalDistribution -> Double -> Double
cumulative LND {..} x = 0.5 * (1 + erf ((log x - meanlog) / (sdlog * m_sqrt_2)))

quantile :: LognormalDistribution -> Double -> Double
quantile LND {..} p
  | p == 0 = 0
  | p == 1 = inf
  | p == 0.5 = exp meanlog
  | p > 0 && p < 1 = exp (meanlog + sdlog * m_sqrt_2 * invErf (2 * p - 1))
  | otherwise = error $ "Statistics.Distribution.Lognormal.quantile: p must be in [0,1] range. Got: "++show p
  where inf = 1/0
