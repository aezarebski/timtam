{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Either.Combinators (fromRight)
import           BDSCOD.Types            (Observation)
import qualified Data.Aeson              as Json
import           Data.List               (intersperse)
import qualified Data.Vector.Unboxed     as Unboxed
import           GHC.Generics            (Generic)
import           GHC.Word                (Word32 (..))
import           Numeric.MCMC.Metropolis (Chain (..), chain')
import           System.Environment      (getArgs)
import           System.Random.MWC       (initialize)
import           BDSCOD.Llhd                (initLlhdState, llhdAndNB)
import           BDSCOD.Types               (LogLikelihood (..),
                                             MCMCConfiguration (..), MWCSeed,
                                             NegativeBinomial (..), NumLineages,
                                             Observation (..),
                                             ObservedEvent (..),
                                             PDESolution (..), Parameters (..),
                                             packParameters, putLambda, putMu,
                                             putNus, putOmega, putPsi, putRhos,
                                             scheduledTimes, unpackParameters)





data MCMCInput = MI { mcmcObservations :: [Observation]
                    , mcmcNumSamples   :: Int
                    , mcmcSampleCSV    :: FilePath
                    , mcmcStepSD       :: Double
                    , mcmcInit         :: [Double]
                    , mcmcSeed         :: [Word32]
                    , mcmcParameterisation :: String
                    , mcmcPrior        :: String } deriving (Show, Generic)

instance Json.FromJSON MCMCInput

main :: IO ()
main = do
  configFile <- head <$> getArgs
  eitherMI <- Json.eitherDecodeFileStrict configFile :: IO (Either String MCMCInput)
  case eitherMI of
    Right MI {..} ->
      do putStrLn $ "read MCMC configuration from " ++ configFile

         -- report some of the details of the mcmc so you can tell if the
         -- configuration has been read correctly
         putStrLn $
           "\n\tprior:                  WARNING NOT IMPLEMENTED" ++
           "\n\tparameterisation name: " ++ mcmcParameterisation ++
           "\n\toutput file:           " ++ mcmcSampleCSV ++
           "\n\tnumber observations:   " ++ show (length mcmcObservations) ++
           "\n\tnumber samples:        " ++ show mcmcNumSamples

         -- construct the target density and a summary function if necessary.
         let asParam = paramConstructor mcmcParameterisation
             llhd x = fromRight (-1e6) $ fst <$> llhdAndNB mcmcObservations (asParam x) initLlhdState
             -- TODO Implement the prior functionality
             prior :: [Double] -> Double
             prior = undefined
             target x = llhd x
             tunable = Nothing

         -- run the sampler and write the results to file.
         gen <- initialize $ Unboxed.fromList mcmcSeed
         ch <- chain' mcmcNumSamples mcmcStepSD mcmcInit target tunable gen
         writeChainToCSV ch mcmcSampleCSV
    Left msg ->
      do putStrLn $ "failed to read MCMC configuration from " ++ configFile
         putStrLn msg

-- | A function for constructing parameters that can be used by the likelihood.
paramConstructor :: String -> ([Double] -> Parameters)
paramConstructor p
  | p == "identity-mu1-lambda-psi-noRho-omega-noNu" =
    \[λ, ψ, ω] -> packParameters (λ, 1.0, ψ, [], ω, [])
  | otherwise = error $ "do not recognise parameterisation: " ++ p

-- | Write the samples to file.
writeChainToCSV :: [Chain [Double] b] -> FilePath -> IO ()
writeChainToCSV samples csv = writeFile csv $ samples2CSV samples ++ "\n"
  where samples2CSV = mconcat . intersperse "\n" . map sample2Row
        sample2Row = mconcat . intersperse "," . map show . chainPosition
