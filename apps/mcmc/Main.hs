{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module provides an application for running an MCMC sampler. The input
-- to this program must match the 'MCMCInput' type.
--

module Main where

import           BDSCOD.Llhd              (initLlhdState, llhdAndNB)
import           BDSCOD.Types             (LogLikelihood (..),
                                           MCMCConfiguration (..), MWCSeed,
                                           NegativeBinomial (..), NumLineages,
                                           Observation (..), ObservedEvent (..),
                                           PDESolution (..), Parameters (..),
                                           nbMV2SP, packParameters, putLambda,
                                           putMu, putNus, putOmega, putPsi,
                                           putRhos, scheduledTimes,
                                           unpackParameters)
import           BDSCOD.Utility           (toMaybe)
import qualified Data.Aeson               as Json
import           Data.Either.Combinators  (fromRight, maybeToRight)
import           Data.List                (intersperse)
import           Data.Maybe               (fromJust, isNothing)
import qualified Data.Vector.Unboxed      as Unboxed
import           Epidemic.Types.Parameter (AbsoluteTime (..), TimeDelta (..))
import           GHC.Generics             (Generic)
import           GHC.Word                 (Word32 (..))
import           Numeric.MCMC.Metropolis  (Chain (..), chain')
import           System.Environment       (getArgs)
import           System.Random.MWC        (initialize)
import           Text.Printf              (printf)


data MCMCInput = MI { mcmcObservations          :: [Observation]
                    , mcmcInit                  :: [Double]
                    , mcmcSeed                  :: [Word32]
                    , mcmcNumSamples            :: Int
                    , mcmcStepSD                :: Double
                    , mcmcSampleCSV             :: FilePath
                    , mcmcRecordFinalPrevalence :: Bool
                    , mcmcParameterisation      :: String
                    , mcmcKnownMu               :: Maybe Double
                    , mcmcSimDuration           :: Maybe Double
                    , mcmcRhoTimes              :: [Double]
                    , mcmcNuTimes               :: [Double]
                    , mcmcPrior                 :: String } deriving (Show, Generic)

instance Json.FromJSON MCMCInput


main :: IO ()
main = do
  putStrLn "\nrunning mcmc application"
  configFile <- head <$> getArgs
  eitherMI <- readConfigAndValidate configFile
  case eitherMI of
    Right MI {..} ->
         -- construct the target density and a summary function if necessary.
         let asParam = paramConstructor mcmcParameterisation mcmcRhoTimes mcmcNuTimes mcmcKnownMu mcmcSimDuration
             llhd x = fromRight (-1e6) $ fst <$> do params <- maybeToRight mempty $ asParam x
                                                    llhdAndNB mcmcObservations params initLlhdState
             -- TODO Implement the prior functionality
             prior :: [Double] -> Double
             prior = undefined
             target x = llhd x
             tunable x =
               fromRight undefined $ snd <$>
               do params <- maybeToRight mempty $ asParam x
                  llhdAndNB mcmcObservations params initLlhdState
             maybeTunable :: Maybe ([Double] -> NegativeBinomial)
             maybeTunable = toMaybe mcmcRecordFinalPrevalence tunable
         in do -- report some of the details of the mcmc so you can tell if the
               -- configuration has been read correctly
           putStrLn $
             replicate 60 '-' ++
             "\n\tconfiguration file:        " ++ configFile ++
             "\n\tprior:                     WARNING NOT IMPLEMENTED" ++
             "\n\tnumber of rho samples:     WARNING NOT IMPLEMENTED" ++
             "\n\tnumber of nu samples:      WARNING NOT IMPLEMENTED" ++
             "\n\tparameterisation name:     " ++ mcmcParameterisation ++
             "\n\toutput file:               " ++ mcmcSampleCSV ++
             "\n\tnumber observations:       " ++ show (length mcmcObservations) ++
             "\n\tnumber MCMC iterations:    " ++ show mcmcNumSamples ++
             "\n\trecord final prevalence:   " ++ show mcmcRecordFinalPrevalence ++
             "\n" ++ replicate 60 '-'
             -- run the sampler and write the results to file.
           gen <- initialize $ Unboxed.fromList mcmcSeed
           ch <- chain' mcmcNumSamples mcmcStepSD mcmcInit target maybeTunable gen
           writeChainToCSV ch mcmcSampleCSV
    Left msg ->
      do putStrLn $ "failed to read valid MCMC configuration from " ++ configFile
         putStrLn msg

-- | The configuration for the MCMC or an error message explaining why it is
-- invalid if this cannot be done.
readConfigAndValidate :: FilePath -> IO (Either String MCMCInput)
readConfigAndValidate configFile =
  do
    eMI <- Json.eitherDecodeFileStrict configFile
    return $ eMI >>= eValidMI
    where
      eValidMI mi@MI {..} =
        let
          timeDeltas = fst <$> mcmcObservations

          deltasSum = foldl (\a (TimeDelta b) -> a + b) 0 timeDeltas
          deltasSumToDuration =
            case mcmcSimDuration of
              Nothing  -> True
              Just dur -> abs (dur - deltasSum) < 1e-6

          -- Construct an association list so we can provide a clear error
          -- message if one of the checks fails.
          namedTests =
            [ ( "At least one observation", not $ null mcmcObservations)
            , ( "Positive number of posterior samples", mcmcNumSamples > 0)
            , ( "Step standard deviation is positive", mcmcStepSD > 0)
            , ( "Time deltas are all positive"
              , minimum timeDeltas > TimeDelta 0.0)
            , ( "Time deltas sum to duration" <>
                printf "\n\tduration: %f\n\tdeltas sum: %f" (fromJust mcmcSimDuration) deltasSum
              , deltasSumToDuration)]
        in if and $ snd <$> namedTests
           then Right mi
           else Left $
                "Validation failed on test: " <>
                (fst . head $ dropWhile snd namedTests)

-- | A function for constructing parameters that can be used by the likelihood.
paramConstructor :: String
                 -> [Double]
                 -> [Double]
                 -> Maybe Double
                 -> Maybe Double
                 -> ([Double] -> Maybe Parameters)
paramConstructor p rhoTimes nuTimes maybeMu maybeDuration
  | p == "identity-mu1-lambda-psi-noRho-omega-noNu" && null rhoTimes && null nuTimes =
    \[λ, ψ, ω] -> Just $ packParameters (λ, 0.026, ψ, [], ω, [])
  | p == "identity-muKnown-lambda-psi-noRho-omega-noNu" && null rhoTimes && null nuTimes =
    let μ = fromJust maybeMu
    in \[λ, ψ, ω] -> Just $ packParameters (λ, μ, ψ, [], ω, [])
  | p == "identity-muKnown-lambda-psi-rhoAtDuration-omega-noNu" && null nuTimes =
    let μ = fromJust maybeMu
        dur = fromJust maybeDuration
    in \[λ, ψ, ρ, ω] -> if minimum [λ, ψ, ρ, ω] > 0.0
                        then Just $ packParameters (λ, μ, ψ, [(AbsoluteTime dur, ρ)], ω, [])
                        else Nothing
  | p == "identity-muKnown-lambda-psiZero-rho-omegaZero-nu" && not (null nuTimes) && not (null rhoTimes) =
    let μ = fromJust maybeMu
    in \[λ, ρ, ν] -> if λ > 0.0 && 0.0 <= ρ && ρ <= 1.0 && 0.0 <= ν && ν <= 1.0
                        then Just $ packParameters ( λ
                                                   , μ
                                                   , 0
                                                   , [(AbsoluteTime rt, ρ) | rt <- rhoTimes]
                                                   , 0
                                                   , [(AbsoluteTime nt, ν) | nt <- nuTimes])
                        else Nothing
  | otherwise =
    error $ "mcmc does not recognise parameterisation of the model: " ++ p ++
    "\nonly some parameterisations have been implemented and this is not one of them" ++
    "\ncheck the paramConstructor function for valid parameterisations."

-- | Write the samples to file.
writeChainToCSV :: [Chain [Double] NegativeBinomial] -> FilePath -> IO ()
writeChainToCSV samples csv = Prelude.writeFile csv $ samples2CSV samples ++ "\n"
  where
    samples2CSV ss = mconcat . intersperse "\n" $ sample2Row <$> ss
    sample2Row s = mconcat . intersperse "," $ show <$> sample2Doubles s
    sample2Doubles s =
      chainScore s : chainPosition s ++ nbAsFields (chainTunables s)
    nbAsFields Nothing = []
    nbAsFields (Just nb) =
      case nbMV2SP nb of
        Right Zero                   -> [1.0,0.0]
        Right (NegBinomSizeProb r p) -> [r, p]
        _                            -> [0.0 / 0.0, 0.0 / 0.0]
