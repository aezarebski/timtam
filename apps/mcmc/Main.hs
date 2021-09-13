{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           BDSCOD.Llhd              (initLlhdState, llhdAndNB)
import           BDSCOD.Types             (Observation)
import           BDSCOD.Types             (LogLikelihood (..),
                                           MCMCConfiguration (..), MWCSeed,
                                           NegativeBinomial (..), NumLineages,
                                           Observation (..), ObservedEvent (..),
                                           PDESolution (..), Parameters (..),
                                           packParameters, putLambda, putMu,
                                           putNus, putOmega, putPsi, putRhos,
                                           scheduledTimes, unpackParameters)
import qualified Data.Aeson               as Json
import           Data.Either.Combinators  (fromRight, maybeToRight)
import           Data.List                (intersperse)
import           Data.Maybe               (fromJust)
import qualified Data.Vector.Unboxed      as Unboxed
import           Epidemic.Types.Parameter (AbsoluteTime (..), TimeDelta (..))
import           GHC.Generics             (Generic)
import           GHC.Word                 (Word32 (..))
import           Numeric.MCMC.Metropolis  (Chain (..), chain')
import           System.Environment       (getArgs)
import           System.Random.MWC        (initialize)
import           Text.Printf              (printf)


data MCMCInput = MI { mcmcObservations     :: [Observation]
                    , mcmcNumSamples       :: Int
                    , mcmcSampleCSV        :: FilePath
                    , mcmcStepSD           :: Double
                    , mcmcInit             :: [Double]
                    , mcmcSeed             :: [Word32]
                    , mcmcParameterisation :: String
                    , mcmcKnownMu          :: Maybe Double
                    , mcmcSimDuration      :: Maybe Double
                    , mcmcPrior            :: String } deriving (Show, Generic)

instance Json.FromJSON MCMCInput


main :: IO ()
main = do
  putStrLn "\nrunning mcmc application"
  configFile <- head <$> getArgs
  eitherMI <- readConfigAndValidate configFile
  case eitherMI of
    Right MI {..} ->
      do -- report some of the details of the mcmc so you can tell if the
         -- configuration has been read correctly
         putStrLn $
           replicate 60 '-' ++
           "\n\tconfiguration file:    " ++ configFile ++
           "\n\tprior:                  WARNING NOT IMPLEMENTED" ++
           "\n\tparameterisation name: " ++ mcmcParameterisation ++
           "\n\toutput file:           " ++ mcmcSampleCSV ++
           "\n\tnumber observations:   " ++ show (length mcmcObservations) ++
           "\n\tnumber samples:        " ++ show mcmcNumSamples ++
           "\n" ++ replicate 60 '-'

         -- construct the target density and a summary function if necessary.
         let asParam = paramConstructor mcmcParameterisation mcmcKnownMu mcmcSimDuration
             llhd x = fromRight (-1e6) $ fst <$> do params <- maybeToRight mempty $ asParam x
                                                    llhdAndNB mcmcObservations params initLlhdState
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
paramConstructor :: String -> Maybe Double -> Maybe Double -> ([Double] -> Maybe Parameters)
paramConstructor p maybeMu maybeDuration
  | p == "identity-mu1-lambda-psi-noRho-omega-noNu" =
    \[λ, ψ, ω] -> Just $ packParameters (λ, 0.026, ψ, [], ω, [])
  | p == "identity-muKnown-lambda-psi-noRho-omega-noNu" =
    let μ = fromJust maybeMu
    in \[λ, ψ, ω] -> Just $ packParameters (λ, μ, ψ, [], ω, [])
  | p == "identity-muKnown-lambda-psi-rhoAtDuration-omega-noNu" =
    let μ = fromJust maybeMu
        dur = fromJust maybeDuration
    in \[λ, ψ, ρ, ω] -> if minimum [λ, ψ, ρ, ω] > 0.0
                        then Just $ packParameters (λ, μ, ψ, [(AbsoluteTime dur, ρ)], ω, [])
                        else Nothing
  | otherwise = error $ "do not recognise parameterisation: " ++ p

-- | Write the samples to file.
writeChainToCSV :: [Chain [Double] b] -> FilePath -> IO ()
writeChainToCSV samples csv = writeFile csv $ samples2CSV samples ++ "\n"
  where samples2CSV = mconcat . intersperse "\n" . map sample2Row
        sample2Row s = mconcat . intersperse "," . map show $ (chainScore s) : (chainPosition s)
