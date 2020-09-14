{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

-- import BDSCOD.Conditioning
import BDSCOD.Aggregation (aggregateUnscheduledObservations)
import BDSCOD.Llhd (initLlhdState, llhdAndNB)
import BDSCOD.Types
  ( AggregatedObservations(..)
  , AggregationTimes
  , Observation(..)
  , Parameters(..)
  , pattern AggTimes
  , maybeAggregationTimes
  , packParameters
  , putLambda
  , putMu
  , putNus
  , putOmega
  , putPsi
  , putRhos
  , scheduledTimes
  , unpackParameters
  )
import BDSCOD.Utility (eventsAsObservations)

-- import Control.Monad (liftM, zipWithM)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import Data.List (intersperse)
import Data.Maybe (fromJust, isJust)
import qualified Epidemic.BDSCOD as SimBDSCOD
import Epidemic.Types.Events (EpidemicEvent(..))

--   ( EpidemicEvent(..)
--   , asNewickString
--   , eventTime
--   , maybeEpidemicTree
--   , maybeReconstructedTree
--   )
import Epidemic.Types.Parameter (Probability, Rate, Time, Timed(..))

-- import Epidemic.Types.Population (Person(..))
import qualified Epidemic.Utility as SimUtil
import GHC.Generics
import Numeric.GSL.Minimization (MinimizeMethod(NMSimplex2), minimizeV)
import Numeric.LinearAlgebra.Data (Vector(..), fromList, linspace, toList)

-- import Numeric.LinearAlgebra.HMatrix
import System.Environment (getArgs)

-- | These objects define the specifics of the evaluation of LLHD profiles. If a
-- point estimate is given, then that is the central point of the profiles,
-- otherwise the parameters are estimated first. In every case the natural death
-- rate is set to the true values of the simulation. The boolean value is true
-- when the unscheduled observations are to be aggregated, otherwise they are
-- left as they are.
data InferenceConfiguration =
  InferenceConfiguration
    { reconstructedTreeOutputFiles :: (FilePath, FilePath)
    , observationsOutputCsv :: FilePath
    , llhdOutputCsv :: FilePath
    , pointEstimatesCsv :: FilePath
    , maybePointEstimate :: Maybe Parameters
    , icMaybeTimesForAgg :: Maybe [Time]
    }
  deriving (Show, Generic)

instance Json.FromJSON InferenceConfiguration

-- | This object configures the whole evaluation of this program and is to be
-- read in from a suitable JSON file.
--
--     * A CSV to write the whole simulation to
--     * The parameters to use for the simulation
--     * The duration of the simulation
--     * The bounds on the size of an acceptable simulation
--     * One inference configuration for the regular data (with and without
--     estimated parameters) and another for the aggregated data
--     * We can toggle printing progress on and off
data Configuration =
  Configuration
    { simulatedEventsOutputCsv :: FilePath
    , simulationParameters :: Parameters
    , simulationDuration :: Time
    , simulationSizeBounds :: (Int, Int)
    , inferenceConfigurations :: ( InferenceConfiguration
                                 , InferenceConfiguration
                                 , InferenceConfiguration)
    , isVerbose :: Bool
    }
  deriving (Show, Generic)

instance Json.FromJSON Configuration

type Simulation x = ReaderT Configuration (ExceptT String IO) x

-- | This type is used to indicate if parameters are the true ones used in the
-- simulation or estimates parameters and in the case of estimated parameters,
-- what sort of data was used to inform the estimate.
data AnnotatedParameter
  = TrueParameters Parameters
  | EstimatedParametersRegularData Parameters
  | EstimatedParametersAggregatedData Parameters
  deriving (Show, Eq)

-- | A BDSCOD simulation configuration based on the parameters in the
-- environment.
bdscodConfiguration = do
  simParams@(Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)) <- asks simulationParameters
  if pLambda > 0 && pMu > 0 && pPsi > 0 && null pRhos && null pNus
    then do simDur <- asks simulationDuration
            let bdscodConfig = SimBDSCOD.configuration simDur (unpackParameters simParams)
            case bdscodConfig of
              Nothing -> throwError "Could not construct BDSCOD configuration"
              (Just config) -> return config
    else throwError "Simulation parameters not acceptable for this program..."

-- | Simulate the actual epidemic making sure that the results are acceptable
-- before returning the results.
simulateEpidemic bdscodConfig = do
  beLoud <- asks isVerbose
  when beLoud $ liftIO (putStrLn "Running simulateEpidemic...")
  simEvents <-
    liftIO $
    SimUtil.simulationWithSystemRandom False bdscodConfig SimBDSCOD.allEvents
  (sizeLowerBound, sizeUpperBound) <- asks simulationSizeBounds
  if length simEvents > sizeLowerBound && length simEvents < sizeUpperBound
    then do
      when beLoud $ liftIO (putStrLn "simulated an acceptable epidemic...")
      simEventsCsv <- asks simulatedEventsOutputCsv
      liftIO $ L.writeFile simEventsCsv (Csv.encode simEvents)
      return simEvents
    else do
      when beLoud $ liftIO (putStrLn "Repeating epidemic simulation...")
      simulateEpidemic bdscodConfig

-- | Take a simulated epidemic and generate the observations, first with full
-- resolution of the event times and the true epidemic parameters, second with
-- the event times and the estimated parameters and third with the sampling
-- times aggregated as defined in the inference configuration.
--
-- __TODO__ This is where we need to include the code to write the trees out for
-- visualisation purposes.
observeEpidemicTwice ::
  [EpidemicEvent]
  -> ( InferenceConfiguration
     , InferenceConfiguration
     , InferenceConfiguration)
  -> Simulation ( (InferenceConfiguration, [Observation])
                , (InferenceConfiguration, [Observation])
                , (InferenceConfiguration, AggregatedObservations))
observeEpidemicTwice simEvents (regInfConfig, regInfConfig', aggInfConfig) = do
  let maybeRegObs = eventsAsObservations <$> SimBDSCOD.observedEvents simEvents
      maybeAggTimes = icMaybeTimesForAgg aggInfConfig >>= maybeAggregationTimes
  case maybeAggTimes of
    (Just aggTimes) -> let maybeAggObs = aggregateUnscheduledObservations aggTimes =<< maybeRegObs
                           in case (maybeRegObs,maybeAggObs) of
                                (Just regObs,Just aggObs) -> return ( (regInfConfig, regObs)
                                                                    , (regInfConfig', regObs)
                                                                    , (aggInfConfig, aggObs))
                                (Just _, Nothing) -> throwError "Could not evaluate aggregated observations..."
                                (Nothing, Just _) -> throwError "Could not evaluate regular observations..."
                                (Nothing, Nothing) -> throwError "Could not evaluate either set of observations..."
    Nothing -> throwError "Could not evaluate aggregation times..."

-- | Recenter the evaluation parametes about the parameters given so that we can
-- get a sensible range of values for visualisation. Since there are several
-- types of parameters in use, the `AnnotatedParameter` type is used to convey
-- what to do. __NOTE__ that for the true parameters and the estimates under the
-- regular data this function is the same, the only time it needs to be
-- different is when using the parameters estimated from the aggregated data
-- since the parameter space is different.
adjustedEvaluationParameters :: AnnotatedParameter -> Simulation [Parameters]
adjustedEvaluationParameters (TrueParameters ps) =
  let meshSize = 100
      lambdaMesh = toList $ linspace meshSize (1, 2.5)
      muMesh = toList $ linspace meshSize (0.05, 1.5)
      psiMesh = toList $ linspace meshSize (0.05, 1.5)
      probMesh = toList $ linspace meshSize (0.05, 0.6) :: [Probability]
      (rhoTimes, nuTimes) = scheduledTimes ps
      rhoMesh = [Timed [(t, r) | t <- rhoTimes] | r <- probMesh]
      omegaMesh = toList $ linspace meshSize (0.05, 1.5)
      nuMesh = [Timed [(t, n) | t <- nuTimes] | n <- probMesh]
      apply f = map (f ps)
      [lPs, mPs, pPs, oPs] =
        zipWith
          apply
          [putLambda, putMu, putPsi, putOmega]
          [lambdaMesh, muMesh, psiMesh, omegaMesh]
      [rPs, nPs] = zipWith apply [putRhos, putNus] [rhoMesh, nuMesh]
   in return $ concat [lPs, mPs, pPs, rPs, oPs, nPs]
adjustedEvaluationParameters (EstimatedParametersRegularData ps) =
  adjustedEvaluationParameters (TrueParameters ps)
adjustedEvaluationParameters (EstimatedParametersAggregatedData _) = undefined

-- | Evaluate the NB posterior approximation of the prevalence for a single
-- point in parameter space and the LLHD over a list of points and write all of
-- the results to CSV including a description of the type of parameters and data
-- used.
generateLlhdProfileCurves ::
     InferenceConfiguration
  -> [Observation]
  -> (AnnotatedParameter, [Parameters])
  -> Simulation ()
generateLlhdProfileCurves InferenceConfiguration {..} obs (centerParam, evalParams) =
  let comma = BBuilder.charUtf8 ','
      (parametersUsed,singleParams) = case centerParam of
                                        (TrueParameters x) -> ("true_parameters_regular_data",x)
                                        (EstimatedParametersRegularData x) -> ("estimated_parameters_regular_data",x)
                                        (EstimatedParametersAggregatedData x) -> ("estimated_parameters_aggregated_data",x)
      parametersUsed' = BBuilder.stringUtf8 parametersUsed
      llhdVals = [fst $ llhdAndNB obs p initLlhdState | p <- evalParams]
      nBVal =
        pure (parametersUsed, snd $ llhdAndNB obs singleParams initLlhdState)
      doublesAsString =
        BBuilder.toLazyByteString .
        mconcat .
        intersperse comma . (parametersUsed' :) . map BBuilder.doubleDec
   in do liftIO $ L.appendFile llhdOutputCsv (doublesAsString llhdVals)
         liftIO $ L.appendFile pointEstimatesCsv (Csv.encode nBVal)

-- | Run the evaluation of the log-likelihood profiles on a given set of regular
-- (i.e., disaggregated) observations at the parameters used to simulate the
-- data set and write the result to file. This will also evaluate the density of
-- the prevalence at the present and write that to file.
evaluateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
evaluateLLHD infConfig obs = do
  liftIO (putStrLn "Running evaluateLLHD...")
  simParams <- asks simulationParameters -- get the actual parameters used to simulate the observations
  evalParams <- adjustedEvaluationParameters (TrueParameters simParams)
  generateLlhdProfileCurves infConfig obs (TrueParameters simParams, evalParams)

-- | Using regular (i.e., disaggregated) observations, estimate the parameters
-- and evaluate the log-likelihood profiles and write the result to file. This
-- will also evaluate the density of the prevalence at the present and write
-- that to file. __NOTE__ This uses the actual simulation parameters as a way to
-- get the scheduled observation times, they are not used in the inference, that
-- starts at a fixed initial condition.
estimateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
estimateLLHD infConfig obs = do
  liftIO (putStrLn "Running estimateLLHD...")
  simParams@(Parameters (_, deathRate, _, _, _, _)) <- asks simulationParameters
  let schedTimes = scheduledTimes simParams
      mleParams = estimateRegularParameters deathRate schedTimes obs -- get the MLE estimate of the parameters
      annotatedMLE = EstimatedParametersRegularData mleParams
  evalParams <- adjustedEvaluationParameters annotatedMLE -- generate a list of evaluation parameters
  generateLlhdProfileCurves infConfig obs (annotatedMLE, evalParams)

-- | Using __aggregated__ observations, estimate the parameters
-- and evaluate the log-likelihood profiles and write the result to file. This
-- will also evaluate the density of the prevalence at the present and write
-- that to file.
estimateLLHDAggregated ::
     InferenceConfiguration -> AggregatedObservations -> Simulation ()
estimateLLHDAggregated infConfig (AggregatedObservations (AggTimes aggTimes) obs) = do
  liftIO (putStrLn "Running estimateLLHDAggregated...")
  Parameters (_, deathRate, _, _, _, _) <- asks simulationParameters
  let schedTimes = (aggTimes,undefined)
      mleParams = estimateAggregatedParameters deathRate schedTimes obs -- get the MLE estimate of the parameters
      annotatedMLE = EstimatedParametersAggregatedData mleParams
  evalParams <- adjustedEvaluationParameters annotatedMLE -- generate a list of evaluation parameters
  generateLlhdProfileCurves infConfig obs (annotatedMLE, evalParams)

-- | This is the main entry point to the actual simulation study. Since this is
-- within the simulation monad it has access to all the configuration data and
-- can perform IO.
simulationStudy :: Simulation ()
simulationStudy = do
  bdscodConfig <- bdscodConfiguration -- get a simulation configuration
  epiSim <- simulateEpidemic bdscodConfig -- simulate the transmission process
  infConfigs <- asks inferenceConfigurations -- get the inference configurations
  simParams <- asks simulationParameters -- get the parameters of the simulation
  (regObs, regObs', aggObs) <- observeEpidemicTwice epiSim infConfigs
  uncurry evaluateLLHD regObs -- evaluate profiles about true parameters
  uncurry estimateLLHD regObs' -- evaluate profiles about estimated parameters
  uncurry estimateLLHDAggregated aggObs -- evaluate profiles about estimated parameters from aggregated data.

main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  maybeConfig <- getConfiguration configFilePath
  case maybeConfig of
    Nothing ->
      putStrLn $ "Could not get configuration from file: " ++ configFilePath
    Just config -> do
      result <- runExceptT (runReaderT simulationStudy config)
      case result of
        Left errMsg -> putStrLn errMsg
        Right _ -> return ()

getConfiguration :: FilePath -> IO (Maybe Configuration)
getConfiguration fp = Json.decode <$> L.readFile fp

-- | Use GSL to estimate the MLE based on the observations given. This uses a
-- simplex method because it seems to be faster and more accurate than the
-- simulated annealing.
--
-- __NOTE__ This will fit the model assuming that we have __aggregated__
-- observations.
--
-- __NOTE__ we fix the death rate to the true value because
-- this is assumed to be known a priori.
estimateAggregatedParameters ::
     Rate -> ([Time], [Time]) -> [Observation] -> Parameters
estimateAggregatedParameters = undefined

-- | Use GSL to estimate the MLE based on the observations given. This uses a
-- simplex method because it seems to be faster and more accurate than the
-- simulated annealing.
--
-- __NOTE__ This will fit the model assuming that we have __disaggregated__
-- observations.
--
-- __NOTE__ we fix the death rate to the true value because
-- this is assumed to be known a priori.
estimateRegularParameters ::
     Rate -> ([Time], [Time]) -> [Observation] -> Parameters
estimateRegularParameters deathRate sched obs =
  let maxIters = 500
      desiredPrec = 1e-3
      initBox = fromList [2, 2, 2, 2, 2]
      energyFunc x =
        negate . fst $
        llhdAndNB obs (vectorAsParameters deathRate sched x) initLlhdState
      randInit = fromList [0, 0, 0, 0, 0]
      (est, _) =
        minimizeV NMSimplex2 desiredPrec maxIters initBox energyFunc randInit
   in vectorAsParameters deathRate sched est

-- | Helper function for @estimateRegularParameters@
vectorAsParameters :: Rate -> ([Time], [Time]) -> Vector Double -> Parameters
vectorAsParameters deathRate (rhoTimes, nuTimes) paramVec =
  let [lnR1, lnR2, lnP1, lnR3, lnP2] = toList paramVec
      p1 = invLogit lnP1
      p2 = invLogit lnP2
   in packParameters
        ( exp lnR1
        , deathRate
        , exp lnR2
        , [(t, p1) | t <- rhoTimes]
        , exp lnR3
        , [(t, p2) | t <- nuTimes])

invLogit :: Double -> Probability
invLogit a = 1 / (1 + exp (-a))

logit :: Probability -> Double
logit p = log (p / (1 - p))
