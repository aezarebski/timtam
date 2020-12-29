{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import BDSCOD.Aggregation
  ( AggregatedObservations(..)
  , AggregationTimes
  , pattern AggTimes
  , aggregateUnscheduledObservations
  , maybeAggregationTimes
  )
import BDSCOD.Llhd
  ( initLlhdState
  , llhdAndNB
  , logPdeStatistics
  , pdeGF
  , pdeStatistics
  )
import BDSCOD.Types
  ( LogLikelihood(..)
  , NegativeBinomial(..)
  , NumLineages
  , Observation(..)
  , ObservedEvent(..)
  , PDESolution(..)
  , Parameters(..)
  , MCMCConfiguration(..)
  , MWCSeed
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
import BDSCOD.Utility (eventsAsObservations, invLogit)
import Control.Monad (liftM2, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Char8 (pack, singleton)
import qualified Data.Csv as Csv
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as Unboxed
import qualified Epidemic.BDSCOD as SimBDSCOD
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , asNewickString
  , maybeEpidemicTree
  , maybeReconstructedTree
  )
import Epidemic.Types.Parameter (Probability, Rate, AbsoluteTime(..), TimeDelta(..), Timed(..))
import Epidemic.Types.Population (People(..), Person(..), numPeople, Identifier(..))
import Epidemic.Utility
import qualified Epidemic.Utility as SimUtil
import GHC.Generics
import Numeric.GSL.Minimization (MinimizeMethod(NMSimplex2), minimizeV)
import Numeric.LinearAlgebra (dot)
import Numeric.LinearAlgebra.Data (Vector(..), fromList, linspace, toList)
import Numeric.MCMC.Metropolis
import System.Environment (getArgs)
import System.Random.MWC (initialize)


-- | Record of the scheduled observation times.
data ScheduledTimes =
  ScheduledTimes
    { stRhoTimes :: [AbsoluteTime]
    , stNuTimes :: [AbsoluteTime]
    }
  deriving (Show)

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
    , icMaybeTimesForAgg :: Maybe ([AbsoluteTime], [AbsoluteTime])
    , icMaybeMCMCConfig :: Maybe MCMCConfiguration
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
--     estimated parameters) and another for the aggregated data with estimated
--     parameters.
--     * A Boolean to toggle printing progress on and off with the
--     @isVerbosePutStrLn@ function.
data Configuration =
  Configuration
    { simulatedEventsOutputCsv :: FilePath
    , simulationParameters :: Parameters
    , simulationDuration :: TimeDelta
    , simulationSizeBounds :: (Int, Int)
    , inferenceConfigurations :: ( InferenceConfiguration
                                 , InferenceConfiguration
                                 , InferenceConfiguration)
    , isVerbose :: Bool
    , configSimulationSeed :: MWCSeed
    }
  deriving (Show, Generic)

instance Json.FromJSON Configuration

-- | Monad for running a simulation, it uses the ExceptT for error messages and
-- the ReaderT for holding program configuration.
type Simulation x = ReaderT Configuration (ExceptT String IO) x

-- | A convenience function for printing output if in verbose mode when in the
-- 'Simulation' monad.
ifVerbosePutStrLn :: String -> Simulation ()
ifVerbosePutStrLn msg =
  do
    beLoud <- asks isVerbose
    when beLoud $ liftIO (putStrLn msg)

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
  simParams@(Parameters (pLambda, pMu, pPsi, Timed pRhos, pOmega, Timed pNus)) <-
    asks simulationParameters
  if pLambda > 0 && pMu > 0 && pPsi > 0 && pOmega > 0 && null pRhos && null pNus
    then do
      (TimeDelta simDur) <- asks simulationDuration
      let bdscodConfig =
            SimBDSCOD.configuration (AbsoluteTime simDur) (unpackParameters simParams)
      case bdscodConfig of
        Nothing -> throwError "Could not construct BDSCOD configuration"
        (Just config) -> return config
    else throwError "Simulation parameters not acceptable for this program..."

-- | Check if the LTT ever returns to 1 after being larger than 1.
pMultipleOrigins :: [EpidemicEvent] -> Bool
pMultipleOrigins epiEvents = go 1 epiEvents
  where
    numLineage :: People -> NumLineages
    numLineage = fromIntegral . numPeople
    go :: NumLineages -> [EpidemicEvent] -> Bool
    go _ [] = False
    go n (Removal _ _:ees) = (n < 3) || go (n-1) ees
    go n (Sampling _ _:ees) = (n < 3) || go (n-1) ees
    go n (Occurrence _ _:ees) = (n < 3) || go (n-1) ees
    go n (Catastrophe _ people:ees) = if m > 1 then go m ees else True where m = n - numLineage people
    go n (Disaster _ people:ees) = if m > 1 then go m ees else True where m = n - numLineage people
    go n (Infection _ _ _:ees) = go (n+1) ees


-- | Simulate the actual epidemic making sure that the results are acceptable
-- before returning, otherwise try using a different seed.
simulateEpidemic seedInt bdscodConfig = do
  ifVerbosePutStrLn "Running simulateEpidemic..."
  genIO <- liftIO $ prngGen seedInt
  simEvents <-
    liftIO $ SimUtil.simulation' bdscodConfig SimBDSCOD.allEvents genIO
  (sizeLowerBound, sizeUpperBound) <- asks simulationSizeBounds
  if length simEvents > sizeLowerBound && length simEvents < sizeUpperBound && (not (pMultipleOrigins simEvents))
    then do
      ifVerbosePutStrLn "simulated an acceptable epidemic..."
      simEventsCsv <- asks simulatedEventsOutputCsv
      ifVerbosePutStrLn $
        "\twriting events to: " ++
        simEventsCsv ++ "\n\tthere are " ++ show (length simEvents) ++ " events."
      liftIO $ L.writeFile simEventsCsv (Csv.encode simEvents)
      return simEvents
    else do
      ifVerbosePutStrLn $ "\tthere where multiple origins: " ++ show (pMultipleOrigins simEvents)
      ifVerbosePutStrLn $ "\trepeating the simulation with seed: " ++ show seedInt
      simulateEpidemic (succ seedInt) bdscodConfig

_isSamplingEE :: EpidemicEvent -> Bool
_isSamplingEE e = case e of
  (Sampling _ _) -> True
  _ -> False

-- | Take a simulated epidemic and generate the observations, first with full
-- resolution of the event times and the true epidemic parameters, second with
-- the event times and the estimated parameters and third with the sampling
-- times aggregated as defined in the inference configuration. This is done all
-- at the same time because the raw epidemic events are needed to genereate the
-- observations and the aggregated observations.
--
-- NOTE that this observation model assumes that it is acceptable to remove
-- every occurrence from the raw events that happens after the last unscheduled
-- sequenced sample.
--
observeEpidemicThrice ::
     [EpidemicEvent] -- ^ the raw simulated epidemic events
  -> Simulation ( (InferenceConfiguration, [Observation])
                , (InferenceConfiguration, [Observation])
                , (InferenceConfiguration, AggregatedObservations) -- ^ a triplet of data sets ready for analysis.
                 )
observeEpidemicThrice simEvents' = do
  (regInfConfig, regInfConfig', aggInfConfig) <- asks inferenceConfigurations
  let simEvents =
        reverse . dropWhile (not . _isSamplingEE) . reverse $ simEvents'
      maybeRegObs = eventsAsObservations <$> SimBDSCOD.observedEvents simEvents
      maybeAggTimes =
        icMaybeTimesForAgg aggInfConfig >>= uncurry maybeAggregationTimes
      maybeAggObs =
        liftM2 aggregateUnscheduledObservations maybeAggTimes maybeRegObs
      (reconNewickTxt, reconNewickCsv) =
        reconstructedTreeOutputFiles regInfConfig
      maybeNewickData =
        asNewickString (AbsoluteTime 0, Person (Identifier 1)) =<<
        maybeReconstructedTree =<< maybeEpidemicTree simEvents
  case maybeNewickData of
    Just (newickBuilder, newickMetaData) -> do
      ifVerbosePutStrLn $
        "Writing reconstructed tree Newick to " ++ reconNewickTxt
      liftIO $
        L.writeFile reconNewickTxt (BBuilder.toLazyByteString newickBuilder)
      ifVerbosePutStrLn $
        "Writing reconstructed tree tip labels to " ++ reconNewickCsv
      liftIO $ L.writeFile reconNewickCsv (Csv.encode newickMetaData)
    Nothing -> throwError "Could not reconstruct tree..."
  case (maybeRegObs, maybeAggObs) of
    (Just regObs, Just aggObs@(AggregatedObservations _ unboxedAggObs)) -> do
      ifVerbosePutStrLn "Writing the regular observations to CSV"
      liftIO $
        L.writeFile (observationsOutputCsv regInfConfig) (Csv.encode regObs)
      ifVerbosePutStrLn "Writing the aggregated observations to CSV"
      liftIO $
        L.writeFile
          (observationsOutputCsv aggInfConfig)
          (Csv.encode unboxedAggObs)
      return
        ( (regInfConfig, regObs)
        , (regInfConfig', regObs)
        , (aggInfConfig, aggObs))
    (Just _, Nothing) ->
      throwError "Could not evaluate aggregated observations..."
    (Nothing, Just _) -> throwError "Could not evaluate regular observations..."
    (Nothing, Nothing) ->
      throwError "Could not evaluate either set of observations..."


-- | Evaluate the NB posterior approximation of the prevalence for a single
-- point in parameter space.
recordPresentPrevalenceEstimate ::
     InferenceConfiguration
  -> [Observation]
  -> AnnotatedParameter
  -> Simulation ()
recordPresentPrevalenceEstimate InferenceConfiguration {..} obs centerParam =
  let (parametersUsed,singleParams) = case centerParam of
                                        (TrueParameters x) -> ("true_parameters_regular_data" :: L.ByteString,x)
                                        (EstimatedParametersRegularData x) -> ("estimated_parameters_regular_data",x)
                                        (EstimatedParametersAggregatedData x) -> ("estimated_parameters_aggregated_data",x)
      nBVal = pure (parametersUsed, snd $ llhdAndNB obs singleParams initLlhdState)
   in do ifVerbosePutStrLn $ "\twriting NB values to: " ++ pointEstimatesCsv
         liftIO $ L.writeFile pointEstimatesCsv (Csv.encode nBVal)

-- | Run the estimation of the prevalence (at present) on the given set of regular
-- (i.e., disaggregated) observations at the parameters used to simulate the
-- data set and write the result to file.
evaluateLLHD :: InferenceConfiguration
             -> [Observation] -- ^ the regular observations
             -> Simulation ()
evaluateLLHD infConfig obs = do
  ifVerbosePutStrLn "Running evaluateLLHD..."
  simParams <- asks simulationParameters
  ifVerbosePutStrLn "\tUsing non-aggregated data with the true parameters..."
  ifVerbosePutStrLn $ show simParams
  recordPresentPrevalenceEstimate infConfig obs (TrueParameters simParams)

-- | Using regular (i.e., disaggregated) observations, estimate the parameters
-- used in the simulation and run the estimation of the prevalence (at present)
-- using this estimate. Finally generate MCMC samples of the posterior
-- distribution to understand the likelihood surface.
--
-- __NOTE__ This uses the actual simulation parameters as a way to get the
-- scheduled observation times, /they are not used in the inference/. The
-- optimisation starts at a fixed initial condition.
--
estimateLLHD :: InferenceConfiguration
             -> [Observation] -- ^ the regular observations
             -> Simulation ()
estimateLLHD infConfig obs = do
  ifVerbosePutStrLn "Running estimateLLHD..."
  Parameters (_, deathRate, _, _, _, _) <- asks simulationParameters
  let mleParams = estimateRegularParameters deathRate obs
      annotatedMLE = EstimatedParametersRegularData mleParams
  ifVerbosePutStrLn "\tUsing non-aggregated data the computed MLE is..."
  ifVerbosePutStrLn $ "\t" ++ show annotatedMLE
  recordPresentPrevalenceEstimate infConfig obs annotatedMLE
  runUnscheduledObservationMCMC infConfig deathRate obs annotatedMLE

-- | Using __aggregated__ observations, estimate the parameters of the process
-- and run the estimation of the prevalence (at present) using this estimate.
-- Finally generate MCMC samples of the posterior distribution to understand the
-- likelihood surface.
estimateLLHDAggregated :: InferenceConfiguration
                       -> AggregatedObservations -- ^ the aggregated observations
                       -> Simulation ()
estimateLLHDAggregated infConfig (AggregatedObservations _ obs) = do
  ifVerbosePutStrLn "Running estimateLLHDAggregated..."
  Parameters (_, deathRate, _, _, _, _) <- asks simulationParameters
  let (Just (rhoTimes,nuTimes)) = icMaybeTimesForAgg infConfig
      schedTimes = ScheduledTimes {stRhoTimes = rhoTimes, stNuTimes = nuTimes}
      mleParams = estimateAggregatedParameters deathRate schedTimes obs
      annotatedMLE = EstimatedParametersAggregatedData mleParams
  ifVerbosePutStrLn "\tUsing aggregated data the computed MLE is..."
  ifVerbosePutStrLn $ "\t" ++ show mleParams
  recordPresentPrevalenceEstimate infConfig obs annotatedMLE
  runScheduledObservationMCMC infConfig deathRate schedTimes obs annotatedMLE

-- | Use GSL to estimate the MLE based on the observations given. This uses a
-- simplex method because it seems to be faster and more accurate than the
-- simulated annealing and does not require a derivative.
--
-- __NOTE__ This will fit the model assuming that we have __aggregated__
-- observations, i.e., it will crash if there are unscheduled samples in the
-- data.
--
-- __NOTE__ we fix the death rate to the true value, which is given as an
-- argument, because this is assumed to be known a priori also, since this is
-- being applied to aggregated data we can set the value of psi and omega to
-- zero.
--
-- __NOTE__ The @energyFunc@ uses a regularisation cost to prevent the
-- parameters from wandering off which can occur with smaller data sets.
--
-- TODO Fix the stupid settings on this!!!
--
estimateAggregatedParameters :: Rate -- ^ the known death rate
                             -> ScheduledTimes -- ^ the time of scheduled observations
                             -> [Observation] -- ^ the aggregated observations
                             -> Parameters
estimateAggregatedParameters deathRate ScheduledTimes{..} obs =
  let maxIters = 100
      desiredPrec = 1e-4
      initBox = fromList ([0.1, 0.1, 0.1] :: [Rate]) -- lambda, rho (because mu, psi, omega and nu are fixed)
      randInit = fromList ([0.1, -0.1, 0.1] :: [Rate])
      vecAsParams x =
        let [lnR1, lnP1, lnP2] = toList x
            p1 = invLogit lnP1
            p2 = invLogit lnP2
            r = exp lnR1
            rts = [(t, p1) | t <- stRhoTimes]
            nts = [(t, p2) | t <- stNuTimes]
          in packParameters (r, deathRate, 0, rts, 0, nts)
      energyFunc x =
        let negLlhd = negate . fst $ llhdAndNB obs (vecAsParams x) initLlhdState
            regCost = 10 * dot x x
          in negLlhd + regCost
      (est, _) = minimizeV NMSimplex2 desiredPrec maxIters initBox energyFunc randInit
   in vecAsParams est


-- | This is the main entry point to the actual simulation study, it is its own
-- function to avoid stressing about configuration IO. Since this is within the
-- simulation monad it has access to all the configuration data and can perform
-- IO.
simulationStudy :: Simulation ()
simulationStudy = do
  bdscodConfig <- bdscodConfiguration
  prngSeed <- asks configSimulationSeed
  epiSim <- simulateEpidemic prngSeed bdscodConfig
  (regObs, regObs', aggObs) <- observeEpidemicThrice epiSim
  uncurry evaluateLLHD regObs
  uncurry estimateLLHD regObs'
  uncurry estimateLLHDAggregated aggObs

-- =============================================================================
-- The following can be used in the REPL to test things out.
--
-- TODO This comment section needs to be deleted once the application has been
-- finished and tested.
--
replMain :: IO ()
replMain = do
  let configFilePath = "agg-app-config.json"
  maybeConfig <- Json.decode <$> L.readFile configFilePath
  case maybeConfig of
    Nothing -> putStrLn $ "Could not get configuration from file: " ++ configFilePath
    (Just config) -> do result <- runExceptT (runReaderT simulationStudy config)
                        case result of
                          Right () -> return ()
                          Left errMsg -> do putStrLn errMsg; return ()
--
-- =============================================================================

-- | We use an external file to configure the simulation so that it is easier to
-- try different parameter values and random seeds. The @simulationStudy@
-- function is real driver of this and runs in the @Simulation@ monad.
main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  maybeConfig <- Json.decode <$> L.readFile configFilePath
  case maybeConfig of
    Nothing ->
      putStrLn $ "Could not get configuration from file: " ++ configFilePath
    Just config -> do
      result <- runExceptT (runReaderT simulationStudy config)
      case result of
        Left errMsg -> putStrLn errMsg
        Right _ -> return ()



-- | Use GSL to estimate the MLE based on the observations given. This uses a
-- simplex method because it seems to be faster and more accurate than the
-- simulated annealing.
--
-- __NOTE__ This will fit the model assuming that we have __disaggregated__
-- observations.
--
-- __NOTE__ we fix the death rate to the true value because
-- this is assumed to be known a priori.
--
estimateRegularParameters ::
     Rate -> [Observation] -> Parameters
estimateRegularParameters deathRate obs =
  let maxIters = 100
      desiredPrec = 1e-4
      initBox = fromList [0.2, 0.2, 0.2] -- lambda, psi (because mu, rho, omega and nu are fixed)
      randInit = fromList [0.1, 0.0, 0.0]
      vecAsParams x =
        let [lnR1, lnR2, lnR3] = toList x
            r1 = exp lnR1
            r2 = exp lnR2
            r3 = exp lnR3
           in packParameters (r1, deathRate, r2, [], r3, [])
      energyFunc x =
        let negLlhd = negate . fst $ llhdAndNB obs (vecAsParams x) initLlhdState
         in negLlhd
      (est, _) = minimizeV NMSimplex2 desiredPrec maxIters initBox energyFunc randInit
   in vecAsParams est

-- | Action to run an MCMC analysis based on regular unscheduled observations.
runUnscheduledObservationMCMC :: InferenceConfiguration
                              -> Rate
                              -> [Observation]
                              -> AnnotatedParameter
                              -> Simulation ()
runUnscheduledObservationMCMC InferenceConfiguration {..} deathRate obs (EstimatedParametersRegularData (Parameters (mleR1, _, mleR2, _, mleR3, _))) =
  case icMaybeMCMCConfig of
    (Just mcmcConfig) ->
      let numIters = mcmcNumIters mcmcConfig
          stepSd = mcmcStepSD mcmcConfig
          variableNames = ["lambda", "psi", "omega", "nbSize", "nbProb"]
          x0 = [mleR1, mleR2, mleR3]
          listAsParams [r1, r2, r3] =
            packParameters (r1, deathRate, r2, [], r3, [])
          logPost x = fst $ llhdAndNB obs (listAsParams x) initLlhdState
          prngSeed = mcmcSeed mcmcConfig
          maybeGenQuantityFunc = Just (\x -> snd $ llhdAndNB obs (listAsParams x) initLlhdState)
       in do ifVerbosePutStrLn "Running runUnscheduledObservationMCMC..."
             genIO <- liftIO $ prngGen prngSeed
             chainVals <-
               liftIO $ (asGenIO $ chain' numIters stepSd x0 logPost maybeGenQuantityFunc) genIO
             liftIO $
               L.writeFile
                 (mcmcOutputCSV mcmcConfig)
                 (chainAsByteString' variableNames chainVals)
             return ()
    Nothing -> ifVerbosePutStrLn "No MCMC configuration found!"

runScheduledObservationMCMC :: InferenceConfiguration
                              -> Rate
                              -> ScheduledTimes
                              -> [Observation]
                              -> AnnotatedParameter
                              -> Simulation ()
runScheduledObservationMCMC InferenceConfiguration {..} deathRate ScheduledTimes {..} obs (EstimatedParametersAggregatedData (Parameters (mleR1, _, _, _, _, _))) =
  case icMaybeMCMCConfig of
    (Just mcmcConfig) ->
      let numIters = mcmcNumIters mcmcConfig
          stepSd = mcmcStepSD mcmcConfig
          variableNames = ["lambda", "rho", "nu", "nbSize", "nbProb"]
          x0 = [mleR1, 0.5, 0.5]
          listAsParams [r1, p1, p2] =
            packParameters
              ( r1
              , deathRate
              , 0
              , [(rt, p1) | rt <- stRhoTimes]
              , 0
              , [(nt, p2) | nt <- stNuTimes])
          -- logPost x@([r1,p1,p2]) = llhd - lnNotExtinct where llhd = fst $ llhdAndNB obs (listAsParams x) initLlhdState; lnNotExtinct = log (r1 - (p1 + p2 + deathRate)) - log r1
          logPost x = fst $ llhdAndNB obs (listAsParams x) initLlhdState
          prngSeed = mcmcSeed mcmcConfig
          maybeGenQuantityFunc = Just (\x -> snd $ llhdAndNB obs (listAsParams x) initLlhdState)
       in do ifVerbosePutStrLn "Running runScheduledObservationMCMC..."
             genIO <- liftIO $ prngGen prngSeed
             chainVals <-
               liftIO $ (asGenIO $ chain' numIters stepSd x0 logPost maybeGenQuantityFunc) genIO
             liftIO $
               L.writeFile
                 (mcmcOutputCSV mcmcConfig)
                 (chainAsByteString' variableNames chainVals)
             return ()
    Nothing -> ifVerbosePutStrLn "No MCMC configuration found!"

-- | A bytestring representation of the MCMC samples.
chainAsByteString :: [String] -- ^ the names of the elements of the chain
                  -> [Chain [Double] b] -- ^ the samples in the chain
                  -> L.ByteString
chainAsByteString varNames chainVals =
  let header = pack $ intercalate "," ("llhd" : varNames)
      records = Csv.encode [chainScore cv : chainPosition cv | cv <- chainVals]
      linebreak = singleton '\n'
   in mconcat [header, linebreak, records]

-- | A bytestring representation of the MCMC samples.
chainAsByteString' :: [String] -- ^ the names of the elements of the chain
                  -> [Chain [Double] NegativeBinomial] -- ^ the samples in the chain
                  -> L.ByteString
chainAsByteString' varNames chainVals =
  let header = pack $ intercalate "," ("llhd" : varNames)
      nbParams cv =
        case chainTunables cv of
          Just (NegBinomSizeProb r p) -> [r, p]
          Just Zero -> [0, 0]
          Nothing -> [-1, -1]
      records =
        Csv.encode
          [chainScore cv : (chainPosition cv ++ nbParams cv) | cv <- chainVals]
      linebreak = singleton '\n'
   in mconcat [header, linebreak, records]

-- | A generator for random numbers from a seed.
prngGen seed = initialize (Unboxed.fromList [seed])
