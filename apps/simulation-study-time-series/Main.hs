{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Conditioning
import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import Control.Monad (liftM, zipWithM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import Data.List (intercalate, intersperse)
import Data.Maybe (fromJust)
import qualified Epidemic.BDSCOD as SimBDSCOD
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , asNewickString
  , eventTime
  , maybeEpidemicTree
  , maybeReconstructedTree
  )
import Epidemic.Types.Parameter
import Epidemic.Types.Population (Person(..))
import qualified Epidemic.Utility as SimUtil
import GHC.Generics
import Numeric.GSL.Minimization (MinimizeMethod(NMSimplex2), minimizeV)
import Numeric.LinearAlgebra.Data (linspace, toList)
import Numeric.LinearAlgebra.HMatrix

import System.Environment (getArgs)

-- | These objects define the specifics for an inference evaluation which can be
-- run at differing points in the simulation to understand how differing amounts
-- of data influences the results.
data InferenceConfiguration =
  InferenceConfiguration
    { inferenceTime :: Time
    , reconstructedTreeOutputFiles :: (FilePath, FilePath)
    , observationsOutputCsv :: FilePath
    , llhdOutputCsv :: FilePath
    , pointEstimatesCsv :: FilePath
    }
  deriving (Show, Generic)

-- | This object configures the whole evaluation of this program and is to be
-- read in from a suitable JSON file.
data Configuration =
  Configuration
    { simulatedEventsOutputCsv :: FilePath
    , simulationParameters :: Parameters
    , simulationDuration :: Time
    , simulationSizeBounds :: (Int,Int)
    , inferenceConfigurations :: [InferenceConfiguration]
    , partialEvaluationOutputCsv :: FilePath
    }
  deriving (Show, Generic)

instance Json.FromJSON Configuration

instance Json.FromJSON InferenceConfiguration

type Simulation x = ReaderT Configuration (ExceptT String IO) x

-- | This type is used to indicate if parameters are the true ones used in the
-- simulation or estimates parameters.
data ParameterKind
  = SimulationParameters
  | EstimatedParameters
  deriving (Show, Eq)

-- A BDSCOD simulation configuration based on the parameters in the environment.
bdscodConfiguration = do
  simParams <- asks simulationParameters
  simDur <- asks simulationDuration
  let bdscodConfig = SimBDSCOD.configuration simDur (unpackParameters simParams)
  case bdscodConfig of
    Nothing -> throwError "Could not construct BDSCOD configuration"
    (Just config) -> return config

partialSimulatedEpidemic bdscodConfig =
  do
    -- simulate the epidemic without conditioning because the conditioning code
    -- assumes there will be psi-samples. Use the system random to get a better
    -- idea of robustness of the simulation.
    simEvents <- liftIO $ SimUtil.simulationWithSystemRandom False bdscodConfig SimBDSCOD.allEvents
    (sizeLowerBound,sizeUpperBound) <- asks simulationSizeBounds
    if length simEvents > sizeLowerBound && length simEvents < sizeUpperBound
      then do infTimes <- map inferenceTime <$> asks inferenceConfigurations
              simEventsCsv <- asks simulatedEventsOutputCsv
              liftIO $ L.writeFile simEventsCsv (Csv.encode simEvents)
              return [filter (\e -> eventTime e <= infTime) simEvents | infTime <- infTimes]
      else do liftIO $ putStrLn "Repeating epidemic simulation..."
              partialSimulatedEpidemic bdscodConfig

-- Run the actual simulation and record the results before returning the dataset
-- of observations generated by this epidemic.
simulatedObservations :: InferenceConfiguration
                      -> [EpidemicEvent]
                      -> Simulation (InferenceConfiguration,[Observation])
simulatedObservations infConfig@InferenceConfiguration{..} simEvents = do
  let Just (newickBuilder,newickMetaData) = asNewickString (0, Person 1) =<< maybeReconstructedTree =<< maybeEpidemicTree simEvents
      maybeObs = eventsAsObservations <$> SimBDSCOD.observedEvents simEvents
      (reconNewickTxt,reconNewickCsv) = reconstructedTreeOutputFiles
  case maybeObs of
    (Just obs) -> do liftIO $ L.writeFile reconNewickTxt (BBuilder.toLazyByteString newickBuilder)
                     liftIO $ L.writeFile reconNewickCsv (Csv.encode newickMetaData)
                     liftIO $ L.writeFile observationsOutputCsv (Csv.encode obs)
                     return (infConfig,obs)
    Nothing -> throwError "Failed to simulate observations."


-- | Evaluate the NB posterior approximation of the prevalence for a single
-- point in parameter space and the LLHD over a list of points and write all of
-- the results to CSV.
generateLlhdProfileCurves :: InferenceConfiguration
                          -> [Observation]
                          -> (Parameters,ParameterKind,[Parameters])
                          -> Simulation ()
generateLlhdProfileCurves InferenceConfiguration{..} obs (singleParams,paramKind,evalParams) =
  let comma = BBuilder.charUtf8 ','
      parametersUsed = show paramKind
      parametersUsed' = BBuilder.stringUtf8 parametersUsed
      llhdVals = [fst $ llhdAndNB obs p initLlhdState | p <- evalParams]
      nBVal = pure (parametersUsed,snd $ llhdAndNB obs singleParams initLlhdState)
      doublesAsString = BBuilder.toLazyByteString . mconcat . intersperse comma . (parametersUsed':) . map BBuilder.doubleDec
  in do
    liftIO $ L.appendFile llhdOutputCsv (doublesAsString llhdVals)
    liftIO $ L.appendFile pointEstimatesCsv (Csv.encode nBVal)

-- | Run the evaluation of the log-likelihood profiles on a given set of
-- observations at the parameters used to simulate the data set and write the
-- result to file.
evaluateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
evaluateLLHD infConfig obs = do
  simParams <- asks simulationParameters -- get the actual parameters used to simulate the observations
  evalParams <- adjustedEvaluationParameters simParams
  generateLlhdProfileCurves infConfig obs (simParams,SimulationParameters,evalParams)

-- | Estimate the parameters of the of the model and then evaluate the LLHD
-- profiles and prevalence and append this to the file. The first value of the
-- CSV output now describes which parameters where used to to evaluate these
-- things.
estimateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
estimateLLHD infConfig obs = do
  simParams@(Parameters (_,deathRate,_,_,_,_)) <- asks simulationParameters
  let schedTimes = scheduledTimes simParams
      mleParams = estimateParameters deathRate schedTimes obs -- get the MLE estimate of the parameters
  evalParams <- adjustedEvaluationParameters mleParams -- generate a list of evaluation parameters
  generateLlhdProfileCurves infConfig obs (mleParams,EstimatedParameters,evalParams)

-- | Use GSL to estimate the MLE based on the observations given. This uses a
-- simplex method because it seems to be faster and more accurate than the
-- simulated annealing.
--
-- __NOTE__ we fix the death rate to the true value because
-- this is assumed to be known a priori.
estimateParameters :: Rate -> ([Time],[Time]) -> [Observation] -> Parameters
estimateParameters deathRate sched obs =
  let maxIters = 500
      desiredPrec = 1e-3
      initBox = fromList [2,2,2,2,2]
      energyFunc x = negate . fst $ llhdAndNB obs (vectorAsParameters deathRate sched x) initLlhdState
      randInit = fromList [0,0,0,0,0]
      (est,_) = minimizeV NMSimplex2 desiredPrec maxIters initBox energyFunc randInit
  in vectorAsParameters deathRate sched est

-- | Helper function for @estimateParameters@
vectorAsParameters :: Rate -> ([Time],[Time]) -> Vector Double -> Parameters
vectorAsParameters deathRate (rhoTimes, nuTimes) paramVec =
  let [lnR1, lnR2, lnP1, lnR3, lnP2] = toList paramVec
      p1 = invLogit lnP1
      p2 = invLogit lnP2
  in packParameters (exp lnR1, deathRate, exp lnR2, [(t,p1)|t<-rhoTimes], exp lnR3, [(t,p2)|t<-nuTimes])


invLogit :: Double -> Probability
invLogit a = 1 / (1 + exp (- a))

logit :: Probability -> Double
logit p = log (p / (1 - p))

-- | Recenter the evaluation parametes about the parameters given.
adjustedEvaluationParameters :: Parameters -> Simulation [Parameters]
adjustedEvaluationParameters ps =
  let lambdaMesh = toList $ linspace 100 (1,2.5)
      muMesh = toList $ linspace 100 (0.05,1.5)
      psiMesh = toList $ linspace 100 (0.05,1.5)
      omegaMesh = toList $ linspace 100 (0.05,1.5)
      apply f = map (f ps)
  in return . concat $ zipWith apply [putLambda,putMu,putPsi,putOmega] [lambdaMesh,muMesh,psiMesh,omegaMesh]

-- | Record the partial results of the LLHD and NB to a CSV at the parameters
-- used in the simulation.
partialEvaluations :: [Observation] -> Simulation ()
partialEvaluations obs = do
  simParams <- asks simulationParameters
  partialEvalsCsv <- asks partialEvaluationOutputCsv
  let partialResults = snd $ verboseLlhdAndNB obs simParams initLlhdState
      records = [show l ++ "," ++ show nb | (l,nb) <- partialResults]
  liftIO $ Prelude.writeFile partialEvalsCsv (intercalate "\n" records)

-- Definition of the complete simulation study.
simulationStudy :: Simulation ()
simulationStudy = do
  bdscodConfig <- bdscodConfiguration
  pEpi <- partialSimulatedEpidemic bdscodConfig
  infConfigs <- asks inferenceConfigurations
  pObs <- zipWithM simulatedObservations infConfigs pEpi
  mapM_ (uncurry evaluateLLHD) pObs
  mapM_ (uncurry estimateLLHD) pObs
  let completeObs = snd $ head pObs
  partialEvaluations completeObs
  return ()

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
