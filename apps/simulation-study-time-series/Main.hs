{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Llhd (llhdAndNB,initLlhdState,verboseLlhdAndNB)
import BDSCOD.Types
import BDSCOD.Utility
import Control.Monad (zipWithM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import Data.List (intercalate, intersperse,nub)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as Unboxed
import qualified Epidemic.BDSCOD as SimBDSCOD
import Epidemic.Types.Events
  ( EpidemicEvent(..)
  , asNewickString
  , eventTime
  , maybeEpidemicTree
  , maybeReconstructedTree
  )
import Epidemic.Types.Parameter
import Epidemic.Types.Population (Person(..), Identifier(..))
import qualified Epidemic.Utility as SimUtil
import GHC.Generics
import Numeric.GSL.Minimization (MinimizeMethod(NMSimplex2), minimizeV)
import Numeric.LinearAlgebra.Data (linspace, toList)
import Numeric.LinearAlgebra.HMatrix
import System.Environment (getArgs)
import System.Random.MWC

-- | These objects define the specifics for an inference evaluation which can be
-- run at differing points in the simulation to understand how differing amounts
-- of data influences the results.
data InferenceConfiguration =
  InferenceConfiguration
    { inferenceTime :: AbsoluteTime
    , reconstructedTreeOutputFiles :: (FilePath, FilePath)
    , observationsOutputCsv :: FilePath
    , llhdOutputCsv :: FilePath
    , pointEstimatesCsv :: FilePath
    }
  deriving (Show, Generic)

-- | These objects describe the evaluation mesh when looking at the likelihood
-- profiles. This is useful because it allows us to provide these details at
-- runtime rather than hardcoding them here.
data LlhdProfileMesh =
  LlhdProfileMesh
  { lpmMeshSize :: Int
  , lpmLambdaBounds :: (Rate,Rate)
  , lpmMuBounds :: (Rate,Rate)
  , lpmPsiBounds :: (Rate,Rate)
  , lpmRhoBounds :: (Probability,Probability)
  , lpmOmegaBounds :: (Rate,Rate)
  , lpmNuBounds :: (Probability,Probability)
  } deriving (Show, Generic)

-- | This object configures the whole evaluation of this program and is to be
-- read in from a suitable JSON file.
data AppConfiguration =
  AppConfiguration
    { simulatedEventsOutputCsv :: FilePath
    , simulationParameters :: Parameters
    , simulationDuration :: TimeDelta
    , simulationSizeBounds :: (Int,Int)
    , inferenceConfigurations :: [InferenceConfiguration]
    , partialEvaluationOutputCsv :: FilePath
    , acLlhdProfileMesh :: LlhdProfileMesh
    }
  deriving (Show, Generic)

instance Json.FromJSON AppConfiguration

instance Json.FromJSON InferenceConfiguration

instance Json.FromJSON LlhdProfileMesh

type Simulation x = ReaderT AppConfiguration (ExceptT String IO) x

-- | This type is used to indicate if parameters are the true ones used in the
-- simulation or estimates parameters.
data ParameterKind
  = SimulationParameters
  | EstimatedParameters
  deriving (Show, Eq)

-- | A BDSCOD simulation configuration based on the parameters in the
-- environment.
bdscodConfiguration = do
  simParams <- asks simulationParameters
  (TimeDelta simDurDouble) <- asks simulationDuration
  let bdscodConfig = SimBDSCOD.configuration (AbsoluteTime simDurDouble) (unpackParameters simParams)
  case bdscodConfig of
    Nothing -> throwError "Could not construct BDSCOD configuration"
    (Just config) -> return config

-- | Simulate the transmission process part of the epidemic making sure that the
-- results are acceptable in terms of the number of observed events before
-- returning a filtration of the events, i.e., the data that was availble at
-- several points in time.
--
-- __NOTE__ the filteration must happen before these are processed into
-- observations since the observations do not accumulate chronologically due to
-- birth events which can occur in the past due to new observations in the
-- present.
partialSimulatedEpidemic bdscodConfig =
  do
    let randomSeed = Unboxed.fromList [1,2,3,4]
    gen <- liftIO $ initialize randomSeed
    simEvents <- liftIO $ SimUtil.simulation' bdscodConfig SimBDSCOD.allEvents gen
    (sizeLowerBound,sizeUpperBound) <- asks simulationSizeBounds
    if length simEvents > sizeLowerBound && length simEvents < sizeUpperBound
      then do infTimes <- map inferenceTime <$> asks inferenceConfigurations
              simEventsCsv <- asks simulatedEventsOutputCsv
              liftIO $ L.writeFile simEventsCsv (Csv.encode simEvents)
              return [filter (\e -> eventTime e <= infTime) simEvents | infTime <- infTimes]
      else do liftIO $ putStrLn "Repeating epidemic simulation..."
              partialSimulatedEpidemic bdscodConfig

-- | Run the actual observation of the simulation and record the results before
-- returning the dataset of observations generated by this epidemic.
simulatedObservations :: InferenceConfiguration
                      -> [EpidemicEvent]
                      -> Simulation (InferenceConfiguration,[Observation])
simulatedObservations infConfig@InferenceConfiguration{..} simEvents = do
  let Just (newickBuilder,newickMetaData) = asNewickString (AbsoluteTime 0, Person (Identifier 1)) =<< maybeReconstructedTree =<< maybeEpidemicTree simEvents
      maybeObs = eventsAsObservations <$> SimBDSCOD.observedEvents simEvents
      (reconNewickTxt,reconNewickCsv) = reconstructedTreeOutputFiles
  case maybeObs of
    (Just obs) -> do liftIO $ L.writeFile reconNewickTxt (BBuilder.toLazyByteString newickBuilder)
                     liftIO $ L.writeFile reconNewickCsv (Csv.encode newickMetaData)
                     liftIO $ L.writeFile observationsOutputCsv (Csv.encode obs)
                     return (infConfig,obs)
    Nothing -> throwError "Failed to simulate observations."


-- | If there is a unique timed value return that. This is used to make it
-- easier to extract the values of timed parameters so that you don't need to
-- store all of them.
uniqueTimedValue :: Eq x => Timed x -> Maybe x
uniqueTimedValue (Timed txs) = case txs of
  [] -> Nothing
  txs' -> let xs = nub [snd tx | tx <- txs']
              isUnique = 1 == length xs
            in if isUnique then Just (head xs) else Nothing

-- | Evaluate the NB posterior approximation of the prevalence for a single
-- point in parameter space and the LLHD over a list of points and write all of
-- the results to CSV.
generateLlhdProfileCurves :: InferenceConfiguration
                          -> [Observation]
                          -> (Parameters,ParameterKind,[Parameters])
                          -> Simulation ()
generateLlhdProfileCurves InferenceConfiguration {..} obs (singleParams, paramKind, evalParams) =
  let comma = BBuilder.charUtf8 ','
      parametersUsed = show paramKind
      parametersUsed' = BBuilder.stringUtf8 parametersUsed
      llhdVals = [fst $ llhdAndNB obs p initLlhdState | p <- evalParams]
      nBValAndParams =
        pure
          ( parametersUsed    -- the kind of parameter considered
          , show $ length obs -- the number of observations
          , snd $ llhdAndNB obs singleParams initLlhdState -- the posterior NB at present
          , show $ getLambda singleParams
          , show $ getMu singleParams
          , show $ getPsi singleParams
          , show <$> uniqueTimedValue $ getRhos singleParams
          , show $ getOmega singleParams
          , show <$> uniqueTimedValue $ getNus singleParams)
      doublesAsString =
        BBuilder.toLazyByteString .
        mconcat .
        intersperse comma . (parametersUsed' :) . map BBuilder.doubleDec
   in do liftIO $ L.appendFile llhdOutputCsv (doublesAsString llhdVals)
         liftIO $ L.appendFile pointEstimatesCsv (Csv.encode nBValAndParams)

-- | Run the evaluation of the log-likelihood profiles on a given set of
-- observations at the parameters used to simulate the data set and write the
-- result to file.
evaluateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
evaluateLLHD infConfig obs = do
  simParams <- asks simulationParameters -- get the actual parameters used to simulate the observations
  llhdProfMesh <- asks acLlhdProfileMesh
  let evalParams = adjustedEvaluationParameters llhdProfMesh simParams
  generateLlhdProfileCurves infConfig obs (simParams,SimulationParameters,evalParams)

-- | Estimate the parameters of the of the model and then evaluate the LLHD
-- profiles and prevalence and append this to the file. The first value of the
-- CSV output now describes which parameters where used to to evaluate these
-- things.
estimateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
estimateLLHD infConfig obs = do
  simParams@(Parameters (_,deathRate,_,_,_,_)) <- asks simulationParameters
  llhdProfMesh <- asks acLlhdProfileMesh
  let schedTimes = scheduledTimes simParams
      mleParams = estimateParameters deathRate schedTimes obs -- get the MLE estimate of the parameters
      evalParams = adjustedEvaluationParameters llhdProfMesh mleParams -- generate a list of evaluation parameters
  generateLlhdProfileCurves infConfig obs (mleParams,EstimatedParameters,evalParams)

-- | Use GSL to estimate the MLE based on the observations given. This uses a
-- simplex method because it seems to be faster and more accurate than the
-- simulated annealing.
--
-- __NOTE__ we fix the death rate to the true value because
-- this is assumed to be known a priori.
estimateParameters :: Rate -> ([AbsoluteTime],[AbsoluteTime]) -> [Observation] -> Parameters
estimateParameters deathRate sched obs =
  let maxIters = 500
      desiredPrec = 1e-3
      initBox = fromList [2,2,2,2,2]
      energyFunc x = negate . fst $ llhdAndNB obs (vectorAsParameters deathRate sched x) initLlhdState
      randInit = fromList [0,0,0,0,0]
      (est,_) = minimizeV NMSimplex2 desiredPrec maxIters initBox energyFunc randInit
  in vectorAsParameters deathRate sched est

-- | Helper function for @estimateParameters@
vectorAsParameters :: Rate -> ([AbsoluteTime],[AbsoluteTime]) -> Vector Double -> Parameters
vectorAsParameters deathRate (rhoTimes, nuTimes) paramVec =
  let [lnR1, lnR2, lnP1, lnR3, lnP2] = toList paramVec
      p1 = invLogit lnP1
      p2 = invLogit lnP2
  in packParameters (exp lnR1, deathRate, exp lnR2, [(t,p1)|t<-rhoTimes], exp lnR3, [(t,p2)|t<-nuTimes])


-- | Recenter the evaluation parametes about the parameters given.
adjustedEvaluationParameters :: LlhdProfileMesh -> Parameters -> [Parameters]
adjustedEvaluationParameters LlhdProfileMesh{..} ps =
  let mesh bounds = toList $ linspace lpmMeshSize bounds
      lambdaMesh = mesh lpmLambdaBounds
      muMesh = mesh lpmMuBounds
      psiMesh = mesh lpmPsiBounds
      rhoProbMesh = mesh lpmRhoBounds
      omegaMesh = mesh lpmOmegaBounds
      nuProbMesh = mesh lpmNuBounds
      (rhoTimes,nuTimes) = scheduledTimes ps
      rhoMesh = [Timed [(t,r) | t <- rhoTimes] | r <- rhoProbMesh]
      nuMesh = [Timed [(t,n) | t <- nuTimes] | n <- nuProbMesh]
      apply f = map (f ps)
      [lPs,mPs,pPs,oPs] = zipWith apply [putLambda,putMu,putPsi,putOmega] [lambdaMesh,muMesh,psiMesh,omegaMesh]
      [rPs,nPs] = zipWith apply [putRhos,putNus] [rhoMesh,nuMesh]
  in concat [lPs,mPs,pPs,rPs,oPs,nPs]

-- | Record the partial results of the LLHD and NB to a CSV at the parameters
-- used in the simulation.
partialEvaluations :: [Observation] -> Simulation ()
partialEvaluations obs = do
  simParams <- asks simulationParameters
  partialEvalsCsv <- asks partialEvaluationOutputCsv
  let partialResults = snd $ verboseLlhdAndNB obs simParams initLlhdState
      records = [show l ++ "," ++ show nb | (l,nb) <- partialResults]
  liftIO $ Prelude.writeFile partialEvalsCsv (intercalate "\n" records)

-- | Definition of the complete simulation study.
simulationStudy :: Simulation ()
simulationStudy = do
  bdscodConfig <- bdscodConfiguration
  liftIO $ putStrLn "\tRunning epidemic simulation"
  pEpi <- partialSimulatedEpidemic bdscodConfig
  infConfigs <- asks inferenceConfigurations
  liftIO $ putStrLn "\tExtracting observations from full simulation"
  pObs <- zipWithM simulatedObservations infConfigs pEpi
  liftIO $ putStrLn "\tEvaluating LLHD on cross-sections about true parameters"
  mapM_ (uncurry evaluateLLHD) pObs
  liftIO $ putStrLn "\tEvaluating LLHD on cross-sections about estimated parameters"
  mapM_ (uncurry estimateLLHD) pObs
  let completeObs = snd $ head pObs
  partialEvaluations completeObs

main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  maybeConfig <- getConfiguration configFilePath
  case maybeConfig of
    Nothing ->
      putStrLn $ "Could not get configuration from file: " ++ configFilePath
    Just config -> do
      putStrLn $ "Succeeded in reading configuration from file: " ++ configFilePath
      result <- runExceptT (runReaderT simulationStudy config)
      case result of
        Left errMsg -> putStrLn errMsg
        Right _ -> return ()

-- | Attempt to read a configuration object from the given filepath.
getConfiguration :: FilePath -> IO (Maybe AppConfiguration)
getConfiguration fp = Json.decode <$> L.readFile fp
