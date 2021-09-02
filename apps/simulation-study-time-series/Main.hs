{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Debug.Trace (traceShow)
import Data.Either.Combinators (fromRight, maybeToRight)
import BDSCOD.Llhd (llhdAndNB,initLlhdState, intervalLlhd)
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
import Numeric.Minimisation.Powell (minimise)
import Numeric.LinearAlgebra.Data (linspace, toList)
import Numeric.LinearAlgebra.HMatrix
import System.Environment (getArgs)
import System.Random.MWC
import Data.Word (Word32)
import System.IO.Unsafe (unsafePerformIO)

minimizeV :: Double -> Int -> Vector Double -> (Vector Double -> Double) -> Vector Double -> (Vector Double, Matrix Double)
minimizeV = undefined

-- | Values of this type are used to specify an analysis of the data.
data InferenceConfiguration =
  InferenceConfiguration
    {
    -- | The time in the simulation at which the inference is carried out.
    inferenceTime :: AbsoluteTime
    ,
    -- | A couple of files to write the Newick representation to and the node labels.
    reconstructedTreeOutputFiles :: (FilePath, FilePath)
    ,
    -- | Where to write the sequence of observed events.
    observationsOutputCsv :: FilePath
    ,
    -- | Where to write the likelihood evaluations.
    llhdOutputCsv :: FilePath
    ,
    -- | Where to write the point estimate of prevalence.
    pointEstimatesCsv :: FilePath
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
    , simulationSeed :: Word32
    , inferenceConfigurations :: [InferenceConfiguration]
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
  let bdscodConfig =
        SimBDSCOD.configuration (AbsoluteTime simDurDouble) (unpackParameters simParams)
  case bdscodConfig of
    Just x -> do liftIO $ putStrLn (show simParams)
                 return x
    Nothing -> throwError "Could not construct BDSCOD configuration"

-- | Simulate the transmission process part of the epidemic making sure that the
-- results are acceptable in terms of the number of observed events before
-- returning a filtration of the events, i.e., the data that was availble at
-- several points in time.
--
-- __NOTE__ the filteration must happen before these are processed into
-- observations since the observations do not accumulate chronologically due to
-- birth events which can occur in the past due to new observations in the
-- present.
partialSimulatedEpidemic seedWord bdscodConfig =
  do
    gen <- liftIO $ initialize (Unboxed.fromList [seedWord])
    simEvents <- liftIO $ SimUtil.simulation' bdscodConfig SimBDSCOD.allEvents gen
    (sizeLowerBound,sizeUpperBound) <- asks simulationSizeBounds
    if length simEvents > sizeLowerBound && length simEvents < sizeUpperBound
      then do infTimes <- map inferenceTime <$> asks inferenceConfigurations
              simEventsCsv <- asks simulatedEventsOutputCsv
              liftIO $ L.writeFile simEventsCsv (Csv.encode simEvents)
              return [filter (\e -> eventTime e <= infTime) simEvents | infTime <- infTimes]
      else do liftIO $ putStrLn "Repeating epidemic simulation..."
              partialSimulatedEpidemic (seedWord + 1) bdscodConfig

-- | Run the actual observation of the simulation and record the results before
-- returning the dataset of observations generated by this epidemic.
simulatedObservations :: InferenceConfiguration
                      -> [EpidemicEvent]
                      -> Simulation (InferenceConfiguration,[Observation])
simulatedObservations infConfig@InferenceConfiguration{..} simEvents = do
  let Just (newickBuilder,newickMetaData) =
        do eTree <- maybeEpidemicTree simEvents
           rTree <- maybeReconstructedTree eTree
           asNewickString (AbsoluteTime 0, Person (Identifier 1)) rTree
      maybeObs = eventsAsObservations <$> SimBDSCOD.observedEvents simEvents
      (reconNewickTxt,reconNewickCsv) = reconstructedTreeOutputFiles
  case maybeObs of
    (Just obs) ->
      do
        liftIO $ L.writeFile reconNewickTxt (BBuilder.toLazyByteString newickBuilder)
        liftIO $ L.writeFile reconNewickCsv (Csv.encode newickMetaData)
        liftIO $ L.writeFile observationsOutputCsv (Csv.encode obs)
        return (infConfig,obs)
    _ -> throwError "Failed to simulate observations."


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
recordLlhdCrossSections :: InferenceConfiguration
                          -> [Observation]
                          -> (Parameters,ParameterKind,[Parameters])
                          -> Simulation ()
recordLlhdCrossSections InferenceConfiguration {..} obs (singleParams, paramKind, evalParams) =
  let comma = BBuilder.charUtf8 ','
      parametersUsed = show paramKind
      parametersUsed' = BBuilder.stringUtf8 parametersUsed
      llhdVals = [fromRight (-1e6) $ fst <$> llhdAndNB obs p initLlhdState | p <- evalParams]
      nBValAndParams =
        Csv.encode . (:[]) $
        ( parametersUsed
        , show $ length obs
        , fromRight Zero $ snd <$> llhdAndNB obs singleParams initLlhdState
        , show $ getLambda singleParams
        , show $ getMu singleParams
        , show $ getPsi singleParams
        , show <$> uniqueTimedValue $ getRhos singleParams
        , show $ getOmega singleParams
        , show <$> uniqueTimedValue $ getNus singleParams )
      doublesAsString =
        BBuilder.toLazyByteString .
        mconcat .
        intersperse comma . (parametersUsed' :) . map BBuilder.doubleDec
   in liftIO $
      do
        L.appendFile llhdOutputCsv (doublesAsString llhdVals)
        L.appendFile pointEstimatesCsv nBValAndParams

-- | Evaluate the LLHD function on cross-sections centred at the true values
evaluateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
evaluateLLHD infConfig obs = do
  simParams <- asks simulationParameters -- get the actual parameters used to simulate the observations
  llhdProfMesh <- asks acLlhdProfileMesh -- this specifies the cross section to use.
  let evalParams = crossSectionParameters llhdProfMesh simParams
  recordLlhdCrossSections infConfig obs (simParams,SimulationParameters,evalParams)

-- | Estimate the parameters of the of the model and then evaluate the LLHD
-- profiles and prevalence and append this to the file. The first value of the
-- CSV output now describes which parameters where used to to evaluate these
-- things.
estimateLLHD :: InferenceConfiguration -> [Observation] -> Simulation ()
estimateLLHD infConfig obs = do
  simParams@(Parameters (_,deathRate,_,_,_,_)) <- asks simulationParameters
  llhdProfMesh <- asks acLlhdProfileMesh
  liftIO $ putStrLn $ "\t\tEstimating parameters using " ++ show (length obs) ++ " observations..."
  let schedTimes = scheduledTimes simParams
      mleParams = estimateParameters deathRate schedTimes obs -- get the MLE estimate of the parameters
      evalParams = crossSectionParameters llhdProfMesh mleParams
  recordLlhdCrossSections infConfig obs (mleParams,EstimatedParameters,evalParams)

-- | Estimate of the MLE. This uses a simplex method.
--
-- __NOTE__ the ugly case statement means that this should work both with and
-- without scheduled observations.
--
-- __NOTE__ we fix the death rate to the true value because
-- this is assumed to be known a priori.
--
estimateParameters :: Rate -> ([AbsoluteTime],[AbsoluteTime]) -> [Observation] -> Parameters
estimateParameters deathRate sched obs =
  let energyFunc v2p x =
        negate $ fromRight (-1e6) (fst <$> llhdAndNB obs (v2p x) initLlhdState)
      mini rI v2p = minimise (energyFunc v2p) rI
  in case sched of
       -- there are no scheduled observations
       ([],[]) ->
         let randInit = [-1.5,-2.5,-2.5] -- initial point to start
             vec2Param vec =
               let [lnR1, lnR2, lnR3] = vec
               in packParameters ( exp lnR1
                                 , deathRate
                                 , exp lnR2
                                 , []
                                 , exp lnR3
                                 , [])
             Right (est,_,grad) = mini randInit vec2Param
         in traceShow grad $ vec2Param est
       -- there is at least one scheduled observation
       (rhoTs, nuTs) ->
         let randInit = [-1.5,-2.5,-1,-2.5,-1]
             vec2Param vec =
               let [lnR1, lnR2, logitP1, lnR3, logitP2] = vec
                   rhoVal = invLogit logitP1
                   nuVal = invLogit logitP2
                   timed v ts = zip ts (repeat v)
               in packParameters ( exp lnR1
                                 , deathRate
                                 , exp lnR2
                                 , timed rhoVal rhoTs
                                 , exp lnR3
                                 , timed nuVal nuTs)
             Right (est,_,_) = mini randInit vec2Param
         in vec2Param est


-- | List of parameters required to plot the cross sections.
crossSectionParameters :: LlhdProfileMesh -> Parameters -> [Parameters]
crossSectionParameters LlhdProfileMesh{..} ps =
  let mesh = toList . linspace lpmMeshSize
      lambdaMesh = mesh lpmLambdaBounds
      muMesh = mesh lpmMuBounds
      psiMesh = mesh lpmPsiBounds
      rhoProbMesh = mesh lpmRhoBounds
      omegaMesh = mesh lpmOmegaBounds
      nuProbMesh = mesh lpmNuBounds
      (rhoTimes,nuTimes) = scheduledTimes ps
      rhoMesh = [Timed [(t,r) | t <- rhoTimes] | r <- rhoProbMesh]
      nuMesh = [Timed [(t,n) | t <- nuTimes] | n <- nuProbMesh]
      -- for each dimension, construct a list of parameter values with the
      -- values from the mesh
      apply f = map (f ps)
      [lPs,mPs,pPs,oPs] =
        zipWith apply [putLambda,putMu,putPsi,putOmega] [lambdaMesh,muMesh,psiMesh,omegaMesh]
      [rPs,nPs] = zipWith apply [putRhos,putNus] [rhoMesh,nuMesh]
  in concat $
     if (rhoTimes,nuTimes) /= ([],[])
     then [lPs,mPs,pPs,rPs,oPs,nPs]
     else [lPs,mPs,pPs,oPs]

-- | Definition of the complete simulation study.
simulationStudy :: Simulation ()
simulationStudy = do
  bdscodConfig <- bdscodConfiguration
  liftIO $ putStrLn "\tRunning epidemic simulation"
  simSeed <- asks simulationSeed
  pEpi <- partialSimulatedEpidemic simSeed bdscodConfig
  infConfigs <- asks inferenceConfigurations
  liftIO $ putStrLn "\tExtracting observations from full simulation"
  pObs <- zipWithM simulatedObservations infConfigs pEpi
  liftIO $ putStrLn "\tEvaluating LLHD on cross-sections about true parameters"
  mapM_ (uncurry evaluateLLHD) pObs
  liftIO $ putStrLn "\tEvaluating LLHD on cross-sections about estimated parameters"
  mapM_ (uncurry estimateLLHD) pObs



main' :: IO ()
main' = do
  let configFilePath = "./examples/simulation-study-time-series/ts-config.json"
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
