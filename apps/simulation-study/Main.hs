{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- import BDSCOD.Conditioning
import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import qualified Data.Aeson as Json
import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
import qualified Data.Csv as Csv
import Data.List (intercalate, intersperse)
import Data.Maybe
import Data.Map.Strict (Map(),fromList,toList)
import qualified Epidemic.BDSCOD as SimBDSCOD
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import qualified Epidemic.Utility as SimUtil
import GHC.Generics
import System.Environment (getArgs)

-- | Type to refer to a parameter without neededing to use a string.
data ParameterName
  = ParamLambda
  | ParamMu
  | ParamPsi
  | ParamRho
  | ParamOmega
  | ParamNu
  deriving (Show, Eq, Ord)

type ProfileParameters = Map ParameterName [Parameters]

-- condLlhdAndNB :: [Observation] -> Parameters -> Time -> Bool -> (LogLikelihood, NegativeBinomial)
-- condLlhdAndNB obs params@(Parameters (birthRate,deathRate,samplingRate,_,occRate,_)) duration cond =
--   let (l,nb) = llhdAndNB obs params initLlhdState
--       lnProbObs = log $ 1 - probabilityUnobserved (birthRate,deathRate,samplingRate+occRate) duration
--    in (if cond then l - lnProbObs else l,nb)

-- | Evalue the LLHD function with or without conditioning upon observing the
-- process at each of the parameter values given and write the results to file.
llhdsWriteFile :: FilePath -> [Observation] -> ProfileParameters -> TimeDelta -> Bool -> IO ()
llhdsWriteFile fp d psMap duration conditionLlhd =
   mapM_ (\(pName,pVals) -> llhdsWriteFile' fp d pName pVals duration conditionLlhd) (toList psMap)

llhdsWriteFile' :: FilePath -> [Observation] -> ParameterName -> [Parameters] -> TimeDelta -> Bool -> IO ()
llhdsWriteFile' fp d paramName ps duration conditionLlhd =
  case ps of
    [] -> return ()
    (p:ps') -> do
      -- let x = condLlhdAndNB d p duration conditionLlhd
      let x = unsafeLlhdAndNB d p initLlhdState
      appendFile fp $ output p x
      llhdsWriteFile' fp d paramName ps' duration conditionLlhd
      where output (Parameters (x1, x2, x3, Timed ((_, x4):_), x5, Timed ((_, x6):_))) (x7, x8) =
              intercalate "," . ((show paramName):) $
              map show [x1, x2, x3, x4, x5, x6, x7] ++ [show x8 ++ "\n"]

appMessage :: String
appMessage =
  intercalate
    "\n"
    [ ""
    , "Simulation Application"
    , "----------------------"
    , ""
    , "This simulates a birth-death-sampling-occurrence-disaster process, computes the"
    , "observations from it and then prints the approximate likelihood profile about"
    , "the true parameters used in the simulation. These values can be used by the"
    , "visualisation scripts to produce a nice figure."
    , ""
    ]

linspace :: Double -> Double -> Integer -> [Double]
linspace x1 x2 n = [x1 + fromIntegral i * delta | i <- [0 .. (n - 1)]]
  where
    delta = (x2 - x1) / (fromIntegral n - 1)

data SimStudyParams =
  SimStudyParams
    { outputEventsFile :: FilePath
    , outputEventsCsv :: FilePath
    , outputObservationsFile :: FilePath
    , outputLlhdFile :: FilePath
    , simDuration :: TimeDelta
    , simLambda :: Rate
    , simMu :: Rate
    , simPsi :: Rate
    , simRho :: Probability
    , simRhoTimes :: [AbsoluteTime]
    , simOmega :: Rate
    , simNu :: Probability
    , simNuTimes :: [AbsoluteTime]
    }
  deriving (Show, Generic)

instance Json.FromJSON SimStudyParams

readConfigFile :: FilePath -> IO (Maybe SimStudyParams)
readConfigFile fp = Json.decode <$> L.readFile fp

inferenceParameters :: SimStudyParams -> ProfileParameters
inferenceParameters SimStudyParams{..} =
  fromList [lambdaParams,muParams,psiParams,rhoParams,omegaParams,nuParams]
  where
    rateRange radius r = filter (>0) $ linspace (r - radius) (r + radius) 200
    probRange = linspace 0.01 0.99 300
    lambdaParams = (ParamLambda, [(Parameters (l, simMu, simPsi, Timed [(rt,simRho) | rt <- simRhoTimes], simOmega, Timed [(nt,simNu) | nt <- simNuTimes])) | l <- rateRange 0.20 simLambda])
    muParams = (ParamMu, [(Parameters (simLambda, m, simPsi, Timed [(rt,simRho) | rt <- simRhoTimes], simOmega, Timed [(nt,simNu) | nt <- simNuTimes])) | m <- rateRange 0.20 simMu])
    psiParams = (ParamPsi, [(Parameters (simLambda, simMu, p, Timed [(rt,simRho) | rt <- simRhoTimes], simOmega, Timed [(nt,simNu) | nt <- simNuTimes])) | p <- rateRange 0.1 simPsi])
    rhoParams = (ParamRho, [(Parameters (simLambda, simMu, simPsi, Timed [(rt,r) | rt <- simRhoTimes], simOmega, Timed [(nt,simNu) | nt <- simNuTimes])) | r <- probRange])
    omegaParams = (ParamOmega, [(Parameters (simLambda, simMu, simPsi, Timed [(rt,simRho) | rt <- simRhoTimes], o, Timed [(nt,simNu) | nt <- simNuTimes])) | o <- rateRange 0.10 simOmega])
    nuParams = (ParamNu, [(Parameters (simLambda, simMu, simPsi, Timed [(rt,simRho) | rt <- simRhoTimes], simOmega, Timed [(nt,n) | nt <- simNuTimes])) | n <- probRange])

main :: IO ()
main = do
  (configFilePath:_) <- getArgs
  config <- readConfigFile configFilePath
  if isJust config
    then putStrLn "Configuration file read successfully!"
    else putStrLn "Could not read configuration JSON!!!"
  let simStudyParams@(SimStudyParams{..}) = fromJust config
      simParams = (simLambda, simMu, simPsi, [(rt,simRho) | rt <- simRhoTimes], simOmega, [(nt,simNu) | nt <- simNuTimes])
      infParamss = inferenceParameters simStudyParams
      (TimeDelta simDurDouble) = simDuration
      simConfig = SimBDSCOD.configuration (AbsoluteTime simDurDouble) simParams
      conditionUponObservation = True
   in if isNothing simConfig
      then
        return ()
      else
        do putStrLn appMessage
           simEvents <- SimUtil.simulation True (fromJust simConfig) SimBDSCOD.allEvents
           Prelude.writeFile outputEventsFile $ intercalate "\n" (map show simEvents)
           L.writeFile outputEventsCsv $ Csv.encode simEvents
             -- Generate the Newick representations so we can have a tree view of the whole simulation.
           let maybeEpiTree = maybeEpidemicTree simEvents
           let Just (newickBuilder,newickMetaData) = asNewickString (AbsoluteTime 0, Person (Identifier 1)) =<< maybeEpiTree
           L.writeFile "demo-newick-string-epitree.txt" $ BBuilder.toLazyByteString newickBuilder
           L.writeFile "demo-newick-metadata-epitree.csv" $ Csv.encode newickMetaData
           let obs =
                 eventsAsObservations <$>
                 SimBDSCOD.observedEvents simEvents
               numSimEvents = length simEvents
               numObs = length obs
             -- Generate the Newick representations of the observed data
           let Just (newickBuilder',newickMetaData') = asNewickString (AbsoluteTime 0, Person (Identifier 1)) =<< maybeReconstructedTree =<< maybeEpiTree
           L.writeFile "demo-newick-string-recontree.txt" $ BBuilder.toLazyByteString newickBuilder'
           L.writeFile "demo-newick-metadata-recontree.csv" $ Csv.encode newickMetaData'
           fromJust $ Prelude.writeFile outputObservationsFile . intersperse '\n' <$> (fmap show obs)
           putStrLn $ "Number of events in the simulation: " ++ show numSimEvents
           putStrLn $ "Number of events in the dataset: " ++ show numObs
           if isJust obs
             then llhdsWriteFile outputLlhdFile (fromJust obs) infParamss simDuration conditionUponObservation
             else putStrLn "The observations are nothing!"
