{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import BDSCOD.Conditioning
import Epidemic.Types
import qualified Data.ByteString.Lazy as L
import Data.Csv
import Data.List (intercalate)
import Data.Maybe
import qualified Epidemic.BDSCOD as SimBDSCOD
import qualified Epidemic.Utility as SimUtil


condLlhdAndNB :: [Observation] -> Parameters -> Time -> Bool -> (LogLikelihood, NegativeBinomial)
condLlhdAndNB obs params@(birthRate,deathRate,samplingRate,_,occRate,_) duration cond =
  let (l,nb) = llhdAndNB obs params initLlhdState
      lnProbObs = log $ 1 - probabilityUnobserved (birthRate,deathRate,samplingRate+occRate) duration
   in (if cond then l - lnProbObs else l,nb)

-- | Evalue the LLHD function with or without conditioning upon observing the
-- process at each of the parameter values given and write the results to file.
llhdsWriteFile :: FilePath -> [Observation] -> [Parameters] -> Time -> Bool -> IO ()
llhdsWriteFile fp d ps duration conditionLlhd =
  case ps of
    [] -> return ()
    (p:ps') -> do
      let x = condLlhdAndNB d p duration conditionLlhd
      appendFile fp $ output p x
      llhdsWriteFile fp d ps' duration conditionLlhd
      where output (x1, x2, x3, [(_, x4)], x5, [(_, x6)]) (x7, x8) =
              intercalate "," $
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

main :: IO ()
main =
  let outputFileSimulation1 = "out/simulated-events.txt"
      outputFileSimulation2 = "out/simulated-all-events.txt"
      outputFileObservations = "out/simulated-events-observed.txt"
      outputFileLlhdValues = "out/simulation-study-llhds.csv"
      simDuration = 6.0
      simLambda = 1.5
      simMu = 0.3
      simPsi = 0.3
      simRho = 0.15
      simRhoTime = 3.0
      simOmega = 0.3
      simNu = 0.15
      simNuTime = 4.0
      simParams = (simLambda, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)])
      infParamss =
        [(l, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)]) | l <- linspace 1.0 8.0 200] ++
        [(simLambda, m, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)]) | m <- linspace 0.01 2.0 200] ++
        [(simLambda, simMu, p, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)]) | p <- linspace 0.01 2.0 200] ++
        [(simLambda, simMu, simPsi, [(simRhoTime,r)], simOmega, [(simNuTime,simNu)]) | r <- linspace 0.01 0.40 400] ++
        [(simLambda, simMu, simPsi, [(simRhoTime,simRho)], o, [(simNuTime,simNu)]) | o <- linspace 0.01 2.0 200] ++
        [(simLambda, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,n)]) | n <- linspace 0.01 0.40 400]
      simConfig = SimBDSCOD.configuration simDuration simParams
      conditionUponObservation = True
   in if isNothing simConfig
      then
        return ()
      else
        do putStrLn appMessage
           simEvents <- SimUtil.simulation True (fromJust simConfig) SimBDSCOD.allEvents
           Prelude.writeFile outputFileSimulation1 $ intercalate "\n" (map show simEvents)
           L.writeFile outputFileSimulation2 $ encode simEvents
           let obs =
                 eventsAsObservations $
                 SimBDSCOD.observedEvents simEvents
               numSimEvents = length simEvents
               numObs = length obs
           Prelude.writeFile outputFileObservations $ intercalate "\n" (map show obs)
           putStrLn $ "Number of events in the simulation: " ++ show numSimEvents
           putStrLn $ "Number of events in the dataset: " ++ show numObs
           llhdsWriteFile outputFileLlhdValues obs infParamss simDuration conditionUponObservation
