{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import Control.Monad (when)
import qualified Data.ByteString.Lazy as L
import Data.Csv
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import qualified Epidemic.BDSCOD as SimBDSCOD
import qualified Epidemic.Utility as SimUtil
import System.Directory (doesFileExist, removeFile)


llhdsWriteFile fp d ps = case ps of
  [] -> return ()
  (p:ps') -> do {appendFile fp $ output p (llhdAndNB d p initLlhdState);
                                    llhdsWriteFile fp d ps'}
                  where
                    output (x1, x2, x3, [(_,x4)], x5, [(_,x6)]) (x7, x8) =
                      intercalate "," $ map show [x1, x2, x3, x4, x5, x6, x7] ++ [show x8 ++ "\n"]

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
    , "After this has finished, run the following command to generated figures."
    , ""
    , "$ Rscript R/llhd-profiles.R"
    , ""
    , "The figures are in the out/ directory."
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
      simDuration = 3.1
      simLambda = 3.2
      simMu = 0.3
      simPsi = 0.3
      simRho = 0.15
      simRhoTime = 2.6
      simOmega = 0.3
      simNu = 0.15
      simNuTime = 3.0
      simParams = (simLambda, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)])
      infParamss =
        [(l, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)]) | l <- linspace 1.0 8.0 200] ++
        [(simLambda, m, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)]) | m <- linspace 0.01 2.0 200] ++
        [(simLambda, simMu, p, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)]) | p <- linspace 0.01 2.0 200] ++
        [(simLambda, simMu, simPsi, [(simRhoTime,r)], simOmega, [(simNuTime,simNu)]) | r <- linspace 0.01 0.40 400] ++
        [(simLambda, simMu, simPsi, [(simRhoTime,simRho)], o, [(simNuTime,simNu)]) | o <- linspace 0.01 2.0 200] ++
        [(simLambda, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,n)]) | n <- linspace 0.01 0.40 400]
      simConfig = SimBDSCOD.configuration simDuration simParams
   in do happyToContinue1 <- checkFileCanBeOverwritten outputFileSimulation1 :: IO Bool
         happyToContinue2 <- checkFileCanBeOverwritten outputFileSimulation2
         happyToContinue3 <- checkFileCanBeOverwritten outputFileObservations
         happyToContinue4 <- checkFileCanBeOverwritten outputFileLlhdValues
         if not happyToContinue1 && not happyToContinue2 && not happyToContinue3 && not happyToContinue4 && not (isJust simConfig)
           then
             return ()
           else
             do putStrLn appMessage
                simEvents <- SimUtil.simulation False (fromJust simConfig) SimBDSCOD.allEvents
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
                llhdsWriteFile outputFileLlhdValues obs infParamss

checkFileCanBeOverwritten :: FilePath -> IO Bool
checkFileCanBeOverwritten fp =
  do
    putStrLn $ "Can the simulation overwrite " ++ fp ++ " ? [y/n]"
    response <- getChar
    if response == 'n'
      then
        do
          putStrLn "Okay, terminating simulation"
          return False
      else
        if response /= 'y'
          then
            do
              putStrLn "\n\nI doughnut understand..."
              checkFileCanBeOverwritten fp
          else
            do
              putStrLn "Okay, continuing the simulation...\n\n"
              fileExists <- doesFileExist fp
              when fileExists $ removeFile fp
              return True
