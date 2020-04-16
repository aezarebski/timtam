{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as L
import Data.Csv
import Data.List (intercalate)
import System.Directory (doesFileExist,removeFile)
import qualified Epidemic.BDSCOD as BDSCOD
import BDSCOD.Llhd
import BDSCOD.Utility


llhdsWriteFile fp d ps = case ps of
  [] -> return ()
  (p:ps') -> do {appendFile fp $ output p (llhdAndNB d p initLlhdState);
                                    llhdsWriteFile fp d ps'}
                  where
                    output (x1, x2, x3, [(_,x4)], x5, [(_,x6)]) (x7, x8) =
                      intercalate "," $ map show [x1, x2, x3, x4, x5, x6, x7] ++ [show x8 ++ "\n"]

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
  let outputFileSimulation1 = "simulated-events.txt"
      outputFileSimulation2 = "simulated-all-events.txt"
      outputFileObservations = "simulated-events-observed.txt"
      outputFileLlhdValues = "simulation-study-llhds.csv"
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
      simConfig = BDSCOD.configuration simDuration simParams
   in do happyToContinue1 <- checkFileCanBeOverwritten outputFileSimulation1 :: IO Bool
         happyToContinue2 <- checkFileCanBeOverwritten outputFileSimulation2
         happyToContinue3 <- checkFileCanBeOverwritten outputFileObservations
         happyToContinue4 <- checkFileCanBeOverwritten outputFileLlhdValues
         if (not happyToContinue1) && (not happyToContinue2) && (not happyToContinue3) && (not happyToContinue4)
           then
             return ()
           else
             do putStrLn appMessage
                simEvents <- BDSCOD.simulation simConfig
                Prelude.writeFile outputFileSimulation1 $ intercalate "\n" (map show simEvents)
                L.writeFile outputFileSimulation2 $ encode simEvents
                let obs =
                      eventsAsObservations $
                      BDSCOD.observedEvents simEvents
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
              if fileExists then removeFile fp else return ()
              return True
