{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Criterion.Main
import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Epidemic
import Epidemic.Types
import qualified Epidemic.BDSCOD as BDSCOD
import qualified Epidemic.Utility as SimUtil
import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility


type SimParams = (Rate, Rate, Rate, [(Time, Probability)], Rate, [(Time, Probability)])


-- llhdsWriteFile fp d ps = case ps of
--   [] -> return ()
--   (p:ps') -> do {appendFile fp $ output p (llhdAndNB d p initLlhdState);
--                                     llhdsWriteFile fp d ps'}
--                   where
--                     output (x1, x2, x3, [(_,x4)], x5, [(_,x6)]) (x7, x8) =
--                       intercalate "," $ map show [x1, x2, x3, x4, x5, x6, x7] ++ [show x8 ++ "\n"]




appMessage =
  intercalate
    "\n"
    [""
    , "Time Complexity Application"
    , "---------------------------"
    , ""
    , "This application simulates several data sets of varying size and then evaluates"
    , "the LLHD function on them while measuring how long it takes. There is then an R"
    , "script to generate a visualisation and estimate the time complexity of the LLHD"
    , "function."
    , ""
    ]


rParamsAndObs :: IO (SimParams,[Observation])
rParamsAndObs =
  let simDuration = 3.0
      simLambda = 3.2
      simMu = 0.3
      simPsi = 0.3
      simRho = 0.15
      simRhoTime = 10.0
      simOmega = 0.3
      simNu = 0.15
      simNuTime = 20.0
      simParams = (simLambda, simMu, simPsi, [(simRhoTime,simRho)], simOmega, [(simNuTime,simNu)])
      simConfig = fromJust $ BDSCOD.configuration simDuration simParams
   in do simEvents <- SimUtil.simulationWithSystemRandom True simConfig BDSCOD.allEvents
         simObs <- pure $ eventsAsObservations (BDSCOD.observedEvents simEvents)
         putStrLn $ "The number of observations is: " ++ (show . length) simObs
         return (simParams, simObs)

evalLLHD :: String -> (SimParams, [Observation]) -> Benchmark
evalLLHD bname (params, obs) =
  bench bname $ nf (\o -> fst $ llhdAndNB o params initLlhdState) obs


main :: IO ()
main =
  do putStrLn appMessage
     defaultMain [ env rParamsAndObs $ \ ~(ps,os) -> bench "01" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "02" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "03" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "04" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "05" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "06" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "07" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "08" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "09" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 , env rParamsAndObs $ \ ~(ps,os) -> bench "10" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
                 ]
