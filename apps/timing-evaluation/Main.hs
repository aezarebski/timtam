{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import Criterion.Main
import Criterion.Types
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (intercalate)
import Data.Maybe (fromJust,isJust)
import qualified Epidemic.BDSCOD as BDSCOD
import Epidemic.Types.Parameter
import qualified Epidemic.Utility as SimUtil
import Statistics.Types (cl95)

type SimParams = (Rate, Rate, Rate, [(Time, Probability)], Rate, [(Time, Probability)])


-- llhdsWriteFile fp d ps = case ps of
--   [] -> return ()
--   (p:ps') -> do {appendFile fp $ output p (llhdAndNB d p initLlhdState);
--                                     llhdsWriteFile fp d ps'}
--                   where
--                     output (x1, x2, x3, [(_,x4)], x5, [(_,x6)]) (x7, x8) =
--                       intercalate "," $ map show [x1, x2, x3, x4, x5, x6, x7] ++ [show x8 ++ "\n"]



appMessage :: String
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


paramsAndRandomObs :: IO (SimParams, Maybe [Observation])
paramsAndRandomObs =
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
      -- This commented line toggles whether to fix the seed.
   -- in do simEvents <- SimUtil.simulationWithSystemRandom True simConfig BDSCOD.allEvents
   in do simEvents <- SimUtil.simulation True simConfig BDSCOD.allEvents
         simObs <- pure $ eventsAsObservations <$> (BDSCOD.observedEvents simEvents)
         putStrLn $ "The number of observations is: " ++ (show . length) simObs
         return (simParams, simObs)

evalLLHD :: String -> (SimParams, [Observation]) -> Benchmark
evalLLHD bname (params, obs) =
  bench bname $ nf (\o -> fst $ llhdAndNB o params initLlhdState) obs

-- | Default benchmarking configuration.
myConfig :: Config
myConfig = Config {
      confInterval = cl95
    , timeLimit    = 5
    , resamples    = 1000
    , regressions  = []
    , rawDataFile  = Nothing
    , reportFile   = Nothing
    , csvFile      = Nothing
    , jsonFile     = Just "out/criterion-report.json"
    , junitFile    = Nothing
    , verbosity    = Quiet
    , template     = "default"
    }

main :: IO ()
main = do
  (ps, maybeObs) <- paramsAndRandomObs
  if isJust maybeObs
    then
    let justObs = fromJust maybeObs
     in do
      B.writeFile "out/simulated-observations.json" $ encode justObs
      defaultMainWith myConfig
        [ bgroup
          "llhd-evaluation"
          [ bench "simulated-data" $
            nf (\p -> fst $ llhdAndNB justObs p initLlhdState) ps
          ]
        ]
    else
    putStrLn "Simulation failed."
  -- do putStrLn appMessage
  --    defaultMain [ env rParamsAndObs $ \ ~(ps,os) -> bench "01" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "02" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "03" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "04" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "05" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "06" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "07" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "08" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "09" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                , env rParamsAndObs $ \ ~(ps,os) -> bench "10" $ nf (\p -> fst $ llhdAndNB os p initLlhdState) ps
  --                ]
