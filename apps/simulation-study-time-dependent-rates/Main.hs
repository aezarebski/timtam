{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import BDSCOD.InhomogeneousBDSLlhd
import BDSCOD.Types
import BDSCOD.Utility
import Control.Monad (liftM, liftM2)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Csv as Csv
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Epidemic
import Epidemic.InhomogeneousBDS
import Epidemic.Types
import Epidemic.Utility
import GHC.Generics

appMessage :: String
appMessage =
  intercalate
    "\n"
    [ ""
    , "Time-Dependent Rates Application"
    , "--------------------------------"
    , ""
    , "This is some place-holder text..."
    , ""
    , "See the file time-dependent-rates-config.json for configuration."
    , ""
    ]


data Config = Config { simulationParameters :: InhomParams
                     , simulationDuration :: Time
                     , simulationEventsFile :: FilePath
                     , inferenceParameters :: [InhomParams]
                     , inferenceLlhdFile :: FilePath
                     } deriving (Show, Generic)

instance Json.FromJSON Config

data LlhdEval = LlhdEval InhomParams LogLikelihood deriving (Show)

writeSimEvents :: Maybe [Epidemic.Event] -> FilePath -> IO ()
writeSimEvents events = case events of
  Nothing -> (\fp -> do putStrLn "ERROR in writing the simulation events"
                        return ())
  Just es -> (\fp -> B.writeFile fp $ Csv.encode es)

writeLlhdVals :: [LlhdEval] -> FilePath -> IO ()
writeLlhdVals llhdEvals fp =
  B.writeFile fp $ B.intercalate "\n" [pack $ show ip ++ show ll | (LlhdEval ip ll) <- llhdEvals]

getSimEvents :: Maybe Time -> Maybe InhomParams -> IO (Maybe [Epidemic.Event])
getSimEvents (Just duration) (Just inhomParams) =
  do conf <- pure $ configuration duration inhomParams
     if isJust conf
       then do sim <- simulation True (fromJust conf) allEvents
               return $ Just sim
       else return Nothing
getSimEvents _ _ =
  do putStrLn "ERROR in running the simulation"
     return Nothing

llhdEvaluations :: [Observation] -> [InhomParams] -> [LlhdEval]
llhdEvaluations obs = map (\ips -> LlhdEval ips (fst $ llhdAndNB obs ips initLlhdState))

readConfigFile :: FilePath -> IO (Maybe Config)
readConfigFile fp = B.readFile fp >>= return . Json.decode



main :: IO ()
main =
  do
    putStrLn appMessage
    config <- readConfigFile "examples/time-dependent-rates-config.json"
    simEvents <- getSimEvents (simulationDuration <$> config) (simulationParameters <$> config)
    simOutputFile <- pure $ simulationEventsFile <$> config
    let
      infParams = inferenceParameters <$> config
      obs = eventsAsObservations <$> simEvents
      llhdVals = (liftM2 llhdEvaluations) obs infParams
      infOutputFile = inferenceLlhdFile <$> config
      in do fromMaybe (return ()) $ (liftM (writeSimEvents simEvents)) simOutputFile
            fromMaybe (return ()) $ (liftM2 writeLlhdVals) llhdVals infOutputFile
