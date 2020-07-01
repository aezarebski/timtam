{-# LANGUAGE RecordWildCards #-}

module Main where

import BDSCOD.Llhd
import BDSCOD.Types
import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (intercalate)
import qualified Data.Vector as V
import Epidemic.Types.Parameter

observations :: [(Time, ObservedEvent)]
observations =
  [ (1.0, OBirth)
  , (1.0, OOccurrence)
  , (1.0, OBirth)
  , (1.0, OBirth)
  , (1.0, OSample)
  , (1.0, OOccurrence)
  , (1.0, OCatastrophe 3)
  ]

parameters :: [Parameters]
parameters =
  [ (lam, 1.0, 0.3, [(7,0.5)], 0.6, [])
  | lam <- [1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0]
  ]

printLlhd :: [Observation] -> [Parameters] -> IO ()
printLlhd d [] = return ()
printLlhd d (p:ps) = do
  print . fst $ llhdAndNB d p initLlhdState
  printLlhd d ps

appMessage :: String
appMessage =
  intercalate
    "\n"
    [ ""
    , "Comparison Application"
    , "----------------------"
    , ""
    , "This prints a list of the LLHD values when running the approximation on one of"
    , "the toy datasets from Manceau /et al/ (2020). These values can subsequently be"
    , "used by one of the visualisation scripts to produce a nice figure."
    , ""
    ]

main :: IO ()
main = do
  putStrLn appMessage
  printLlhd observations parameters
