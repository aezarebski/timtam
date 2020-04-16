{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (intercalate)
import qualified Data.Vector as V
import BDSCOD.Llhd

observations =
  [ (1.0, Birth)
  , (1.0, Occurrence)
  , (1.0, Birth)
  , (1.0, Birth)
  , (1.0, Sample)
  , (1.0, Occurrence)
  , (1.0, Catastrophe 3)
  ]

parameters =
  [ (lam, 1.0, 0.3, [(7,0.5)], 0.6, [])
  | lam <- [1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0]
  ]

printLlhd d [] = return ()
printLlhd d (p:ps) = do
  print . fst $ llhdAndNB d p initLlhdState
  printLlhd d ps

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
