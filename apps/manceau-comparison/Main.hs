{-# LANGUAGE DeriveGeneric #-}

module Main where

import BDSCOD.Llhd
import BDSCOD.Types
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (intercalate)
import Epidemic.Types.Parameter
import GHC.Generics (Generic)

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

data Result =
  Result
    { birthRate :: Rate
    , llhd :: LogLikelihood
    }
  deriving (Generic)

instance ToRecord Result

paramsAndLlhd :: [Observation] -> Parameters -> Result
paramsAndLlhd obs p =
  let (ll, _) = llhdAndNB obs p initLlhdState
      (lam, _, _, _, _, _) = p
   in Result lam ll


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
  let csvByteString = encode $ map (paramsAndLlhd observations) parameters
      outputCsv = "out/computed-values.csv"
   in BL.writeFile outputCsv csvByteString
