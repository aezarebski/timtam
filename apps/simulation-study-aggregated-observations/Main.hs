{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

-- import BDSCOD.Conditioning
-- import BDSCOD.Llhd
import BDSCOD.Types
-- import BDSCOD.Utility
-- import Control.Monad (liftM, zipWithM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
import qualified Data.Aeson as Json
-- import qualified Data.ByteString.Builder as BBuilder
import qualified Data.ByteString.Lazy as L
-- import qualified Data.Csv as Csv
-- import Data.List (intercalate, intersperse)
-- import Data.Maybe (fromJust)
-- import qualified Epidemic.BDSCOD as SimBDSCOD
import Epidemic.Types.Events ()
--   ( EpidemicEvent(..)
--   , asNewickString
--   , eventTime
--   , maybeEpidemicTree
--   , maybeReconstructedTree
--   )
import Epidemic.Types.Parameter (Time)
-- import Epidemic.Types.Population (Person(..))
-- import qualified Epidemic.Utility as SimUtil
import GHC.Generics
-- import Numeric.GSL.Minimization (MinimizeMethod(NMSimplex2), minimizeV)
-- import Numeric.LinearAlgebra.Data (linspace, toList)
-- import Numeric.LinearAlgebra.HMatrix
import System.Environment (getArgs)

-- | These objects define the specifics of the evaluation of LLHD profiles. If a
-- point estimate is given, then that is the central point of the profiles,
-- otherwise the parameters are estimated first. In every case the natural death
-- rate is set to the true values of the simulation. The boolean value is true
-- when the unscheduled observations are to be aggregated, otherwise they are
-- left as they are.
data InferenceConfiguration =
  InferenceConfiguration
    { reconstructedTreeOutputFiles :: (FilePath, FilePath)
    , observationsOutputCsv :: FilePath
    , llhdOutputCsv :: FilePath
    , pointEstimatesCsv :: FilePath
    , maybePointEstimate :: Maybe Parameters
    , aggregateObservations :: Bool
    }
  deriving (Show, Generic)

instance Json.FromJSON InferenceConfiguration

-- | This object configures the whole evaluation of this program and is to be
-- read in from a suitable JSON file.
data Configuration =
  Configuration
    { simulatedEventsOutputCsv :: FilePath
    , simulationParameters :: Parameters
    , simulationDuration :: Time
    , simulationSizeBounds :: (Int,Int)
    , inferenceConfigurations :: [InferenceConfiguration]
    }
  deriving (Show, Generic)

instance Json.FromJSON Configuration

type Simulation x = ReaderT Configuration (ExceptT String IO) x

-- | This type is used to indicate if parameters are the true ones used in the
-- simulation or estimates parameters.
data AnnotatedParameter
  = TrueParameters Parameters
  | EstimatedParameters Parameters
  deriving (Show, Eq)

-- | This is the main entry point to the actual simulation study. Since this is
-- within the simulation monad it has access to all the configuration data and
-- can perform IO.
simulationStudy :: Simulation x
simulationStudy = undefined

main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  maybeConfig <- getConfiguration configFilePath
  case maybeConfig of
    Nothing ->
      putStrLn $ "Could not get configuration from file: " ++ configFilePath
    Just config -> do
      result <- runExceptT (runReaderT simulationStudy config)
      case result of
        Left errMsg -> putStrLn errMsg
        Right _ -> return ()

getConfiguration :: FilePath -> IO (Maybe Configuration)
getConfiguration fp = Json.decode <$> L.readFile fp
