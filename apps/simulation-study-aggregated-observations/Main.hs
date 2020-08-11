
module Main where

-- import BDSCOD.Conditioning
-- import BDSCOD.Llhd
-- import BDSCOD.Types
-- import BDSCOD.Utility
-- import Control.Monad (liftM, zipWithM)
-- import Control.Monad.Except (ExceptT, runExceptT, throwError)
-- import Control.Monad.Reader (ReaderT, asks, liftIO, runReaderT)
-- import qualified Data.Aeson as Json
-- import qualified Data.ByteString.Builder as BBuilder
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy as L
-- import qualified Data.Csv as Csv
-- import Data.List (intercalate, intersperse)
-- import Data.Maybe (fromJust)
-- import qualified Epidemic.BDSCOD as SimBDSCOD
-- import Epidemic.Types.Events
--   ( EpidemicEvent(..)
--   , asNewickString
--   , eventTime
--   , maybeEpidemicTree
--   , maybeReconstructedTree
--   )
-- import Epidemic.Types.Parameter
-- import Epidemic.Types.Population (Person(..))
-- import qualified Epidemic.Utility as SimUtil
-- import GHC.Generics
-- import Numeric.GSL.Minimization (MinimizeMethod(NMSimplex2), minimizeV)
-- import Numeric.LinearAlgebra.Data (linspace, toList)
-- import Numeric.LinearAlgebra.HMatrix
import System.Environment (getArgs)


main :: IO ()
main = do
  configFilePath <- head <$> getArgs
  putStrLn configFilePath
