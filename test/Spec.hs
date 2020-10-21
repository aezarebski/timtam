
import BDSCOD.Conditioning
import qualified BDSCOD.InhomogeneousBDSLlhd as InhomBDSLlhd
import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import Control.Monad (replicateM)
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector.Unboxed as Unboxed
import qualified Epidemic as EpiSim
import qualified Epidemic.BirthDeathSampling as EpiBDS
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import qualified Epidemic.Utility as EpiUtil
import Numeric.GSL.SimulatedAnnealing
import Numeric.LinearAlgebra.HMatrix
import System.Random.MWC
import Test.Hspec

-- | Check if @y@ is withing @delta@ of @x@
withinDeltaOf :: (Ord a, Num a)
              => a -- ^ delta
              -> a -- ^ y
              -> a -- ^ x
              -> Bool
withinDeltaOf delta y x = abs (y - x) < delta

-- | Approximate the derivative of @f@ at @x@ with a step of size @h@.
finiteDifference :: Fractional a
                 => a         -- ^ h
                 -> (a -> a)  -- ^ f
                 -> a         -- ^ x
                 -> a
finiteDifference h f x = (f (x+h) - f (x-h)) / (2*h)

testNbPGF = do
  describe "Test nbPGF: 1" $ do
    it "known value of PGF is correct 1" $
      nbPGF Zero 0.0 `shouldBe` 1

    it "known value of PGF is correct 2" $
      nbPGF Zero 0.5 `shouldBe` 1

    it "known value of PGF is correct 3" $
      nbPGF Zero 1.0 `shouldBe` 1

    it "known value of PGF is correct 4" $
      nbPGF (NegBinom 1 0.5) 0.0 `shouldBe` 0.5

    it "known value of PGF is correct 5" $
      nbPGF (NegBinom 1 0.5) 0.5 `shouldSatisfy` (withinDeltaOf 1e-6 (2.0 / 3.0))

    it "known value of PGF is correct 5" $
      nbPGF (NegBinom 1 0.5) 1.0 `shouldBe` 1.0

    it "PGF partial derivative seems correct 1" $
      nbPGF' (NegBinom 1 0.5) 1.0 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-3 (\x -> nbPGF (NegBinom 1 0.5) x) 1.0))

    it "PGF partial derivative seems correct 2" $
      nbPGF' (NegBinom 1 0.5) 0.5 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-3 (\x -> nbPGF (NegBinom 1 0.5) x) 0.5))

    it "PGF partial derivative seems correct 3" $
      nbPGF' (NegBinom 1 0.5) 0.0 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-3 (\x -> nbPGF (NegBinom 1 0.5) x) 0.0))

    it "PGF second partial derivative seems correct 1" $
      nbPGF'' (NegBinom 1 0.5) 1.0 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-3 (\x -> nbPGF' (NegBinom 1 0.5) x) 1.0))

    it "PGF second partial derivative seems correct 2" $
      nbPGF'' (NegBinom 1 0.5) 0.5 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\x -> nbPGF' (NegBinom 1 0.5) x) 0.5))

    it "PGF second partial derivative seems correct 3" $
      nbPGF'' (NegBinom 1 0.5) 0.0 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\x -> nbPGF' (NegBinom 1 0.5) x) 0.0))

  describe "Test nbPGF: 2" $ do
    it "test pochhammer and logPochhammer" $ do
      pochhammer 2 5 > 2 `shouldBe` True
      let pochhammersWorking (a,b) = withinDeltaOf 1e-5 (log $ pochhammer a b) (logPochhammer a b)
      all pochhammersWorking [(a,b) | a <- [1..10], b <- [1..10], a <= b] `shouldBe` True
      all pochhammersWorking [(a+0.1,b) | a <- [1..10], b <- [1..10], a <= b] `shouldBe` True

    it "test nbPGFdash and logNbPGFdash" $ do
      let nbPGFdashWorking (j,r,p,z) = withinDeltaOf 1e-5 (log $ nbPGFdash j (NegBinom r p) z) (logNbPGFdash j (NegBinom r p) z)
      all nbPGFdashWorking [(j,r,p,z) | j <- [2..50], r <- [2..50], p <- [0.1,0.3,0.5,0.7,0.9], z <- [0.1,0.3,0.5,0.7,0.9]] `shouldBe` True

    it "test nbPGF and logNbPGF" $ do
      let nbPGFWorking (r,p,z) = withinDeltaOf 1e-5 (log $ nbPGF (NegBinom r p) z) (logNbPGF (NegBinom r p) z)
      all nbPGFWorking [(r,p,z) | r <- [2..50], p <- [0.1,0.3,0.5,0.7,0.9], z <- [0.1,0.3,0.5,0.7,0.9]] `shouldBe` True

    it "test nbPGF' and logNbPGF'" $ do
      let nbPGFWorking (r,p,z) = withinDeltaOf 1e-5 (log $ nbPGF' (NegBinom r p) z) (logNbPGF' (NegBinom r p) z)
      all nbPGFWorking [(r,p,z) | r <- [2..50], p <- [0.1,0.3,0.5,0.7,0.9], z <- [0.1,0.3,0.5,0.7,0.9]] `shouldBe` True

    it "test nbPGF'' and logNbPGF''" $ do
      let nbPGFWorking (r,p,z) = withinDeltaOf 1e-5 (log $ nbPGF'' (NegBinom r p) z) (logNbPGF'' (NegBinom r p) z)
      all nbPGFWorking [(r,p,z) | r <- [2..50], p <- [0.1,0.3,0.5,0.7,0.9], z <- [0.1,0.3,0.5,0.7,0.9]] `shouldBe` True

testp0 = do
  describe "Test p0" $ do
    it "Initial condition 1" $ do
      p0 (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 0.0001 0.2 `shouldSatisfy` (withinDeltaOf 1e-3 0.2)
      p0 (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 0.0001 1.0 `shouldSatisfy` (withinDeltaOf 1e-3 1.0)

    it "Evolution 1" $
      let a = p0 (Parameters (2.3, 0.5, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 0.2
          b = p0 (Parameters (2.3, 0.5, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.1 0.2
       in a > b `shouldBe` True

    it "Long term condition 1" $ do
      p0 (Parameters (0.001, 5.9, 0.001, Timed [(1000, 0.001)], 0.01, Timed [])) 100.0 0.2 `shouldSatisfy` (withinDeltaOf 1e-2 1.0)
      p0 (Parameters (0.001, 5.9, 0.001, Timed [(1000, 0.001)], 0.01, Timed [])) 100.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-2 1.0)

    it "Long term condition 2" $ do
      p0 (Parameters (10.1, 0.1, 9.0, Timed [(1000, 1.0)], 0.01, Timed [])) 100.0 0.2 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)
      p0 (Parameters (10.1, 0.1, 9.0, Timed [(1000, 1.0)], 0.01, Timed [])) 100.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)

    it "First partial derivative seems correct 1" $ do
      p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> p0 (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 z) 0.7))
      p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0 (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 z) 0.9))
      p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0 (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 z) 0.7))
      p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0 (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 z) 0.9))

    it "Second partial derivative seems correct 1" $ do
      p0'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 z) 0.7))
      p0'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 z) 0.9))
      p0'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 z) 0.7))
      p0'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 z) 0.9))


testRr = do
  describe "Test rr" $ do
    it "Initial condition 1" $ do
      rr (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 0.0001 0.2 `shouldSatisfy` (withinDeltaOf 1e-3 (0.8/0.8))
      rr (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 0.0001 0.9 `shouldSatisfy` (withinDeltaOf 1e-3 (0.1/0.1))

    it "Evolution 1" $
      let a = rr (Parameters (0.001, 0.001, 0.001, Timed [(1000,0.9)], 0.001, Timed [])) 0.01 0.3
          b = rr (Parameters (0.001, 0.001, 0.001, Timed [(1000,0.9)], 0.001, Timed [])) 0.01 0.2
       in a > b `shouldBe` True

    it "Long term condition 1" $ do
      rr (Parameters (3, 0.9, 0.01, Timed [(1000,0.1)], 0.1, Timed [])) 100.0 0.2 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)
      rr (Parameters (3, 0.9, 0.01, Timed [(1000,0.1)], 0.1, Timed [])) 100.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)

    it "Long term condition 2" $
      let a = rr (Parameters (3, 0.1, 0.01, Timed [(1000,0.1)], 0.1, Timed [])) 1.1 (0.9/(1-0.9))
          b = rr (Parameters (3, 0.1, 0.01, Timed [(1000,0.1)], 0.1, Timed [])) 1.1 (0.8/(1-0.8))
          c = rr (Parameters (3, 0.1, 0.01, Timed [(1000,0.1)], 0.1, Timed [])) 1.1 (0.3/(1-0.3))
       in do
        a < b `shouldBe` True
        b < c `shouldBe` True

    it "First partial derivative seems correct 1" $ do
      rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> rr (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 z) 0.7))
      rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 z) 0.9))
      rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 z) 0.7))
      rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 z) 0.9))

    it "Second partial derivative seems correct 1" $ do
      rr'' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 z) 0.7))
      rr'' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 z) 0.9))
      rr'' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 z) 0.7))
      rr'' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 z) 0.9))


testPdeGF = do
  describe "Test pdeGF" $ do
    it "First partial derivative seems correct 1" $ do
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 ((\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) z)) 0.7))
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) z) 0.9))
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) z) 0.7))
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) z) 0.9))
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))

    it "Second partial derivative seems correct 1" $ do
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) z) 0.7))
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol Zero 1) z) 0.9))
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) z) 0.7))
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol Zero 1) z) 0.9))
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-6 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF'' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))

testPdeStatistics = do
  describe "Test pdeStatistics" $ do
    it "Properties 1" $
      let (c,m,v) = pdeStatistics (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1 (PDESol Zero 1)
          (c',m',v') = pdeStatistics (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2 (PDESol Zero 1)
          (c'',m'',v'') = pdeStatistics (Parameters (2.3, 1, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 20 (PDESol Zero 1)
       in do
        c > c' `shouldBe` True
        c' > c'' `shouldBe` True
        c'' `shouldSatisfy` (withinDeltaOf 1e-6 0.0)
        m < m' `shouldBe` True
        m' < m'' `shouldBe` True
        v < v' `shouldBe` True
        v' < v'' `shouldBe` True
        m < v `shouldBe` True
        m' < v' `shouldBe` True
        m'' < v'' `shouldBe` True

    it "Properties 2" $
      let (c,m,v) = pdeStatistics (Parameters (2.3, 1.2, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 1 (PDESol (nbFromMAndV (3.0,9.0)) 2)
          (c',m',v') = pdeStatistics (Parameters (2.3, 1.2, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 2 (PDESol (nbFromMAndV (3.0,9.0)) 2)
          (c'',m'',v'') = pdeStatistics (Parameters (2.3, 1.2, 0.3, Timed [(1000,0.5)], 0.6, Timed [])) 20 (PDESol (nbFromMAndV (3.0,9.0)) 2)
       in do
        c > c' `shouldBe` True
        c' > c'' `shouldBe` True
        c'' `shouldSatisfy` (withinDeltaOf 1e-6 0.0)
        m < m' `shouldBe` True
        m' < m'' `shouldBe` True
        v < v' `shouldBe` True
        v' < v'' `shouldBe` True
        m < v `shouldBe` True
        m' < v' `shouldBe` True
        m'' < v'' `shouldBe` True

    it "Properties 3" $
      let (c,m,v) = pdeStatistics (Parameters (2.0,1.0,0.5, Timed [(1000,0.5)],0.4,Timed [])) 2.0 (PDESol (NegBinom 3.9 0.5) 1.0)
       in do
        c > 0 `shouldBe` True
        m > 0 `shouldBe` True
        v > m `shouldBe` True


testLlhd = do
  describe "Test llhd" $ do
    it "Manceau example" $
      let obs = [(1.0,OBirth),(1.0,OOccurrence),(1.0,OBirth),(1.0,OBirth),(1.0,OSample),(1.0,OOccurrence),(1.0,OCatastrophe 3)]
          (llhdVal1,_) = llhdAndNB obs (Parameters (1.1,1.0,0.3, Timed [(7.0,0.5)],0.6,Timed [])) initLlhdState
          (llhdVal2,_) = llhdAndNB obs (Parameters (1.2,1.0,0.3, Timed [(7.0,0.5)],0.6,Timed [])) initLlhdState
          (llhdVal3,_) = llhdAndNB obs (Parameters (1.3,1.0,0.3, Timed [(7.0,0.5)],0.6,Timed [])) initLlhdState
          (llhdVal9,_) = llhdAndNB obs (Parameters (1.9,1.0,0.3, Timed [(7.0,0.5)],0.6,Timed [])) initLlhdState
       in do
        llhdVal1 `shouldSatisfy` (withinDeltaOf 3e-1 (-40.5))
        llhdVal2 `shouldSatisfy` (withinDeltaOf 3e-1 (-41.0))
        llhdVal3 `shouldSatisfy` (withinDeltaOf 3e-1 (-41.5))
        llhdVal9 `shouldSatisfy` (withinDeltaOf 3e-1 (-46.0))

testInhomBDSLlhd = do
  describe "Test inhomogeneous BDS LLHD" $ do
    it "Check for constant parameters it looks right" $
      let obs = [(1.0,OBirth),(1.0,OBirth),(1.0,OSample),(1.0,OSample),(1.0,OSample)]
          tlams = fromJust $ asTimed [(0,1.2)]
          tlams' = fromJust $ asTimed [(0,1.2),(10,5.0)]
          tlams'' = fromJust $ asTimed [(0,1.3),(10,5.0)]
          tlams''' = fromJust $ asTimed [(0,1.3),(0.5,1.4)]
          tlams'''' = fromJust $ asTimed [(0,1.3),(1.5,1.3),(2.5,1.3)]
          lam = fromJust $ cadlagValue tlams 0.1
          lam'' = fromJust $ cadlagValue tlams'' 0.1
          (llhdValXXX1,_) = InhomBDSLlhd.inhomLlhdAndNB obs (InhomBDSLlhd.InhomParams (tlams,1.0,0.3)) InhomBDSLlhd.initLlhdState
          (llhdValXXX2,_) = InhomBDSLlhd.inhomLlhdAndNB obs (InhomBDSLlhd.InhomParams (tlams',1.0,0.3)) InhomBDSLlhd.initLlhdState
          (llhdValXXX3,_) = InhomBDSLlhd.inhomLlhdAndNB obs (InhomBDSLlhd.InhomParams (tlams'',1.0,0.3)) InhomBDSLlhd.initLlhdState
          (llhdValXXX4,_) = InhomBDSLlhd.inhomLlhdAndNB obs (InhomBDSLlhd.InhomParams (tlams''',1.0,0.3)) InhomBDSLlhd.initLlhdState
          (llhdValXXX5,_) = InhomBDSLlhd.inhomLlhdAndNB obs (InhomBDSLlhd.InhomParams (tlams'''',1.0,0.3)) InhomBDSLlhd.initLlhdState
          (llhdValYYY1,_) = llhdAndNB obs (Parameters (lam,1.0,0.3,Timed [],0.0,Timed [])) initLlhdState
          (llhdValYYY2,_) = llhdAndNB obs (Parameters (lam'',1.0,0.3,Timed [],0.0,Timed [])) initLlhdState
          (llhdValYYY3,_) = llhdAndNB obs (Parameters (lam'' + 0.1,1.0,0.3,Timed [],0.0,Timed [])) initLlhdState
       in do
        llhdValXXX1 `shouldSatisfy` (withinDeltaOf 1e-3 (llhdValYYY1))
        llhdValXXX2 `shouldSatisfy` (withinDeltaOf 1e-3 (llhdValYYY1))
        llhdValXXX3 `shouldSatisfy` (withinDeltaOf 1e-3 (llhdValYYY2))
        llhdValXXX3 `shouldSatisfy` (\l -> not $ withinDeltaOf 1e-3 llhdValYYY1 l)
        (if llhdValYYY3 < llhdValYYY2 then llhdValXXX3 > llhdValXXX4 else llhdValXXX3 < llhdValXXX4) `shouldBe` True
        llhdValXXX5 `shouldSatisfy` (withinDeltaOf 1e-1 (llhdValYYY2)) -- exposes limitation of approximation!!!
    describe "Check values are finite when sensible" $
      let infParams = (InhomBDSLlhd.InhomParams (fromJust $ asTimed [(0.0,1.0),(1.0,1.0)],0.4,0.4)) :: InhomBDSLlhd.InhomParams
          (InhomBDSLlhd.InhomParams (tlams,_,_)) = infParams
          obs = [(0.3,OBirth),(0.5,OBirth),(0.19,OSample)]
          llhdVal = fst $ InhomBDSLlhd.inhomLlhdAndNB obs infParams InhomBDSLlhd.initLlhdState
          obs' = [(0.3,OBirth),(0.5,OBirth),(0.20,OSample)]
          llhdVal' = fst $ InhomBDSLlhd.inhomLlhdAndNB obs' infParams InhomBDSLlhd.initLlhdState
          obs'' = [(0.3,OBirth),(0.5,OBirth),(0.21,OSample)]
          llhdVal'' = fst $ InhomBDSLlhd.inhomLlhdAndNB obs'' infParams InhomBDSLlhd.initLlhdState
       in do
        it "Check cadlagValue" $ do
          cadlagValue tlams 1.1 == Just 1.0 `shouldBe` True
          cadlagValue tlams 1.0 == Just 1.0 `shouldBe` True
          cadlagValue tlams 0.9 == Just 1.0 `shouldBe` True
        it "Check nextTime" $ do
          isJust (nextTime tlams 1.1) `shouldBe` True
          isJust (nextTime tlams 1.0) `shouldBe` True
          isJust (nextTime tlams 0.9) `shouldBe` True
        it "Check llhdValue" $ do
          isInfinite llhdVal `shouldBe` False
          isInfinite llhdVal' `shouldBe` False
          isInfinite llhdVal'' `shouldBe` False

testConversion = do
  describe "Test conversion between event types" $ do
    it "Demonstration data set 1" $
      let p1 = Person 1
          p2 = Person 2
          p4 = Person 4
          p5 = Person 5
          p6 = Person 6
          simObsEvents =
            [ Infection 1 p1 p2
            , Sampling 3 p1
            , Infection 4 p2 p4
            , Sampling 6 p4
            , Occurrence 8 p2
            , Occurrence 11 p6
            , Sampling 12 p5
            ]
          llhdObsEvents =
            [ (1.0, OBirth)
            , (2.0, OSample)
            , (1.0, OBirth)
            , (2.0, OSample)
            , (2.0, OOccurrence)
            , (3.0, OOccurrence)
            , (1.0, OSample)
            ]
       in eventsAsObservations simObsEvents `shouldSatisfy` (== llhdObsEvents)

testImpossibleParameters = do
  describe "Test correct handling of impossible parameters" $ do
    it "Test negative birth rate is impossible" $
      let obs = [(1.0,OBirth),(1.0,OOccurrence),(1.0,OBirth),(1.0,OBirth),(1.0,OSample),(1.0,OOccurrence)]
          llhd1 = fst $ llhdAndNB obs (Parameters (0.0000000001,1.0,0.3,Timed [],0.6,Timed [])) initLlhdState
          llhd2 = fst $ llhdAndNB obs (Parameters (0.0000000000,1.0,0.3,Timed [],0.6,Timed [])) initLlhdState
          llhd3 = fst $ llhdAndNB obs (Parameters (-0.0000000001,1.0,0.3,Timed [],0.6,Timed [])) initLlhdState
       in do
        isNaN llhd1 `shouldBe` False
        isNaN llhd2 `shouldBe` False
        isNaN llhd3 `shouldBe` False
        isInfinite llhd1 `shouldBe` False
        isInfinite llhd2 `shouldBe` True
        isInfinite llhd3 `shouldBe` True
        llhd1 < 0 `shouldBe` True
        llhd2 < 0 `shouldBe` True
        llhd3 < 0 `shouldBe` True
    it "Test negative sampling rate is impossible" $
      let obs1 = [(1.0,OBirth),(1.0,OOccurrence),(1.0,OBirth),(1.0,OBirth),(1.0,OSample),(1.0,OOccurrence)]
          obs2 = [(1.0,OBirth),(1.0,OOccurrence),(1.0,OBirth),(1.0,OBirth),(1.0,OOccurrence)]
          llhd11 = fst $ llhdAndNB obs1 (Parameters (1.0,1.0,0.1,Timed [],0.6,Timed [])) initLlhdState
          llhd12 = fst $ llhdAndNB obs1 (Parameters (1.0,1.0,0.0,Timed [],0.6,Timed [])) initLlhdState
          llhd13 = fst $ llhdAndNB obs1 (Parameters (1.0,1.0,-0.1,Timed [],0.6,Timed [])) initLlhdState
          llhd21 = fst $ llhdAndNB obs2 (Parameters (1.0,1.0,0.1,Timed [],0.6,Timed [])) initLlhdState
          llhd22 = fst $ llhdAndNB obs2 (Parameters (1.0,1.0,0.0,Timed [],0.6,Timed [])) initLlhdState
          llhd23 = fst $ llhdAndNB obs2 (Parameters (1.0,1.0,-0.1,Timed [],0.6,Timed [])) initLlhdState
       in do
        isNaN llhd11 `shouldBe` False
        isNaN llhd12 `shouldBe` False
        isNaN llhd13 `shouldBe` False
        isNaN llhd21 `shouldBe` False
        isNaN llhd22 `shouldBe` False
        isNaN llhd23 `shouldBe` False
        isInfinite llhd11 `shouldBe` False
        isInfinite llhd12 `shouldBe` True
        isInfinite llhd13 `shouldBe` True
        isInfinite llhd21 `shouldBe` False
        isInfinite llhd22 `shouldBe` False
        isInfinite llhd23 `shouldBe` True

tmpIsSampling :: EpidemicEvent -> Bool
tmpIsSampling e = case e of
  Sampling{} -> True
  _ -> False

-- | Simulate from the birth-death-sampling process multiple times and use this
-- to estimate the CI of probability the process is unobserved then check that
-- this matches the function which computes this probability.
bdsSimulations :: (Rate, Rate, Rate) -> Time -> IO Bool
bdsSimulations simRates simDuration =
  let simConfig = EpiBDS.configuration simDuration simRates
      probUnobservedVals =
        map
          (\x -> probabilityUnobserved simRates (simDuration + x))
          [-0.1, 0.0, 0.1]
      probUnobserved = probUnobservedVals !! 1
      numReplicates = 100
      numObservations = length . filter tmpIsSampling
      phat ns =
        (fromIntegral . length $ filter (> 0) ns) / (fromIntegral (length ns))
      probCI ns = (ph - d, ph + d)
        where
          ph = phat ns
          n = fromIntegral $ length ns
          d = 3 * sqrt (ph * (1 - ph) / n)
   in do sims <-
           replicateM
             numReplicates
             (EpiUtil.simulationWithSystemRandom
                False
                simConfig
                EpiBDS.allEvents)
         (a, b) <- pure . probCI $ map numObservations sims
         return $ a < (1 - probUnobserved) && (1 - probUnobserved) < b

-- | The tests is non-deterministic so on occasion it may fail. Unless it fails
-- multiple times this does not need further investigation.
testConditioningProbability :: SpecWith ()
testConditioningProbability =
  describe "Test probability of going unobserved is correct" $
    it "Test empirical estimate has CI containing value" $ do
  x <- bdsSimulations (2.0,0.4,0.1) 0.7
  x `shouldBe` True
  x' <- bdsSimulations (2.0,0.1,0.4) 0.7
  x' `shouldBe` True
  x'' <- bdsSimulations (2.0,0.1,0.4) 0.1
  x'' `shouldBe` True

testHmatrixUsage :: SpecWith ()
testHmatrixUsage =
  describe "Testing hmatrix-gsl usage" $
    it "test simulated annealing example" $ do
      let exampleParams = SimulatedAnnealingParams 200 1000 1.0 1.0 0.008 1.003 2.0e-6
          exampleE x = exp (-(x - 1)**2) * sin (8 * x)
          exampleM x y = abs $ x - y
          exampleS rands stepSize current = (rands ! 0) * 2 * stepSize - stepSize + current
          exampleMin = simanSolve 0 1 exampleParams 15.5 exampleE exampleM exampleS Nothing
      withinDeltaOf 1e-2 exampleMin 1.36 `shouldBe` True

testParameterUpdate :: SpecWith ()
testParameterUpdate =
  let params1 = (Parameters (2.4, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) :: Parameters
      params2 = (Parameters (2.3, 1, 0.3, Timed [(1000, 0.5)], 0.6, Timed [])) :: Parameters
      params3 = (Parameters (2.4, 1, 0.3, Timed [(1000, 0.6)], 0.6, Timed [])) :: Parameters
    in do describe "Testing parameter update function" $ do
            it "test lambda update" $ do
              (params1 /= params2) `shouldBe` True
              (params1 == putLambda params2 2.4) `shouldBe` True
            it "test rhos update" $ do
              (params1 /= params3) `shouldBe` True
              (params1 == putRhos params3 (Timed [(1000, 0.5)])) `shouldBe` True

testMWCSeeding :: SpecWith ()
testMWCSeeding = do
  describe "Testing MWC seeding" $
    it "test" $ do
      True `shouldBe` True
  
-- import System.Random.MWC
-- import Data.Vector.Unboxed as Unboxed
 
-- example :: IO (Double,Double)
-- example = do
--   gen <- create
--   x1 <- uniform gen
--   x2 <- uniform gen
--   gen' <- initialise (Unboxed.fromList [1,2,3])
--   y1 <- uniform gen'
--   y2 <- uniform gen'
--   gen'' <- initialise (Unboxed.fromList [1,2,3])
--   z <- uniform gen''
--   return (x1,x2,y1,y2,z) 


main :: IO ()
main = hspec $ do
  -- testNbPGF
  -- testPdeStatistics
  -- testp0
  -- testRr
  -- testPdeGF
  -- testLlhd
  -- testConversion
  -- testImpossibleParameters
  -- testInhomBDSLlhd
  -- testConditioningProbability
  -- testHmatrixUsage
  -- testParameterUpdate
  testMWCSeeding
