{-# LANGUAGE DeriveGeneric #-}

import BDSCOD.Conditioning
import BDSCOD.Aggregation
import qualified BDSCOD.InhomogeneousBDSLlhd as InhomBDSLlhd
import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import Control.Monad (replicateM)
import Data.List (sort)
import Data.Maybe (fromJust, isJust)
import Data.Tuple (swap)
import qualified Data.Vector.Unboxed as Unboxed
import qualified Epidemic as EpiSim
import qualified Epidemic.BirthDeathSampling as EpiBDS
import qualified Epidemic.BDSCOD as EpiBDSCOD
import Epidemic.Types.Events
import Epidemic.Types.Parameter
import Epidemic.Types.Population
import qualified Epidemic.Utility as EpiUtil
import GHC.Generics
import Generic.Random (genericArbitraryU)
import Numeric.GSL.SimulatedAnnealing
import Numeric.LinearAlgebra.HMatrix
import qualified System.Random.MWC as MWC
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.Hspec.Core.QuickCheck (modifyMaxDiscardRatio)

-- | Check if @y@ is withing @delta@ of @x@
withinDeltaOf :: (Ord a, Num a)
              => a -- ^ delta
              -> a -- ^ y
              -> a -- ^ x
              -> Bool
withinDeltaOf delta y x = abs (y - x) < delta

-- | Apply the @withinDeltaOf@ function to two lists.
allWithinDeltaOf :: (Ord a, Num a) => a -> [a] -> [a] -> Bool
allWithinDeltaOf _ [] [] = True
allWithinDeltaOf delta [y] [x] = withinDeltaOf delta y x
allWithinDeltaOf delta (y:ys) (x:xs) = withinDeltaOf delta y x && allWithinDeltaOf delta ys xs
allWithinDeltaOf _ _ _ = False





-- | Approximate the derivative of @f@ at @x@ with a step of size @h@.
finiteDifference :: Fractional a
                 => a         -- ^ h
                 -> (a -> a)  -- ^ f
                 -> a         -- ^ x
                 -> a
finiteDifference h f x = (f (x+h) - f (x-h)) / (2*h)

testTestingHelpers =
  describe "Testing the helper functions for the testing suite" $ do
    context "withinDeltaOf" $ do
      it "sanity" $ do
        withinDeltaOf 0.2 1 2 `shouldBe` False
        withinDeltaOf 0.2 1 1.1 `shouldBe` True
      it "identity" $ property $
        \x -> withinDeltaOf 0.2 x (x :: Double)
      it "range" $ property $
        \x -> withinDeltaOf 1 1 (1 + (x :: Double)) || not (x < 1 && x > (-1))
    context "finiteDifference" $ do
      it "sine" $ property $
        \x -> let fFD = finiteDifference 0.01 sin
                  f' = cos
                in withinDeltaOf 1e-3 (fFD (x :: Double)) (f' x)
      it "polynomial" $ property $
        \x -> let fFD = finiteDifference 0.01 (\z -> z + 0.5 * z ** 2)
                  f' = \z -> 1 + z
                in withinDeltaOf 1e-3 (fFD (x :: Double)) (f' x)

testLogSumExp =
  describe "Test the log-sum-exp function" $
    it "equivalence to unsafe implementation" $ property $
      \x -> (not $ null x) ==> withinDeltaOf 1e-6 (logSumExp x) (log (sum [exp x' | x' <- x :: [Double]]))


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

testLogPdeGF1 = do
    describe "test pdeGF and logPdeGF" $ do
      modifyMaxDiscardRatio (const 1000) $
        it "test pdeGF and logPdeGF" $ property $
        \(z
         , delay
         , lam
         , mu) -> (z > 0) &&
                  (delay < 30) &&
                  (delay > 0) &&
                  (lam < 30) &&
                  (lam > 0) &&
                  (mu < 30) &&
                  (mu > 0) ==>
                  withinDeltaOf 1e-3 (log $ pdeGF (params lam mu) delay pdeSol z) (logPdeGF (params lam mu) delay pdeSol z)
                  where params lam mu = (Parameters (lam, mu, 0.3, Timed [(1000,0.5)], 0.6, Timed []))
                        pdeSol = (PDESol Zero 1)

testLogPdeGF2 = do
    describe "test pdeGF and logPdeGF again" $ do
      modifyMaxDiscardRatio (const 10000) $
        it "test pdeGF and logPdeGF again" $ property $
        \(z
         , lam
         , delay
         , nbMean
         , nbVar) -> (z <= 1) && (z > 0) &&
                     (lam < 200) &&
                     (lam > 0) &&
                     (delay < 200) &&
                     (delay > 0) &&
                     (nbMean > 0) &&
                     (nbVar > nbMean) ==>
                     withinDeltaOf 1e-3 (log $ pdeGF (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar)) z) (logPdeGF (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar)) z)
                     where params lam = (Parameters (lam / 10, 0.3, 0.3, Timed [(1000,0.5)], 0.6, Timed []))
                           pdeSol nbStats = (PDESol (nbFromMAndV nbStats) 1)
                           scaledDelay d = d / 10


testLogPdeGFDash1 = do
    describe "test pdeGF' and logPdeGF'" $ do
      modifyMaxDiscardRatio (const 1000) $
        it "test pdeGF' and logPdeGF'" $ property $
        \(z100
         , delay
         , lam
         , mu) -> (z100 <= 100) &&
                  (z100 > 0) &&
                  (delay < 30) &&
                  (delay > 0) &&
                  (lam < 30) &&
                  (lam > 0) &&
                  (mu < 30) &&
                  (mu > 0) ==>
                  withinDeltaOf 1e-3 (log $ pdeGF' (params lam mu) delay pdeSol (z100 / 100)) (logPdeGF' (params lam mu) delay pdeSol (z100 / 100))
                  where params lam mu = (Parameters (lam, mu, 0.3, Timed [(1000,0.5)], 0.6, Timed []))
                        pdeSol = (PDESol Zero 1)

testLogPdeGFDash2 = do
    describe "test pdeGF' and logPdeGF' again" $ do
      modifyMaxDiscardRatio (const 10000) $
        it "test pdeGF' and logPdeGF' again" $ property $
        \(z
         , lam
         , delay
         , nbMean
         , nbVar) -> (z <= 1) && (z > 0) &&
                     (lam < 200) &&
                     (lam > 0) &&
                     (delay < 200) &&
                     (delay > 0) &&
                     (nbMean > 0) &&
                     (nbVar > nbMean) ==>
                     withinDeltaOf 1e-3 (log $ pdeGF' (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar)) z) (logPdeGF' (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar)) z)
                     where params lam = (Parameters (lam / 10, 0.3, 0.3, Timed [(1000,0.5)], 0.6, Timed []))
                           pdeSol nbStats = (PDESol (nbFromMAndV nbStats) 1)
                           scaledDelay d = d / 50

testLogPdeGFDashDash1 = do
    describe "test pdeGF'' and logPdeGF''" $ do
      modifyMaxDiscardRatio (const 1000) $
        it "test pdeGF'' and logPdeGF''" $ property $
        \(z100
         , delay
         , lam
         , mu) -> (z100 <= 100) &&
                  (z100 > 0) &&
                  (delay < 30) &&
                  (delay > 0) &&
                  (lam < 30) &&
                  (lam > 0) &&
                  (mu < 30) &&
                  (mu > 0) ==>
                  withinDeltaOf 1e-3 (log $ pdeGF'' (params lam mu) delay pdeSol (z100 / 100)) (logPdeGF'' (params lam mu) delay pdeSol (z100 / 100))
                  where params lam mu = (Parameters (lam, mu, 0.3, Timed [(1000,0.5)], 0.6, Timed []))
                        pdeSol = (PDESol Zero 1)

testLogPdeGFDashDash2 = do
    describe "test pdeGF'' and logPdeGF'' again" $ do
      modifyMaxDiscardRatio (const 10000) $
        it "test pdeGF'' and logPdeGF'' again" $ property $
        \(z
         , lam
         , delay
         , nbMean
         , nbVar) -> (z <= 1) && (z > 0) &&
                     (lam < 200) &&
                     (lam > 0) &&
                     (delay < 200) &&
                     (delay > 0) &&
                     (nbMean > 0) &&
                     (nbVar > nbMean) ==>
                     withinDeltaOf 1e-3 (log $ pdeGF'' (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar)) z) (logPdeGF'' (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar)) z)
                     where params lam = (Parameters (lam / 10, 0.3, 0.3, Timed [(1000,0.5)], 0.6, Timed []))
                           pdeSol nbStats = (PDESol (nbFromMAndV nbStats) 1)
                           scaledDelay d = d / 50



testLogPdeStatistics = do
    describe "test pdeStatistics and logPdeStatistics" $ do
      modifyMaxDiscardRatio (const 10000) $
        it "test pdeStatistics and logPdeStatistics" $ property $
        \( lam
         , delay
         , nbMean
         , nbVar) -> (lam < 200) &&
                     (lam > 6) &&
                     (delay < 200) &&
                     (delay > 0) &&
                     (nbMean > 1) &&
                     (nbVar > nbMean) ==>
                     withinDeltaOf 1e-3 (log . fst' $ fooUnlogged lam delay nbMean nbVar) (fst' $ fooLogged lam delay nbMean nbVar) &&
                     withinDeltaOf 1e-3 (log . snd' $ fooUnlogged lam delay nbMean nbVar) (snd' $ fooLogged lam delay nbMean nbVar) &&
                     withinDeltaOf 1e-3 (log . thd' $ fooUnlogged lam delay nbMean nbVar) (thd' $ fooLogged lam delay nbMean nbVar)
                     where params lam = (Parameters (lam / 10, 0.3, 0.3, Timed [(1000,0.5)], 0.6, Timed []))
                           pdeSol nbStats = (PDESol (nbFromMAndV nbStats) 1)
                           scaledDelay d = d / 50
                           fooUnlogged lam delay nbMean nbVar = pdeStatistics (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar))
                           fooLogged lam delay nbMean nbVar  = (logPdeStatistics (params lam) (scaledDelay delay) (pdeSol (nbMean,nbVar)))
                           fst' (a,_,_) = a
                           snd' (_,a,_) = a
                           thd' (_,_,a) = a



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
      let obs = [(1.0,OBirth),(1.0,OOccurrence),(1.0,OBirth),(1.0,OBirth),(1.0,ObsUnscheduledSequenced),(1.0,OOccurrence),(1.0,OCatastrophe 3)]
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
      let obs = [(1.0,OBirth),(1.0,OBirth),(1.0,ObsUnscheduledSequenced),(1.0,ObsUnscheduledSequenced),(1.0,ObsUnscheduledSequenced)]
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
          obs = [(0.3,OBirth),(0.5,OBirth),(0.19,ObsUnscheduledSequenced)]
          llhdVal = fst $ InhomBDSLlhd.inhomLlhdAndNB obs infParams InhomBDSLlhd.initLlhdState
          obs' = [(0.3,OBirth),(0.5,OBirth),(0.20,ObsUnscheduledSequenced)]
          llhdVal' = fst $ InhomBDSLlhd.inhomLlhdAndNB obs' infParams InhomBDSLlhd.initLlhdState
          obs'' = [(0.3,OBirth),(0.5,OBirth),(0.21,ObsUnscheduledSequenced)]
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
            , (2.0, ObsUnscheduledSequenced)
            , (1.0, OBirth)
            , (2.0, ObsUnscheduledSequenced)
            , (2.0, OOccurrence)
            , (3.0, OOccurrence)
            , (1.0, ObsUnscheduledSequenced)
            ]
       in eventsAsObservations simObsEvents `shouldSatisfy` (== llhdObsEvents)




testImpossibleParameters = do
  describe "Test correct handling of impossible parameters" $ do
    it "Test negative birth rate is impossible" $
      let obs = [(1.0,OBirth),(1.0,OOccurrence),(1.0,OBirth),(1.0,OBirth),(1.0,ObsUnscheduledSequenced),(1.0,OOccurrence)]
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
      let obs1 = [(1.0,OBirth),(1.0,OOccurrence),(1.0,OBirth),(1.0,OBirth),(1.0,ObsUnscheduledSequenced),(1.0,OOccurrence)]
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
      numReplicates = 1000
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

testConditioningProbability :: SpecWith ()
testConditioningProbability =
  describe "Test probability of going unobserved is correct" $ do
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

-- | This test case looks at how to set the seed when using @mwc-random@. We
-- care about this because we want to be able to generate reproducible
-- simulations without being restricted to the fixed seed that is the default
-- value.
testMWCSeeding :: SpecWith ()
testMWCSeeding = do
  describe "Testing MWC seeding" $ do
    it "test create works as expected" $ do
      gen <- MWC.create
      x1 <- (MWC.uniform gen :: IO Double)
      x2 <- (MWC.uniform gen :: IO Double)
      gen' <- MWC.create
      x1' <- (MWC.uniform gen' :: IO Double)
      (x1 /= x2) `shouldBe` True
      (x1 == x1') `shouldBe` True
    it "test initialise works as expected" $ do
      xGen <- MWC.create
      x1 <- (MWC.uniform xGen :: IO Double)
      yGen <- MWC.initialize (Unboxed.fromList [1,2,3])
      y1 <- (MWC.uniform yGen :: IO Double)
      (x1 /= y1) `shouldBe` True
      y2 <- (MWC.uniform yGen :: IO Double)
      (y1 /= y2) `shouldBe` True
      zGen <- MWC.initialize (Unboxed.fromList [1,2,3])
      z1 <- (MWC.uniform zGen :: IO Double)
      (y1 == z1) `shouldBe` True
      wGen <- MWC.initialize (Unboxed.fromList [3,2,1])
      w1 <- (MWC.uniform wGen :: IO Double)
      (z1 /= w1) `shouldBe` True



-- | Generate a random @ObservedEvent@
qcRandomObservedEvent :: Gen ObservedEvent
qcRandomObservedEvent = do
  isUnscheduled <- chooseAny
  if isUnscheduled
    then elements [OBirth,ObsUnscheduledSequenced,OOccurrence]
    else do isSequenced <- chooseAny
            numLineages <- suchThat chooseAny (>0)
            if isSequenced
              then return (OCatastrophe numLineages)
              else return (ODisaster numLineages)


-- | Generate a random list of @observation@ values
qcRandomObservations :: Gen [Observation]
qcRandomObservations = do
  duration <- suchThat chooseAny (>0) :: Gen Time
  eventAbsTimes <- listOf1 (choose (0,duration))
  let eats = sort (0:eventAbsTimes)
      timeDeltas = [b - a | (a, b) <- zip (init eats) (tail eats)]
      numEvents = length eventAbsTimes
  eventTypes <- vectorOf numEvents qcRandomObservedEvent
  return $ zip timeDeltas eventTypes


-- | A copy of the @withinDeltaOf@ function specialised to
-- @AggregatedObservations@ to test for approximate equality of aggregated
-- observations.
withinDeltaOfAggObs :: Double
                    -> AggregatedObservations
                    -> AggregatedObservations
                    -> Bool
withinDeltaOfAggObs delta (AggregatedObservations aggTimes obs) (AggregatedObservations aggTimes' obs') =
  withinDeltaOfAggTimes delta aggTimes aggTimes' &&  allWithinDeltaOfObs delta obs obs'

withinDeltaOfAggTimes :: Double -> AggregationTimes -> AggregationTimes -> Bool
withinDeltaOfAggTimes delta (AggTimes ts) (AggTimes ts') =
  let times = map fst ts
      obsEvents = map snd ts
      times' = map fst ts'
      obsEvents' = map snd ts'
      timesWithinDelta = allWithinDeltaOf delta times times'
      observedEventsEqual = all (uncurry (withinDeltaOfObsEvent delta)) (zip obsEvents obsEvents')
  in timesWithinDelta && observedEventsEqual

withinDeltaOfObsEvent :: Double -> ObservedEvent -> ObservedEvent -> Bool
withinDeltaOfObsEvent delta (ODisaster nl) (ODisaster nl') = withinDeltaOf delta nl nl'
withinDeltaOfObsEvent delta (OCatastrophe nl) (OCatastrophe nl') = withinDeltaOf delta nl nl'
withinDeltaOfObsEvent _ OBirth OBirth = True
withinDeltaOfObsEvent _ ObsUnscheduledSequenced ObsUnscheduledSequenced = True
withinDeltaOfObsEvent _ OOccurrence OOccurrence = True
withinDeltaOfObsEvent _ _ _ = False

withinDeltaOfObs :: Double -> Observation -> Observation -> Bool
withinDeltaOfObs delta (t,oe) (t',oe') = withinDeltaOf delta t t' && withinDeltaOfObsEvent delta oe oe'

allWithinDeltaOfObs :: Double -> [Observation] -> [Observation] -> Bool
allWithinDeltaOfObs _ [] [] = True
allWithinDeltaOfObs delta [y] [x] = withinDeltaOfObs delta y x
allWithinDeltaOfObs delta (y:ys) (x:xs) = withinDeltaOfObs delta y x && allWithinDeltaOfObs delta ys xs
allWithinDeltaOfObs _ _ _ = False



testAggregation :: SpecWith ()
testAggregation = do
  describe "Testing Aggregation" $ do
    let smallDelta = 1e-4
        tinyDelta = 1e-6
        emptyAggTimes = fromJust (maybeAggregationTimes [] [])
        propertyIdentity obs = let aggObs1 = aggregateUnscheduledObservations emptyAggTimes obs
                                   aggObs2 = AggregatedObservations emptyAggTimes obs
                                  in withinDeltaOfAggObs smallDelta aggObs1 aggObs2
        duration obs = sum $ map fst obs
        propertyRemoveSeq obs = let dur = duration obs
                                    ats = fromJust $ maybeAggregationTimes [dur + tinyDelta] []
                                    (AggregatedObservations _ obs') = aggregateUnscheduledObservations ats obs
                                in  not $ any isUnscheduledSequenced obs'
        propertyRemoveUnseq obs = let dur = duration obs
                                      ats = fromJust $ maybeAggregationTimes [] [dur + tinyDelta]
                                      (AggregatedObservations _ obs') = aggregateUnscheduledObservations ats obs
                                  in  not $ any isOccurrence obs'
        propertyRemoveUnsched1 obs = let dur = duration obs
                                         ats = fromJust $ maybeAggregationTimes [dur + tinyDelta] [dur + tinyDelta + 1.0]
                                         (AggregatedObservations _ obs') = aggregateUnscheduledObservations ats obs
                                     in  not (any isOccurrence obs') && not (any isUnscheduledSequenced obs')
        propertyRemoveUnsched2 obs = let dur = duration obs
                                         ats = fromJust $ maybeAggregationTimes [0.4 * dur] [0.5 * dur]
                                         (AggregatedObservations _ obs') = aggregateUnscheduledObservations ats obs
                                     in  not (any isOccurrence obs') && not (any isUnscheduledSequenced obs')
        propertyBirthsRemain obs = let dur = duration obs
                                       numBs = length $ filter isBirth obs
                                       ats = fromJust $ maybeAggregationTimes [dur + tinyDelta] [dur + tinyDelta + 1.0]
                                       (AggregatedObservations _ obs') = aggregateUnscheduledObservations ats obs
                                       numBs' = length $ filter isBirth obs'
                                   in numBs == numBs'
        propertyLineagesConst os = let dur = duration os
                                       numSeq = sum $ map numSequenced os
                                       numUnseq = sum $ map numUnsequenced os
                                       ats = fromJust $ maybeAggregationTimes [dur + tinyDelta] [dur + tinyDelta + 1.0]
                                       (AggregatedObservations _ os') = aggregateUnscheduledObservations ats os
                                       numSeq' = sum $ map numSequenced os'
                                       numUnseq' = sum $ map numUnsequenced os'
                                   in withinDeltaOf smallDelta numSeq numSeq' && withinDeltaOf smallDelta numUnseq numUnseq'
    it "without aggregation nothing changes" $ forAll qcRandomObservations propertyIdentity
    it "sequenced aggregation removes all such unscheduled observations" $ forAll qcRandomObservations propertyRemoveSeq
    it "unsequenced aggregation removes all such unscheduled observations" $ forAll qcRandomObservations propertyRemoveUnseq
    it "aggregating both removes all relevent observations 1" $ forAll qcRandomObservations propertyRemoveUnsched1
    it "aggregating both removes all relevent observations 2" $ forAll qcRandomObservations propertyRemoveUnsched2
    it "aggregating leaves birth observations unchanged" $ forAll qcRandomObservations propertyBirthsRemain
    it "aggregating leaves the number of observed lineages unchanged" $ forAll qcRandomObservations propertyLineagesConst



main :: IO ()
main = hspec $ do
  -- ** slow tests **
  testNbPGF
  testHmatrixUsage
  testConditioningProbability
  -- ** fast tests **
  testTestingHelpers
  testPdeStatistics
  testp0
  testRr
  testPdeGF
  testLlhd
  testConversion
  testImpossibleParameters
  testInhomBDSLlhd
  testParameterUpdate
  testMWCSeeding
  testLogPdeGF1
  testLogPdeGF2
  testLogPdeGFDash1
  testLogPdeGFDash2
  testLogPdeGFDashDash1
  testLogPdeGFDashDash2
  testLogSumExp
  testLogPdeStatistics
  testAggregation
