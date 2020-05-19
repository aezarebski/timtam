import Test.Hspec

import qualified BDSCOD.InhomogeneousBDSLlhd as InhomBDSLlhd
import BDSCOD.Llhd
import BDSCOD.Types
import BDSCOD.Utility
import Data.Maybe (fromJust)
import qualified Epidemic as EpiSim
import Epidemic.Types

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
  describe "Test nbPGF" $ do
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


testp0 = do
  describe "Test p0" $ do
    it "Initial condition 1" $ do
      p0 (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 0.0001 0.2 `shouldSatisfy` (withinDeltaOf 1e-3 0.2)
      p0 (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 0.0001 1.0 `shouldSatisfy` (withinDeltaOf 1e-3 1.0)

    it "Evolution 1" $
      let a = p0 (2.3, 0.5, 0.3, [(1000, 0.5)], 0.6, []) 1.0 0.2
          b = p0 (2.3, 0.5, 0.3, [(1000, 0.5)], 0.6, []) 1.1 0.2
       in a > b `shouldBe` True

    it "Long term condition 1" $ do
      p0 (0.001, 5.9, 0.001, [(1000, 0.001)], 0.01, []) 100.0 0.2 `shouldSatisfy` (withinDeltaOf 1e-2 1.0)
      p0 (0.001, 5.9, 0.001, [(1000, 0.001)], 0.01, []) 100.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-2 1.0)

    it "Long term condition 2" $ do
      p0 (10.1, 0.1, 9.0, [(1000, 1.0)], 0.01, []) 100.0 0.2 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)
      p0 (10.1, 0.1, 9.0, [(1000, 1.0)], 0.01, []) 100.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)

    it "First partial derivative seems correct 1" $ do
      p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> p0 (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 z) 0.7))
      p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0 (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 z) 0.9))
      p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0 (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 z) 0.7))
      p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0 (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 z) 0.9))

    it "Second partial derivative seems correct 1" $ do
      p0'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 z) 0.7))
      p0'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 z) 0.9))
      p0'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 z) 0.7))
      p0'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> p0' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 z) 0.9))


testRr = do
  describe "Test rr" $ do
    it "Initial condition 1" $ do
      rr (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 0.0001 0.2 `shouldSatisfy` (withinDeltaOf 1e-3 (0.8/0.8))
      rr (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 0.0001 0.9 `shouldSatisfy` (withinDeltaOf 1e-3 (0.1/0.1))

    it "Evolution 1" $
      let a = rr (0.001, 0.001, 0.001, [(1000,0.9)], 0.001, []) 0.01 0.3
          b = rr (0.001, 0.001, 0.001, [(1000,0.9)], 0.001, []) 0.01 0.2
       in a > b `shouldBe` True

    it "Long term condition 1" $ do
      rr (3, 0.9, 0.01, [(1000,0.1)], 0.1, []) 100.0 0.2 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)
      rr (3, 0.9, 0.01, [(1000,0.1)], 0.1, []) 100.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-2 0.0)

    it "Long term condition 2" $
      let a = rr (3, 0.1, 0.01, [(1000,0.1)], 0.1, []) 1.1 (0.9/(1-0.9))
          b = rr (3, 0.1, 0.01, [(1000,0.1)], 0.1, []) 1.1 (0.8/(1-0.8))
          c = rr (3, 0.1, 0.01, [(1000,0.1)], 0.1, []) 1.1 (0.3/(1-0.3))
       in do
        a < b `shouldBe` True
        b < c `shouldBe` True

    it "First partial derivative seems correct 1" $ do
      rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> rr (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 z) 0.7))
      rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 z) 0.9))
      rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 z) 0.7))
      rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 z) 0.9))

    it "Second partial derivative seems correct 1" $ do
      rr'' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-2 (finiteDifference 1e-5 (\z -> rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 z) 0.7))
      rr'' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 z) 0.9))
      rr'' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 0.7 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 z) 0.7))
      rr'' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 0.9 `shouldSatisfy` (withinDeltaOf 1e-5 (finiteDifference 1e-5 (\z -> rr' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 z) 0.9))


testPdeGF = do
  describe "Test pdeGF" $ do
    it "First partial derivative seems correct 1" $ do
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol Zero 1) z) 0.7))
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol Zero 1) z) 0.9))
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol Zero 1) z) 0.7))
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol Zero 1) z) 0.9))
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF' (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))

    it "Second partial derivative seems correct 1" $ do
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol Zero 1) z) 0.7))
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol Zero 1) z) 0.9))
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol Zero 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol Zero 1) z) 0.7))
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol Zero 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol Zero 1) z) 0.9))
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-6 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 1.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.7 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.7))
      pdeGF'' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) 0.9 `shouldSatisfy` (withinDeltaOf 1e-4 (finiteDifference 1e-5 (\z -> pdeGF' (2.3, 1, 0.3, [(1000, 0.5)], 0.6, []) 2.0 (PDESol (nbFromMAndV (3.0,9.0)) 1) z) 0.9))

testPdeStatistics = do
  describe "Test pdeStatistics" $ do
    it "Properties 1" $
      let (c,m,v) = pdeStatistics (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 1 (PDESol Zero 1)
          (c',m',v') = pdeStatistics (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 2 (PDESol Zero 1)
          (c'',m'',v'') = pdeStatistics (2.3, 1, 0.3, [(1000,0.5)], 0.6, []) 20 (PDESol Zero 1)
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
      let (c,m,v) = pdeStatistics (2.3, 1.2, 0.3, [(1000,0.5)], 0.6, []) 1 (PDESol (nbFromMAndV (3.0,9.0)) 2)
          (c',m',v') = pdeStatistics (2.3, 1.2, 0.3, [(1000,0.5)], 0.6, []) 2 (PDESol (nbFromMAndV (3.0,9.0)) 2)
          (c'',m'',v'') = pdeStatistics (2.3, 1.2, 0.3, [(1000,0.5)], 0.6, []) 20 (PDESol (nbFromMAndV (3.0,9.0)) 2)
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
      let (c,m,v) = pdeStatistics (2.0,1.0,0.5,[(1000,0.5)],0.4,[]) 2.0 (PDESol (NegBinom 3.9 0.5) 1.0)
       in do
        c > 0 `shouldBe` True
        m > 0 `shouldBe` True
        v > m `shouldBe` True


testLlhd = do
  describe "Test llhd" $ do
    it "Manceau example" $
      let obs = [(1.0,Birth),(1.0,Occurrence),(1.0,Birth),(1.0,Birth),(1.0,Sample),(1.0,Occurrence),(1.0,Catastrophe 3)]
          (llhdVal1,_) = llhdAndNB obs (1.1,1.0,0.3,[(7.0,0.5)],0.6,[]) initLlhdState
          (llhdVal2,_) = llhdAndNB obs (1.2,1.0,0.3,[(7.0,0.5)],0.6,[]) initLlhdState
          (llhdVal3,_) = llhdAndNB obs (1.3,1.0,0.3,[(7.0,0.5)],0.6,[]) initLlhdState
          (llhdVal9,_) = llhdAndNB obs (1.9,1.0,0.3,[(7.0,0.5)],0.6,[]) initLlhdState
       in do
        llhdVal1 `shouldSatisfy` (withinDeltaOf 3e-1 (-40.5))
        llhdVal2 `shouldSatisfy` (withinDeltaOf 3e-1 (-41.0))
        llhdVal3 `shouldSatisfy` (withinDeltaOf 3e-1 (-41.5))
        llhdVal9 `shouldSatisfy` (withinDeltaOf 3e-1 (-46.0))

testInhomBDSLlhd = do
  describe "Test inhomogeneous BDS LLHD" $ do
    it "Check for constant parameters it looks right" $
      let obs = [(1.0,Birth),(1.0,Birth),(1.0,Sample),(1.0,Sample),(1.0,Sample)]
          tlams = fromJust $ asTimed [(0,1.2)]
          tlams' = fromJust $ asTimed [(0,1.2),(10,5.0)]
          tlams'' = fromJust $ asTimed [(0,1.3),(10,5.0)]
          tlams''' = fromJust $ asTimed [(0,1.3),(0.5,1.4)]
          tlams'''' = fromJust $ asTimed [(0,1.3),(1.5,1.3),(2.5,1.3)]
          lam = fromJust $ cadlagValue tlams 0.1
          lam'' = fromJust $ cadlagValue tlams'' 0.1
          (llhdValXXX1,_) = InhomBDSLlhd.llhdAndNB obs (tlams,1.0,0.3) initLlhdState
          (llhdValXXX2,_) = InhomBDSLlhd.llhdAndNB obs (tlams',1.0,0.3) initLlhdState
          (llhdValXXX3,_) = InhomBDSLlhd.llhdAndNB obs (tlams'',1.0,0.3) initLlhdState
          (llhdValXXX4,_) = InhomBDSLlhd.llhdAndNB obs (tlams''',1.0,0.3) initLlhdState
          (llhdValXXX5,_) = InhomBDSLlhd.llhdAndNB obs (tlams'''',1.0,0.3) initLlhdState
          (llhdValYYY1,_) = llhdAndNB obs (lam,1.0,0.3,[],0.0,[]) initLlhdState
          (llhdValYYY2,_) = llhdAndNB obs (lam'',1.0,0.3,[],0.0,[]) initLlhdState
          (llhdValYYY3,_) = llhdAndNB obs (lam'' + 0.1,1.0,0.3,[],0.0,[]) initLlhdState
       in do
        llhdValXXX1 `shouldSatisfy` (withinDeltaOf 1e-3 (llhdValYYY1))
        llhdValXXX2 `shouldSatisfy` (withinDeltaOf 1e-3 (llhdValYYY1))
        llhdValXXX3 `shouldSatisfy` (withinDeltaOf 1e-3 (llhdValYYY2))
        llhdValXXX3 `shouldSatisfy` (\l -> not $ withinDeltaOf 1e-3 llhdValYYY1 l)
        (if llhdValYYY3 < llhdValYYY2 then llhdValXXX3 > llhdValXXX4 else llhdValXXX3 < llhdValXXX4) `shouldBe` True
        llhdValXXX5 `shouldSatisfy` (withinDeltaOf 1e-1 (llhdValYYY2)) -- exposes limitation of approximation!!!

testConversion = do
  describe "Test conversion between event types" $ do
    it "Demonstration data set 1" $
      let p1 = EpiSim.Person 1
          p2 = EpiSim.Person 2
          p4 = EpiSim.Person 4
          p5 = EpiSim.Person 5
          p6 = EpiSim.Person 6
          simObsEvents =
            [ EpiSim.InfectionEvent 1 p1 p2
            , EpiSim.SamplingEvent 3 p1
            , EpiSim.InfectionEvent 4 p2 p4
            , EpiSim.SamplingEvent 6 p4
            , EpiSim.OccurrenceEvent 8 p2
            , EpiSim.OccurrenceEvent 11 p6
            , EpiSim.SamplingEvent 12 p5
            ]
          llhdObsEvents =
            [ (1.0, Birth)
            , (2.0, Sample)
            , (1.0, Birth)
            , (2.0, Sample)
            , (2.0, Occurrence)
            , (3.0, Occurrence)
            , (1.0, Sample)
            ]
       in eventsAsObservations simObsEvents `shouldSatisfy` (== llhdObsEvents)

testImpossibleParameters = do
  describe "Test correct handling of impossible parameters" $ do
    it "Test negative birth rate is impossible" $
      let obs = [(1.0,Birth),(1.0,Occurrence),(1.0,Birth),(1.0,Birth),(1.0,Sample),(1.0,Occurrence)]
          llhd1 = fst $ llhdAndNB obs (0.0000000001,1.0,0.3,[],0.6,[]) initLlhdState
          llhd2 = fst $ llhdAndNB obs (0.0000000000,1.0,0.3,[],0.6,[]) initLlhdState
          llhd3 = fst $ llhdAndNB obs (-0.0000000001,1.0,0.3,[],0.6,[]) initLlhdState
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
      let obs1 = [(1.0,Birth),(1.0,Occurrence),(1.0,Birth),(1.0,Birth),(1.0,Sample),(1.0,Occurrence)]
          obs2 = [(1.0,Birth),(1.0,Occurrence),(1.0,Birth),(1.0,Birth),(1.0,Occurrence)]
          llhd11 = fst $ llhdAndNB obs1 (1.0,1.0,0.1,[],0.6,[]) initLlhdState
          llhd12 = fst $ llhdAndNB obs1 (1.0,1.0,0.0,[],0.6,[]) initLlhdState
          llhd13 = fst $ llhdAndNB obs1 (1.0,1.0,-0.1,[],0.6,[]) initLlhdState
          llhd21 = fst $ llhdAndNB obs2 (1.0,1.0,0.1,[],0.6,[]) initLlhdState
          llhd22 = fst $ llhdAndNB obs2 (1.0,1.0,0.0,[],0.6,[]) initLlhdState
          llhd23 = fst $ llhdAndNB obs2 (1.0,1.0,-0.1,[],0.6,[]) initLlhdState
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

main :: IO ()
main = hspec $ do
  testNbPGF
  testPdeStatistics
  testp0
  testRr
  testPdeGF
  testLlhd
  testConversion
  testImpossibleParameters
  testInhomBDSLlhd
