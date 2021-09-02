{-# LANGUAGE MultiWayIf           #-}

module Numeric.Minimisation.Powell (minimise) where

import Debug.Trace (traceShow)

data Golden = Golden { gPoint      :: Double
                     , gValue      :: Double
                     , gDerivative :: Double } deriving (Show)


-- | Bracket the minimum
--
-- >>> bracket (\x -> x * (x - 1)) 0 0.1
--
bracket :: (Double -> Double)
        -> Double
        -> Double
        -> Either String (Double,Double,Double)
bracket f = brcktIter 1.618034 f 0

brcktIter g f iter a b =
  let c = b + g * (b - a)
      (fa, fb, fc) = (f a, f b, f c)
      in if | iter > 100 -> Left "bracket exceeding maximum iterations."
            | a > b     -> brcktIter g f (iter+1) b a
            | fa < fb   -> Left "points given to bracket go uphill."
            | fb < fc   -> Right (a,b,c)
            | otherwise -> brcktIter g f (iter+1) b c

-- | Use a five-point stencil to numerically compute the derivative.
firstDerivative :: Floating a => a -> (a -> a) -> a -> a
firstDerivative h f x =
  (-f (x + 2 * h) + 8 * f (x + h) - 8 * f (x - h) + f (x - 2 * h)) / (12 * h)

-- | The gradient vector computed using 'firstDerivative'.
gradient :: Floating a => a -> ([a] -> a) -> [a] -> [a]
gradient h f x =
  let n = length x
      es = [replicate (i-1) 0 ++ [1] ++ replicate (n-i) 0 | i <- [1..n]]
      g e d = f [xi + d * ei | (xi,ei) <- zip x e]
  in [firstDerivative h (g e) 0.0 | e <- es]

-- | Golden section search.
--
-- >>> golden (\x -> x * (x - 1)) (0.26,0.52,0.94)
--
golden :: (Double -> Double)
       -> (Double,Double,Double)
       -> Either String Golden
golden f (a,b,c) =
  let
    r = 0.61803399
    tol = 3.0e-8
    (fa,fb,fc) = (f a,f b,f c)
    (x0,x3) = (a,c)
    (x1,x2) = if abs (c-b) > abs (b-a)
              then (b,r*b+(1-r)*c)
              else ((1-r)*a+r*b,b)
    (xmin,fmin) = gwl r tol f (x0,x1,x2,x3) (f x1,f x2)
    fder = firstDerivative tol f xmin
  in if a < b && b < c && fa > fb && fb < fc
     then Right $ Golden xmin fmin fder
     else Left "invalid triple given to golden"

-- | Golden while loop
gwl :: Double
    -> Double
    -> (Double -> Double)
    -> (Double,Double,Double,Double)
    -> (Double,Double)
    -> (Double,Double)
gwl r tol f (x0,x1,x2,x3) (f1,f2) =
  if | abs (x3 - x0) > tol * (abs x1 + abs x2) ->
         if f2 < f1
         then let x' = r*x2+(1-r)*x3
              in gwl r tol f (x1,x2,x',x3) (f2,f x')
         else let x' = r*x1+(1-r)*x0
              in gwl r tol f (x0,x',x1,x2) (f2,f x')
     | f1 < f2 -> (x1,f1)
     | otherwise -> (x2,f2)

-- | Powell's method with re-initialisation after \(N\) iterations of the basic
-- procedure.
--
-- >>> minimise (\xs -> sum $ (^2) <$> xs) [1..5]
--
minimise :: ([Double] -> Double) -> [Double] -> Either String ([Double], Double, [Double])
minimise f p0 = do (pmin,fmin) <- pwllR 1e-6 f p0 0
                   Right (pmin, fmin, gradient 1e-8 f pmin)

pwllR :: Double
      -> ([Double] -> Double)
      -> [Double]
      -> Int
      -> Either String ([Double], Double)
pwllR tol f p0 iter =
  if (traceShow iter iter) < 200
  then
    do (p1,_) <- powellBasic f (p0,eDirs $ length p0)
       let f0 = f p0
           f1 = f p1
       if 2.0 * (f0 - (traceShow (f1,p1) f1)) <= tol * (abs f0 + abs f1) + 1e-8
         then Right (p1,f1)
         else pwllR tol f p1 (iter+1)
  else Left "pwllR exceeding maximum iterations."


-- | Basis vectors
eDirs :: Int -> [[Double]]
eDirs n = [replicate (i-1) 0 ++ [1] ++ replicate (n-i) 0 | i <- [1..n]]

-- | Basic procedure used in Powell's method
--
-- >>> powellBasic (\xs -> sum $ (^2) <$> xs) ([1..5],(eDirs 5))
--
powellBasic :: ([Double] -> Double)
            -> ([Double], [[Double]])
            -> Either String ([Double], [[Double]])
powellBasic f (p0,us0) =
  do pN <- dsIter f p0 us0
     let usN = tail us0 ++ [zipWith (-) pN p0]
     return (pN,usN)

-- | Iterate over the direction set.
dsIter :: ([Double] -> Double)
       -> [Double]
       -> [[Double]]
       -> Either String [Double]
dsIter _ p0 [] = Right p0
dsIter f p0 (u:us) =
  let
    h = 1e-8
    p' :: Double -> [Double]
    p' a = [pj + a*uj | (pj,uj) <- zip p0 u]
    g :: Double -> Double
    g = f . p'
    gderiv = firstDerivative h g 0.0
    gdownhill = if gderiv < 0.0 then g else g . negate
  in do bkt <- bracket gdownhill 0.0 h
        gldn <- golden gdownhill bkt
        let p1 = p' $ if gderiv < 0.0
                      then gPoint gldn
                      else negate $ gPoint gldn
        dsIter f p1 us
