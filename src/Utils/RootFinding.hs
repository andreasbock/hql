-- |
-- Module:      Utils.RootFinding
-- Copyright:   (c) Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Newton-Raphson method for yield (currently)

-- TODO: Make it _much_ more general

module Utils.RootFinding where

-- import Utils.Calendar
-- import Utils.Payments
-- import Utils.Currency hiding (sum)

-- Bisection method implementation finding the root of f(x) in the interval [a,b]
--           tolerance     function 'f'        a         b         y 
bisection :: Double -> (Double -> Double) -> Double -> Double -> Double
bisection eps f a b =
  let
    m  = (a + b)/2
    fa = f a
    fm = f m
  in
    if (b-a < eps || fm == 0)
    then m
    else if (signum fa /= signum fm)
         then bisection eps f a m
         else bisection eps f m b

-- TODO: move to test
-- f x = x**2 - x - 3
-- a = 0
-- b = 4
-- epsilon = 0.0001
-- r = bisection epsilon f a b

-- myCF = [5,5,5,5,5,105] :: [Double]
-- myOffsets = [0.5,1,1.5,2,2.5,3] :: [Double]
-- myPV = 150.0
-- start = -1
-- stop  =  1
-- h = yieldCashflow myPV myCF myOffsets
-- r = bisection epsilon h start stop
