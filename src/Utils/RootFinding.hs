-- |
-- Module:      Utils.RootFinding
-- Copyright:   (c) Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--

module Utils.RootFinding where

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
