module Utils.RootFinding.Tests where

-- TODO: QuickCheck
f x = x**2 - x - 3
a = 0
b = 4
epsilon = 0.0001
r = bisection epsilon f a b
