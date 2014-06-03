-- Module:      Instruments.Utils.BinomialModel
-- Copyright:   (c) Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Portability: portable
--
-- Types and functions for working with binomial models for option pricing
module Instruments.Utils.BinomialModel where

type Value = Double
type OnePeriodBM = (Double, Double, Double) -- value, up, down, rate modifier

data BinomialModel = Node      OnePeriodBM BinomialModel BinomialModel
                   | Singleton OnePeriodBM Value

bm_rec :: BinomialModel -> Double
bm_rec (Singleton b@(u,d,r) v) = (q*d_up+(1-q)*d_dn)/r
  where q = mtgl b
        d_up = v*u
        d_dn = v*d

bm_rec (Node b@(u,d,r) bmu bmd) = (q*d_up+(1-q)*d_dn)/r
  where q = mtgl b
        d_up = u * bm_rec bmu
        d_dn = d * bm_rec bmd

-- Compute q measure
mtgl :: (Double, Double, Double) -> Double
mtgl (u,d,r) = (r-d)/(u-d)
