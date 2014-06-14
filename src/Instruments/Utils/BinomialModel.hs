-- Module:      Instruments.Utils.BinomialModel
-- Copyright:   (c) Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Portability: portable
--
-- Types and functions for working with binomial models for option pricing
module Instruments.Utils.BinomialModel where
import Utils.Currency

type Value = Cash
type OnePeriodBM = (Double, Double, Double) -- value, up, down, rate modifier

data BinomialModel = Node      OnePeriodBM BinomialModel BinomialModel
                   | Singleton OnePeriodBM Value

bm_rec :: BinomialModel -> Cash
bm_rec (Singleton b@(u,d,r) v) = scale (recip r) $ d_up + d_dn
  where q = mtgl b
        d_up = scale (q*u) v
        d_dn = scale ((1-q)*d) v

bm_rec (Node b@(u,d,r) bmu bmd) = scale (recip r) $ d_up + d_dn
  where q = mtgl b
        d_up = scale (q*u)     $ bm_rec bmu
        d_dn = scale ((1-q)*d) $ bm_rec bmd

-- Compute q measure
mtgl :: (Double, Double, Double) -> Double
mtgl (u,d,r) = (r-d)/(u-d)
