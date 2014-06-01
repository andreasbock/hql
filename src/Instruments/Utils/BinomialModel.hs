-- Module:      Instruments.Utils.BinomialModel
-- Copyright:   (c) Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Portability: portable
--
-- Types and functions for working with binomial models for option pricing
module Instruments.Utils.BinomialModel where

type OnePeriodBM = (Double, Double, Double) -- up, down, rate modifier
type D_Up = Double                          -- price @ t+1 up
type D_Down = Double                        -- price @ t+1 down

data BinomialModel = Node      OnePeriodBM BinomialModel BinomialModel
                   | Singleton OnePeriodBM D_Up D_Down

bm_rec :: BinomialModel -> Double
bm_rec (Singleton b@(_,_,r) d_up d_dn) = (q*d_up+(1-q)*d_dn)/r
  where q = mtgl b
bm_rec (Node b@(_,_,r) bmu bmd) = (q*d_up+(1-q)*d_dn)/r
  where q = mtgl b
        d_up = bm_rec bmu
        d_dn = bm_rec bmd

mtgl :: (Double, Double, Double) -> Double
mtgl (u,d,r) = (r-d)/(u-d)
