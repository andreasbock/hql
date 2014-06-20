-- Module:      Instruments.Utils.BinomialModel
-- Copyright:   (c) Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Portability: portable
--
-- Types and functions for working with binomial models for option pricing
module Instruments.Utils.BinomialModel where
import Utils.Currency
import Debug.Trace
import Control.Applicative

type Value = Cash
type OnePeriodBM = (Double, Double, Double) -- value, up, down, rate modifier

data BinomialModel = Node      OnePeriodBM BinomialModel BinomialModel
                   | Singleton OnePeriodBM Value

bm_rec :: BinomialModel -> Maybe Cash
bm_rec (Singleton b@(u,d,r) v) =
  let
    q_sum = (+) <$> d_up <*> d_dn
  in scale (1/r) <$> q_sum
  where q = mtgl b
        d_up = scale_q           q  u v
        d_dn = scale_q ((1-) <$> q) d v

bm_rec (Node b@(u,d,r) bmu bmd) =
  do
    q    <- mtgl b
    d_up <- fmap (scale $ q*u)     $ bm_rec bmu
    d_dn <- fmap (scale $ (1-q)*d) $ bm_rec bmd
    return $ scale (1/r) $ d_up + d_dn

-- | Compute Q measure
mtgl :: OnePeriodBM -> Maybe Double
mtgl (u,d,r) | 0 <= q && q <= 1 = Just q
              | otherwise = trace "No equivalent martingale measure!" Nothing
  where q =  (r-d)/(u-d)

scale_q :: Maybe Double -> Double -> Cash -> Maybe Cash
scale_q  Nothing c v = Nothing
scale_q (Just f) c v = pure $ scale (f*c) v
