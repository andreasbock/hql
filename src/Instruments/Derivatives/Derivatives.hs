{-# LANGUAGE GADTs, RankNTypes, RecordWildCards, TypeFamilies #-}
-- |
-- Module:      Instruments.Derivatives.Derivatives
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with interest rates
module Instruments.Derivatives.Derivatives where

import Instruments.Instrument
import Instruments.Utils.BinomialModel
import Instruments.FixedIncome.Bonds.Bonds
import Utils.Calendar
import Utils.Currency

type Strike = Cash
data Underlying a

class Instrument d => Derivative d where
  underlying :: d -> Underlying a

data Option where
  Call :: { c_strike :: Strike
          , c_expiry :: Date
          , c_undrly :: Underlying a
          } -> Option
  Put :: { p_strike :: Strike
         , p_expiry :: Date
         , p_undrly :: Underlying a
         } -> Option

instance Instrument Option where
  data PricingEngine Option = BinMdl BinomialModel
  --                        | ... fill in ...
  expired Call{..} = isExpired c_expiry
  expired Put{..} = isExpired p_expiry
  pv Call{..} mdl = undefined
  pv Put{..} mdl = undefined

