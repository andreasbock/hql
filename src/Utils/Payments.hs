-- |
-- Module:      Utils.Payments
-- Copyright:   (c) Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Basic types and functions

module Utils.Payments where

import Utils.Calendar
import Utils.Currency

type Repayment = Double
type Payment = (Date,Cash)
type Payments = [Payment]
