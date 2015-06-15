{-# LANGUAGE TypeFamilies, GADTs, RecordWildCards, RankNTypes #-}
-- |
-- Module:      Instruments.FixedIncome.Bonds.Bonds
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working with interest rates

-- module HQL.Instruments.FixedIncome.Bonds where
module Instruments.FixedIncome.Bonds where

import Control.Monad (liftM)
import qualified Data.List as L
import Utils.Calendar
import Utils.Payments
import Utils.Currency
import Utils.DayCount
import Utils.RootFinding
import Instruments.Instrument
import Instruments.Utils.TermStructure
import Instruments.Utils.InterestRate
import Prelude hiding (sum)
-- import qualified Prelude hiding (sum)

--
-- Global parameters
--

yieldTolerance :: Double
yieldTolerance = 0.000001

--
-- Classes
--

-- | Bond class specifies common denominator for all bond types
class Instrument b => Bond b where
  clean, dirty :: b -> TermStructure -> Date -> Cash
  -- | Yield to maturity
  ytm          :: b -> BondQuote -> TermStructure -> Date -> Double
  -- | Accrued interest
  ai           :: b -> Date -> Cash
  -- | Principal (or face value) of a bond
  principal    :: b -> Cash
  -- | A list of Payments indicating remaining cashflow
  outstanding  :: b -> Payments
  -- | Returns a list of Payments representing the cashflow
  -- over the bond over its lifetime
  cashflow     :: b -> Payments
  -- | Returns the coupon part of the cashflow
  coupons      :: b -> Payments
  -- | Returns the dates at which cashflow is exchanged
  paymentDates :: b -> [Date]
  -- | Macaulay duration
  duration     :: b -> TermStructure -> Date -> Double
  duration bond ts now =
    let
      weighted_sum = amount $ sum $ zipWith3 macaulay dfs cs [1::Double ..]
    in
      inv_pv * weighted_sum
    where (ds,cs) = unzip $ cashflow bond
          dfs = dfsAt ts $ map (price_sanity . diffDayCount bond now) ds
          inv_pv = recip . amount $ dirty bond ts now
          macaulay df cs t = scale t $ discount df cs
  convexity    :: b -> Payment
  diffDayCount :: b -> Date -> Date -> Double
  clean bond ts now = dirty bond ts now - ai bond now
  -- If we cannot discount a given cashflow
  -- it has no theoretical value
    
  -- Add Either to signify whether or not we
  -- successfully discounted all cashflows?
  dirty bond ts now = sum $ zipWith discount dfs cs
    where (ds,cs) = unzip $ cashflow bond
          dfs = dfsAt ts $ map (price_sanity . diffDayCount bond now) ds
  -- | Yield to maturity
  ytm _    Clean _  _   = undefined -- needs `ai`
  ytm bond Dirty ts now = 0.0 -- bisection yieldTolerance yieldCashflow a b
    where (Cash pv currency) = dirty bond ts now
          (ds,cs) = unzip $ cashflow bond
          ds' = getYearOffsets now ds :: [Double]
          cs' = unCurrencies cs       :: [Double]
          -- function that should equal zero when `r` is the yield
          yieldCashflow :: Double -> Double
          yieldCashflow r = (L.sum $ zipWith (f r) cs' ds') - pv
          -- helper to compute the discounted cash flows using the yield `r`
          f :: Double -> Double -> Double -> Double
          f r c o = c*((1 + r)**o)
          -- interval [a,b] for bisection method, ±100% yields
          a = -1
          b =  1
  -- | Accrued interest
  ai = undefined

-- | Class declaration for amortized bonds
class Bond a => Amortized a where
  repayments :: a -> Payments
  repayments a = zip ds $ zipWith (-) cf cps
    where (ds,cf) = unzip $ cashflow a
          cps = snd $ unzip $ coupons a

-- | Class for mortage-backed obligations
class Instrument m => MBO m where
  prepayment          :: m -> Cash
  periodicPrepayment  :: m -> Cash
  survivalRate        :: m -> Rate
  scheduledPrepayment :: m -> Cash
  scheduledCoupon     :: m -> Cash

--
-- Instruments
--

data FixedAmortizedBond where
  Annuity :: { asett :: Date,
               amatu :: Date,
               aface :: Cash,
               arate :: Double,
               astms :: Settlements,
               adcc  :: Basis,
               aroll :: RollConvention } -> FixedAmortizedBond
  Serial :: {  asett :: Date,
               amatu :: Date,
               aface :: Cash,
               arate :: Double,
               astms :: Settlements,
               adcc  :: Basis,
               aroll :: RollConvention } -> FixedAmortizedBond

data FixedCouponBond where
  Zero   :: { fsett :: Date,
              fmatu :: Date,
              fface :: Cash,
              frate :: Double,
              fdcc  :: Basis,
              froll :: RollConvention } -> FixedCouponBond
  Consol :: { fsett :: Date,
              fface :: Cash,
              frate :: Double,
              fstms :: Settlements,
              fdcc  :: Basis,
              froll :: RollConvention } -> FixedCouponBond
  Bullet :: { fsett :: Date,
              fmatu :: Date,
              fface :: Cash,
              frate :: Double,
              fstms :: Settlements,
              fdcc  :: Basis,
              froll :: RollConvention } -> FixedCouponBond
--
-- Instances

instance Instrument FixedCouponBond where
  data PricingEngine FixedCouponBond = FCB TermStructure
  pv c@Consol{..} _ = return $ scale frate fface
  pv bond (FCB ts) = liftM (dirty bond ts) getDay
  expired Zero{..} = isExpired fmatu
  expired Consol{..} = return False
  expired Bullet{..} = isExpired fmatu

instance Instrument FixedAmortizedBond where
  data PricingEngine FixedAmortizedBond = FAB TermStructure
  pv bond (FAB ts) = liftM (dirty bond ts) getDay
  expired Serial{..} = isExpired amatu
  expired Annuity{..} = isExpired amatu

instance Bond FixedCouponBond where
  diffDayCount Zero{..}    = modifier fdcc
  diffDayCount Bullet{..}  = modifier fdcc
  diffDayCount Consol{..}  = modifier fdcc
  principal Zero{..} = fface
  principal Consol{..} = fface
  principal Bullet{..} = fface
  outstanding z@Zero{..} = [(fsett, principal z)]
  outstanding b@Bullet{..} = map (flip (,) fface) $ paymentDates b
  outstanding c@Consol{..} = map (flip (,) fface) $ paymentDates c
  coupons Zero{..} = []
  coupons Consol{..} = map (mkPayment (frate/stms) fface) dates
    where stms = fromIntegral fstms
          dates =  extrapolateDates froll fstms fsett
  coupons b@Bullet{..} = init $ cashflow b
  cashflow Zero{..} = (fmatu, fface) : []
  cashflow c@Consol{..} = coupons c
  cashflow Bullet{..}
    | dates == [] = [(fmatu,fface)]
    | otherwise   = map (\d -> (d,cpn)) (init dates) ++ [(fmatu,cpn+fface)]
    where dates = interpolateDates fmatu froll fstms fsett
          cpn = scale (frate/stms) fface
          stms  = fromIntegral fstms
  paymentDates Zero{..} = fmatu : []
  paymentDates Bullet{..} = interpolateDates fmatu froll fstms fsett
  paymentDates Consol{..} = extrapolateDates froll fstms fsett

  convexity = undefined

--   ytm z@Zero{..} ts = (face ** (negate $ recip t)) - 1
--     where zpv  = pv z ts
--           t    = duration z ts now
--           (Cash face _) = fface
--   ytm _ _ = error "Not implemented (Newton-Raphson method)."

instance Bond FixedAmortizedBond where
  diffDayCount Annuity{..} = modifier adcc
  diffDayCount Serial{..}  = modifier adcc
  principal Serial{..} = aface
  principal Annuity{..} = aface
  outstanding s@Serial{..} =
    let
      dates = paymentDates s
      repayment = scale (recip . fromIntegral $ length dates) aface
      outstds = take (length dates) $ iterate (\p -> p-repayment) aface
    in
      zip (asett : dates) (outstds ++ [(aface-aface)])
  outstanding a@Annuity{..} = zip (init ds) $ snd $ L.mapAccumL accOutstd aface rpys
    where (ds, rpys) = unzip $ repayments a
          accOutstd outstd rpy = (newOutstd, outstd)
            where newOutstd = outstd - rpy
  cashflow Serial{..} =
    let
      dates = interpolateDates amatu aroll astms asett
      repayment = scale (recip . fromIntegral $ length dates) aface
      accPymts :: Cash -> Date -> (Cash, Payment)
      accPymts outstd date = (outstd - repayment, (date, payment))
        where payment = repayment + scale arate outstd
    in
      snd $ L.mapAccumL accPymts aface dates
  cashflow Annuity{..} = map (flip (,) yield) dates
    where dates = interpolateDates amatu aroll astms asett
          yield = scale (recip $ (1-(1+r)**(-n))/r) aface
          n = fromIntegral $ length dates - 1 -- Subtract settlement
          r = arate / fromIntegral astms
  coupons Serial{..} =
    let
      dates = interpolateDates amatu aroll astms asett
      repayment = scale (recip . fromIntegral $ length dates) aface
    in
      snd $ L.mapAccumL (\o d -> (o-repayment, (d, scale arate o))) aface dates
  coupons a@Annuity{..} = zip (init ds) $ snd $ L.mapAccumL mkCpns aface cfs
    where (ds, cfs) = unzip $ cashflow a
          repayment = scale (recip $ (1-(1+perPeriodRate)**(-n))/perPeriodRate) aface
          mkCpns outstd cf = (outstd - repayment, coupon)
            where coupon = scale perPeriodRate outstd
          n = fromIntegral $ length ds -1
          perPeriodRate = arate / fromIntegral astms

  paymentDates Serial{..} = interpolateDates amatu aroll astms asett
  paymentDates Annuity{..} = interpolateDates amatu aroll astms asett
  convexity = undefined

instance Amortized FixedAmortizedBond

instance Show FixedCouponBond where
  show Zero{..} = "Zero:\n"
                  ++ "  Settlement Date:  " ++ show fsett ++ "\n"
                  ++ "  Maturity:         " ++ show fmatu ++ "\n"
                  ++ "  Face value:       " ++ show fface ++ "\n"
                  ++ "  Rate:             " ++ show frate ++ "\n"
                  ++ "  Basis:            " ++ show fdcc  ++ "\n"
                  ++ "  Roll Convention:  " ++ show froll
  show Consol{..} = "Consol:\n"
                    ++ "  Settlement Date:  " ++ show fsett ++ "\n"
                    ++ "  Face value:       " ++ show fface ++ "\n"
                    ++ "  Rate:             " ++ show frate ++ "\n"
                    ++ "  Settlements/year: " ++ show fstms ++ "\n"
                    ++ "  Basis:            " ++ show fdcc  ++ "\n"
                    ++ "  Roll Convention:  " ++ show froll
  show Bullet{..} = "Bullet:\n"
                    ++ "  Settlement Date:  " ++ show fsett ++ "\n"
                    ++ "  Maturity:         " ++ show fmatu ++ "\n"
                    ++ "  Face value:       " ++ show fface ++ "\n"
                    ++ "  Rate:             " ++ show frate ++ "\n"
                    ++ "  Settlements/year: " ++ show fstms ++ "\n"
                    ++ "  Basis:            " ++ show fdcc  ++ "\n"
                    ++ "  Roll Convention:  " ++ show froll

instance Show FixedAmortizedBond where
  show Annuity{..} = "Annuity:\n"
                     ++ "  Settlement Date:  " ++ show asett ++ "\n"
                     ++ "  Maturity:         " ++ show amatu ++ "\n"
                     ++ "  Face value:       " ++ show aface ++ "\n"
                     ++ "  Rate:             " ++ show arate ++ "\n"
                     ++ "  Settlements/year: " ++ show astms ++ "\n"
                     ++ "  Basis:            " ++ show adcc  ++ "\n"
                     ++ "  Roll Convention:  " ++ show aroll
  show Serial{..} = "Serial:\n"
                     ++ "  Settlement Date:  " ++ show asett ++ "\n"
                     ++ "  Maturity:         " ++ show amatu ++ "\n"
                     ++ "  Face value:       " ++ show aface ++ "\n"
                     ++ "  Rate:             " ++ show arate ++ "\n"
                     ++ "  Settlements/year: " ++ show astms ++ "\n"
                     ++ "  Basis:            " ++ show adcc  ++ "\n"
                     ++ "  Roll Convention:  " ++ show aroll


-- Computes the yield 'y' in the follow formula:
-- c*(1 + y)^-1 + c*(1 + y)^-2 + . . . + c*(1 + y)^-Y + B*(1 + y)^-Y = PV
-- yieldCashflow :: Double -> [Double] -> [Double] -> Double -> Double
-- yieldCashflow pv coupons offsets r = (sum $ zipWith f coupons offsets) - pv
--   where f c o = c * (1 + r)**o
-- myCF = [5,5,5,5,5,105] :: [Double]
-- myOffsets = [0.5,1,1.5,2,2.5,3] :: [Double]
-- myPV = 150.0
-- start = -1
-- stop  =  1
-- h = yieldCashflow myPV myCF myOffsets
-- r = bisection yieldTolerance h start stop

-- Helper functions
mkPayment :: Rate -> Cash -> Date -> Payment
mkPayment rate face date = (date, scale rate face)

discount (Just df) = scale df
discount Nothing   = scale 0

price_sanity x
  | x >= 0    = x
  | otherwise = error "Cannot price past cashflow!"
