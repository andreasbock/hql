-- |
-- Module:      Utils.Currency
-- Copyright:   (c) Johan Astborg, Andreas Bock
-- License:     BSD-3
-- Maintainer:  Andreas Bock <bock@andreasbock.dk>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for HQLs Cash type
module Utils.Currency where
import Prelude hiding (sum)

-- Some common currencies
data Currency = USD | EUR | GBP | CHF | JPY | DKK | SEK | KRW deriving (Show,Eq)

-- redesign, see discounting
data Cash = Cash Double Currency

-- Format cash using currency symbol
-- TODO: Format with ***##.## decimals etc
instance Show Cash where
  show (Cash v USD) = '$' : show v
  show (Cash v EUR) = '€' : show v
  show (Cash v GBP) = '£' : show v
  show (Cash v JPY) = '¥' : show v
  show (Cash v CHF) = show v ++ " CHF"
  show (Cash v DKK) = show v ++ " kr"
  show (Cash v SEK) = show v ++ " kr"

instance Num Cash where
  (Cash v USD) + (Cash w USD) = Cash (v + w) USD
  (Cash v EUR) + (Cash w EUR) = Cash (v + w) EUR
  (Cash v GBP) + (Cash w GBP) = Cash (v + w) GBP
  (Cash v CHF) + (Cash w CHF) = Cash (v + w) CHF
  (Cash v JPY) + (Cash w JPY) = Cash (v + w) JPY
  (Cash v DKK) + (Cash w DKK) = Cash (v + w) DKK
  (Cash v SEK) + (Cash w SEK) = Cash (v + w) SEK
  _ + _ = error "Currency mismatch!"

  (Cash v USD) - (Cash w USD) = Cash (v - w) USD
  (Cash v EUR) - (Cash w EUR) = Cash (v - w) EUR
  (Cash v GBP) - (Cash w GBP) = Cash (v - w) GBP
  (Cash v CHF) - (Cash w CHF) = Cash (v - w) CHF
  (Cash v JPY) - (Cash w JPY) = Cash (v - w) JPY
  (Cash v DKK) - (Cash w DKK) = Cash (v - w) DKK
  (Cash v SEK) - (Cash w SEK) = Cash (v - w) SEK
  _ - _ = error "Currency mismatch!"

  (Cash v USD) * (Cash w USD) = Cash (v * w) USD
  (Cash v EUR) * (Cash w EUR) = Cash (v * w) EUR
  (Cash v GBP) * (Cash w GBP) = Cash (v * w) GBP
  (Cash v CHF) * (Cash w CHF) = Cash (v * w) CHF
  (Cash v JPY) * (Cash w JPY) = Cash (v * w) JPY
  (Cash v DKK) * (Cash w DKK) = Cash (v * w) DKK
  (Cash v SEK) * (Cash w SEK) = Cash (v * w) SEK
  _ * _ = error "Currency mismatch!"

    -- TODOs: Ajust for negative cash?
  fromInteger = undefined
  abs c = c
  signum c = c

instance Fractional Cash where
  (Cash v USD) / (Cash w USD) = Cash (v / w) USD
  (Cash v EUR) / (Cash w EUR) = Cash (v / w) EUR
  (Cash v GBP) / (Cash w GBP) = Cash (v / w) GBP
  (Cash v CHF) / (Cash w CHF) = Cash (v / w) CHF
  (Cash v JPY) / (Cash w JPY) = Cash (v / w) JPY
  (Cash v DKK) / (Cash w DKK) = Cash (v / w) DKK
  (Cash v SEK) / (Cash w SEK) = Cash (v / w) SEK
  _ / _ = error "Currency mismatch!"
  fromRational = undefined

instance Eq Cash where
  (Cash v c) == (Cash v' c') = v == v' && c == c'

instance Ord Cash where
  (Cash v c) < (Cash v' c')
    | c /= c'   = error "Currency mismatch!"
    | otherwise = v < v'

expC, add, scale :: Double -> Cash -> Cash
expC  d (Cash v c) = Cash (v**d) c
scale d (Cash v c) = Cash (d*v)  c
add   d (Cash v c) = Cash (d+v)  c

sum :: [Cash] -> Cash
sum (c:[]) = c
sum (c:cs) = c + sum cs

amount :: Cash -> Double
amount (Cash a _) = a

currency :: Cash -> Currency
currency (Cash _ c) = c

unCurrency :: Cash -> Double
unCurrency (Cash v _) = v

unCurrencies :: [Cash] -> [Double]
unCurrencies = map unCurrency

--- Support for currency conversion
data CurrencyPair = CurrencyPair Currency Currency
data ExchangeRate = ExchangeRate Double CurrencyPair

instance Show CurrencyPair where
    show (CurrencyPair b q) = show b ++ "/" ++ show q

-- TODO: Bid/Ask
instance Show ExchangeRate where
    show (ExchangeRate mid pair) = show pair ++ " " ++ show mid

{- Sample usage:

    For example, use the Cash data
    > Cash 115.2 USD
    $115.2

    > Cash 89.0 SEK
    89.0 kr

    Define a currecy pair
    > CurrencyPair EUR USD
    EUR/USD

    Specify an exchange rate for EUR/USD
    > ExchangeRate 1.32 (CurrencyPair EUR USD)
    EUR/USD 1.21

    **** TODOs
    Support conversion of currencies, like this:

    Cash 1000.00 USD `to` EUR
    -- Will use implicits and infix operator
    -- Idea: use the exchange rate as an implicit (ImplicitParams)
    -- See: http://www.haskell.org/haskellwiki/Implicit_parameters

-}
