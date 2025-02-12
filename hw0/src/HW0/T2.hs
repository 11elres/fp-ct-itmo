module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import           Data.Void (Void)

type Not a = a -> Void

doubleNeg :: a -> Not (Not a) -- a -> (a -> Void) -> Void
doubleNeg a notA = notA a

reduceTripleNeg :: Not (Not (Not a)) -> Not a -- (((a -> Void) -> Void) -> Void) -> a -> Void
reduceTripleNeg notA3 a = notA3 (doubleNeg a)
