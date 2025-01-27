{-# LANGUAGE LambdaCase #-}

module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function   (fix)
import           Numeric.Natural (Natural)

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix $ \rec -> \case
  (x:xs) -> f x : rec xs
  []     -> []

fib :: Natural -> Natural
fib = fix (\rec a1 a2 n ->
  if n == 0 then a1
  else rec a2 (a1 + a2) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix $ \rec n -> if n == 0 then 1
                      else n * rec (n-1)
