module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

-- | Peano numbers.
data N = Z | S N

-- | Addition.
nplus :: N -> N -> N
nplus Z a     = a 
nplus a Z     = a 
nplus a (S b) = S $ nplus a b

-- | Multiplication.
nmult :: N -> N -> N
nmult Z _     = Z
nmult _ Z     = Z
nmult a (S b) = nplus a $ nmult a b 

-- | Substraction (Nothing if result is negative).
nsub :: N -> N -> Maybe N
nsub a Z         = Just a 
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

-- | Comparison.
ncmp :: N -> N -> Ordering
ncmp a b = case nsub a b of
  Nothing -> LT
  Just Z  -> EQ
  Just _  -> GT

-- | Form N by Natural.
nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural a = S $ nFromNatural $ a - 1

-- | Convert N to Num.
nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S a) = nToNum a + 1

-- | Checks if the number is even.
nEven :: N -> Bool
nEven = (== EQ) . (ncmp Z) . (flip nmod $ S $ S Z)

-- | Checks if the number is odd.
nOdd :: N -> Bool
nOdd = not . nEven

-- | Integer division.
ndiv :: N -> N -> N
ndiv a b = case (nsub a b) of
             Nothing -> Z
             Just c  -> S $ ndiv c b

-- | Modulo.
nmod :: N -> N -> N
nmod a b = case (nsub a b) of
             Nothing -> a 
             Just c  -> nmod c b
