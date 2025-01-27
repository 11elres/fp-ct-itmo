module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import           Data.Char (isSpace)
import           HW0.T1    (distrib)

a :: (Either String a, Either String b)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))

-- | Constructor (,) and deffered calculations in it.
a_whnf :: (Either String a, Either String b)
a_whnf = (,) (Left ("AB" ++ "CD" ++ "EF")) (Left ("AB" ++ "CD" ++ "EF"))

b :: [Bool]
b = map isSpace "Hello, World"

-- | Constructor (:) and deffered calculations
-- of applying function and mapping for tail in it.
b_whnf :: [Bool]
b_whnf = (:) (isSpace 'H') (map isSpace "ello, World")

c :: String
c = if (1 :: Int) > 0 || error "X" then "Y" else "Z"

-- | if-then-else calculation result where 'Y'
-- is calculated (because value) and [] is deffered.
c_whnf :: String
c_whnf = (:) 'Y' []
