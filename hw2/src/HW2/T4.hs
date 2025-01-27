module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

-- | Nonempty list.
data ListPlus a 
  = a :+ ListPlus a -- ^ head and tail
  | Last a          -- ^ last element
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (Last x)  <> ys = x :+ ys
  (x :+ xs) <> ys = x :+ (xs <> ys)

-- | Either or both.
data Inclusive a b = This a | That b | Both a b
  deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (This a1)    <> (This a2)    = This (a1 <> a2)
  (This a1)    <> (That b2)    = Both a1 b2
  (This a1)    <> (Both a2 b2) = Both (a1 <> a2) b2
  (That b1)    <> (This a2)    = Both a2 b1
  (That b1)    <> (That b2)    = That (b1 <> b2)
  (That b1)    <> (Both a2 b2) = Both a2 (b1 <> b2)
  (Both a1 b1) <> (This a2)    = Both (a1 <> a2) b1
  (Both a1 b1) <> (That b2)    = Both a1 (b1 <> b2)
  (Both a1 b1) <> (Both a2 b2) = Both (a1 <> a2) (b1 <> b2)

-- | Wrapped String with apply operation by concatenation with '.'.
newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  (DS "") <> ds      = ds
  ds      <> (DS "") = ds
  (DS s1) <> (DS s2) = DS $ s1 ++ "." ++ s2

instance Monoid DotString where
  mempty = DS ""

-- | Wrapped function.
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (F f) <> (F g) = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
 