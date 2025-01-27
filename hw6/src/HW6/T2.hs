{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete
  ) where

import           GHC.TypeLits

-- | Type alias for 'Symbol' type set.
type TSet = [Symbol]

-- | Check if 'name' is in 'set'.
type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name (name ': _) = 'True
  Contains name (_ ': xs)   = Contains name xs
  Contains _ '[]            = 'False

-- | Delete 'name' from 'set'.
type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete name (name ': xs) = xs
  Delete name (x ': xs)    = x ': Delete name xs
  Delete name '[]          = '[]

-- | Add 'name' to 'set', if not present.
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add name (name ': xs) = name ': xs
  Add name (x ': xs)    = x ': Add name xs
  Add name '[]          = '[name]
