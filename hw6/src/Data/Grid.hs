-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gWrite
  , gLeft
  , gRight
  , gUp
  , gDown
  ) where

import           Control.Comonad (Comonad (..))

import           Data.ListZipper

-- | Grid: 'ListZipper' of 'ListZipper's.
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f = Grid . fmap (fmap f) . unGrid

instance Comonad Grid where
  extract = extract . extract . unGrid

  extend f = fmap f . Grid . fmap gHorizontal . gVertical

-- | Replaces the focus with the given value.
gWrite :: a -> Grid a -> Grid a
gWrite x (Grid g) = Grid $ lWrite newLine g
  where
    oldLine = extract g
    newLine = lWrite x oldLine

-- | Move the 'Grid' up.
gUp :: Grid a -> Grid a
gUp   (Grid g) = Grid (lLeft  g)

-- | Move the 'Grid' down.
gDown :: Grid a -> Grid a
gDown (Grid g) = Grid (lRight g)

-- | Move the 'Grid' to the left.
gLeft :: Grid a -> Grid a
gLeft  (Grid g) = Grid (fmap lLeft  g)

-- | Move the 'Grid' to the right.
gRight :: Grid a -> Grid a
gRight (Grid g) = Grid (fmap lRight g)

-- | Auxiliary function.
gHorizontal, gVertical :: Grid a -> ListZipper (Grid a)
gHorizontal = lGenerator gLeft gRight
gVertical   = lGenerator gUp   gDown
