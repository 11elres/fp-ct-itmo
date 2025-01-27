module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

-- | Binary tree.
data Tree a
  = Leaf     -- ^ empty tree
  | Branch
    Int      -- ^ tree size
    (Tree a) -- ^ left subtree
    a        -- ^ value at the root of this tree
    (Tree a) -- ^ right subtree
  deriving (Show)

-- | Fold tree in order (for Tree with Sorted invariant).
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr f acc (Branch _ l val r) = tfoldr f (f val $ tfoldr f acc r) l
tfoldr _ acc Leaf               = acc
