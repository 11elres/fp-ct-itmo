module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

-- | Additional information of subtree: (size, depth).
type Meta = (Int, Int)

-- | Height balanced binary search tree with unique values. 
data Tree a 
  = Leaf      -- ^ Empty Tree.
  | Branch    -- ^ Tree with two subtrees and value.
    Meta      -- ^ additional information
    (Tree a)  -- ^ left subtree
    a         -- ^ value
    (Tree a)  -- ^ right subtree
  deriving (Show)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (size, _) _ _ _) = size

-- | Depth of the tree, O(1).
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, depth) _ _ _) = depth

-- | Check if the element is in the tree, O(tdepth tree).
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember element (Branch _ l val r) 
  | element == val = True
  | val < element  = tmember element r
  | otherwise      = tmember element l

-- | Insert an element into the tree, O(tdepth tree).
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert element Leaf = Branch (1, 1) Leaf element Leaf
tinsert element t@(Branch _ t1 val t2) 
  | element == val = t
  | element > val  = let t2' = (tinsert element t2) 
                     in mkBalanced $ mkBranch t1 val t2'
  | otherwise      = let t1' = (tinsert element t1)
                     in mkBalanced $ mkBranch t1' val t2
 
-- | Makes Tree with correct Meta by childs (with correct Meta) and value.
mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch t1 val t2 = let size  = tsize t1 + tsize t2 + 1
                         depth = (+1) $ max (tdepth t1) (tdepth t2)
                     in Branch (size, depth) t1 val t2

-- | Makes correct balanced Tree by childs (correct and balanced) and value.
mkBalanced :: Tree a -> Tree a
mkBalanced t@(Branch _ l _ r) = 
  let diff = tdepth l - tdepth r
  in if diff > 1
     then rightRotate t
     else if diff < -1
     then leftRotate t
     else t
mkBalanced t = t

-- | Makes a left rotate of the tree, if can.
leftRotate :: Tree a -> Tree a 
leftRotate t@(Branch _ _ _ (Branch _ r1 _ r2)) 
  | tdepth r1 - tdepth r2 <= 0 = smallLeftRotate t
  | otherwise                  = bigLeftRotate t
leftRotate t = t

-- | Makes a right rotate of the tree, if can.
rightRotate :: Tree a -> Tree a 
rightRotate t@(Branch _ (Branch _ l1 _ l2) _ _)
  | tdepth l1 - tdepth l2 >= 0 = smallRightRotate t
  | otherwise                  = bigRightRotate t
rightRotate t = t

-- | Makes a small left rotate of the tree, if can.
smallLeftRotate :: Tree a  -> Tree a            
smallLeftRotate (Branch _ l val (Branch _ r1 rVal r2)) = mkBranch (mkBranch l val r1) rVal r2
smallLeftRotate t = t

-- | Makes a small right rotate of the tree, if can.
smallRightRotate :: Tree a -> Tree a            
smallRightRotate (Branch _ (Branch _ l1 lVal l2) val r) = mkBranch l1 lVal (mkBranch l2 val r)
smallRightRotate t = t

-- | Makes a big left rotate of the tree, if can.
bigLeftRotate :: Tree a  -> Tree a            
bigLeftRotate (Branch _ l val r) = smallLeftRotate $ mkBranch l val (smallRightRotate r)
bigLeftRotate t = t

-- | Makes a big right rotate of the tree, if can.
bigRightRotate :: Tree a -> Tree a            
bigRightRotate (Branch _ l val r) = smallRightRotate $ mkBranch (smallLeftRotate l) val r
bigRightRotate t = t

-- | Build a tree from a list, O(n * log n).
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf
