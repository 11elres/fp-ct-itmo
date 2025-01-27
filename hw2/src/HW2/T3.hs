module HW2.T3
  ( epart
  , mcat
  ) where

-- | Apply all Just elements in list.
mcat :: Monoid a => [Maybe a] -> a
mcat = foldr (\m acc -> case m of
                          Nothing  -> acc
                          Just val -> val <> acc) 
             mempty

-- | Apply all Left and Right elements in list separately.
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr (\e (l, r) -> case e of
                              (Left val)  -> (val <> l, r)
                              (Right val) -> (l, val <> r)) 
              (mempty, mempty)
