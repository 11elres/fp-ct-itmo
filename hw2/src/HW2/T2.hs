module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty(..))

-- | Split a list into sublists by separator.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr (\c (h :| t) -> if c == sep 
                                    then [] :| (h:t) 
                                    else (c:h) :| t) 
                    ([] :| []) 

-- | Join lists into completed list by separator.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldr1 (\word acc -> word ++ (sep:acc))
