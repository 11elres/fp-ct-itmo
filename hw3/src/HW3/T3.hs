{-# LANGUAGE LambdaCase #-}

module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption = \case
  None     -> None
  (Some x) -> x

joinExcept :: Except e (Except e a) -> Except e a
joinExcept = \case 
  (Error x)   -> Error x
  (Success x) -> x

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((x :# e1) :# e2) = x :# e2 <> e1
  
(++.) :: List a -> List a -> List a
Nil ++. ys       = ys 
(x :. xs) ++. ys = x :. (xs ++. ys)

infixr 5 ++.

joinList :: List (List a) -> List a
joinList Nil      = Nil
joinList (h :. t) = h ++. joinList t

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> let F f' = f x in f' x) 
