{-# LANGUAGE LambdaCase #-}

module HW3.T2
  ( distOption
  , wrapOption
  , distPair
  , wrapPair
  , distQuad
  , wrapQuad
  , distAnnotated
  , wrapAnnotated
  , distExcept
  , wrapExcept
  , distPrioritised
  , wrapPrioritised
  , distStream
  , wrapStream
  , distList
  , wrapList
  , distFun
  , wrapFun
  ) where

import HW3.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some x, Some y) = Some (x, y)
distOption _                = None

wrapOption :: a -> Option a
wrapOption = Some

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2) 

wrapPair :: a -> Pair a
wrapPair x = P x x

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

wrapQuad :: a -> Quad a
wrapQuad x = Q x x x x

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x :# e1, y :# e2) = (x, y) :# e1 <> e2

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated = (:# mempty)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept = \case
  (Error x, _)          -> Error x
  (_, Error y)          -> Error y
  (Success x, Success y) -> Success (x, y)

wrapExcept :: a -> Except e a
wrapExcept = Success

getPrioritised :: Prioritised a -> a
getPrioritised = \case
  (Low x)    -> x
  (Medium x) -> x 
  (High x)   -> x

getPrioritisedPair :: (Prioritised a, Prioritised b) -> (a, b)
getPrioritisedPair (x, y) = (getPrioritised x, getPrioritised y)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised pPair =  
  (case pPair of
    (High _, _)     -> High
    (_, (High _))   -> High
    (Medium _, _)   -> Medium
    (_, (Medium _)) -> Medium
    _               -> Low)
  (getPrioritisedPair pPair)

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (x1 :> s1, x2 :> s2) = (x1, x2) :> distStream (s1, s2)

wrapStream :: a -> Stream a
wrapStream x = x :> wrapStream x 

helpDistList :: List a -> List b -> List b -> List (a, b)
helpDistList Nil _ _                   = Nil 
helpDistList (_ :. t) Nil y            = helpDistList t y y 
helpDistList x'@(xh :. _) (yh :. yt) y = (xh, yh) :. (helpDistList x' yt y)

distList :: (List a, List b) -> List (a, b)
distList (x, y) = helpDistList x y y

wrapList :: a -> List a
wrapList = (:. Nil)

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f1, F f2) = F (\x -> (f1 x, f2 x))

wrapFun :: a -> Fun i a
wrapFun x = F (\_ -> x)
