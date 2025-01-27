{-# LANGUAGE LambdaCase #-}

module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import Control.Monad(ap)
import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f s = S $ \x -> mapAnnotated f $ runS s x

wrapState :: a -> State s a
wrapState x = S $ \s -> x :# s

joinState :: State s (State s a) -> State s a
joinState s = S $ \x -> let val :# s' = runS s x in 
                          runS val s'

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \x -> () :# f x

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) = Control.Monad.ap    

instance Monad (State s) where
  m >>= f = joinState (mapState f m)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) x y = Op (Add x y)
  (-) x y = Op (Sub x y)
  (*) x y = Op (Mul x y)
  abs = Op . Abs
  signum = Op . Sgn
  fromInteger = Val . fromInteger 

instance Fractional Expr where
  (/) x y = Op (Div x y)
  fromRational = Val . fromRational 

eval :: Expr -> State [Prim Double] Double
eval = \case 
  Op (Add a b) -> evalBinary Add a b
  Op (Sub a b) -> evalBinary Sub a b
  Op (Mul a b) -> evalBinary Mul a b
  Op (Div a b) -> evalBinary Div a b
  Op (Abs a)   -> evalUnary Abs a
  Op (Sgn a)   -> evalUnary Sgn a
  Val a        -> pure a

calculate :: Prim Double -> Double
calculate = \case
  (Add a b) -> a + b
  (Sub a b) -> a - b
  (Mul a b) -> a * b
  (Div a b) -> a / b
  (Abs a)   -> abs a
  (Sgn a)   -> signum a

constructState :: Prim Double -> State [Prim Double] Double
constructState op = 
  do modifyState (op:)
     pure $ calculate op 

evalUnary :: (Double -> Prim Double) -> Expr -> State [Prim Double] Double
evalUnary name expr = 
  do ans <- eval expr
     constructState $ name ans

evalBinary :: (Double -> Double -> Prim Double) -> Expr -> Expr -> State [Prim Double] Double
evalBinary name aExpr bExpr = 
  do aAns <- eval aExpr
     bAns <- eval bExpr
     constructState $ name aAns bAns
