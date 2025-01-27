{-# LANGUAGE LambdaCase #-}

module HW4.T1
  ( -- * Datatypes
    EvaluationError (..)
  , ExceptState (..)
    -- * functions
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad(ap)
import HW4.Types

-- | State monad with exception.
data ExceptState e s a = ES 
  { runES :: s -> Except e (Annotated s a) -- ^ running function
  }

-- | By a function maps value inside of 'ExceptState' context.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s -> case runES es s of
  Error e           -> Error e
  Success (a :# s') -> Success $ f a :# s'

-- | Wraps value into 'ExceptState'.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success $ a :# s

-- | Runs given 'ExceptState' and by this result, 
-- if calculation was successful, runs nested 'ExceptState' 
-- with state of gotten result, else return gotten 'Error'.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState mm = ES $ \s -> case runES mm s of
  Error e           -> Error e
  Success (m :# s') -> runES m s'

-- | Changes state by given function.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success $ () :# f s

-- | Raises given error.
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = Control.Monad.ap

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero
  deriving Show

-- | Typealias for 'Expr' evaluation with 'ExceptState'.
type ExprEvaluation = ExceptState EvaluationError [Prim Double] Double

-- | Evaluates expression. 
-- 
-- Returns result annotated by list of performed operations in 'ExceptState' context.
eval :: Expr -> ExprEvaluation
eval = \case
  Op (Add a b) -> evalBinary Add a b
  Op (Sub a b) -> evalBinary Sub a b
  Op (Mul a b) -> evalBinary Mul a b
  Op (Div a b) -> evalBinary Div a b
  Op (Abs a)   -> evalUnary Abs a
  Op (Sgn a)   -> evalUnary Sgn a
  Val a        -> pure a

-- | Calculates result by 'Prim'.
calculate :: Prim Double -> ExprEvaluation
calculate = \case
  Div _ b | b == 0 -> throwExceptState DivideByZero
  op               -> return $ case op of
    Add a b -> a + b
    Sub a b -> a - b
    Mul a b -> a * b
    Div a b -> a / b
    Abs a   -> abs a
    Sgn a   -> signum a

-- | Remembers operation and returns result of it.
constructState :: Prim Double -> ExprEvaluation
constructState op = modifyExceptState (op:) >> calculate op

-- | Evaluates unary operation.
evalUnary :: (Double -> Prim Double) -> Expr -> ExprEvaluation
evalUnary name expr = eval expr >>= constructState . name

-- | Evaluates binary operation.
evalBinary :: (Double -> Double -> Prim Double) -> Expr -> Expr -> ExprEvaluation
evalBinary name aExpr bExpr = do
  a <- eval aExpr
  b <- eval bExpr
  constructState $ name a b
