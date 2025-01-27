{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Module with combinators for working with evaluation of operation on 'HiValue'.
module HW5.Helpers.EvalCombinators
  (
  -- * operations evaluators
    evalUnaryReturn
  , evalBinaryReturn
  , evalUnary
  , evalBinary
  -- * arity filter functions
  , requireTernary
  , requireBinary
  , requireUnary
  ) where

import           Control.Monad                  ((>=>))
import           Control.Monad.Trans.Except     (ExceptT, throwE)

import           HW5.Base                       (HiError (..), HiMonad,
                                                 HiValue (..))
import           HW5.Helpers.HiValueTranslators (HiConstructable (..))

-- | Like 'evalUnary', but evaluation doesn't throw errors.
evalUnaryReturn
  :: (HiMonad m, HiConstructable b)
  => (HiValue -> ExceptT HiError m a)
  -> (a -> b)
  -> [HiValue]
  -> ExceptT HiError m HiValue
evalUnaryReturn get eval = evalUnary get (return . eval)

-- | Like 'evalBinary', but evaluation doesn't throw errors.
evalBinaryReturn
  :: (HiMonad m, HiConstructable c)
  => (HiValue -> ExceptT HiError m a)
  -> (HiValue -> ExceptT HiError m b)
  -> (a -> b -> c)
  -> [HiValue]
  -> ExceptT HiError m HiValue
evalBinaryReturn get1 get2 eval = evalBinary get1 get2 (\a b -> return $ eval a b)

-- | Evaluates unary function with given extractor from 'HiValue'
-- and evaluator, which result can be wrapped in 'HiValue'.
evalUnary
  :: (HiMonad m, HiConstructable b)
  => (HiValue -> ExceptT HiError m a)
  -> (a -> ExceptT HiError m b)
  -> [HiValue]
  -> ExceptT HiError m HiValue
evalUnary get eval = requireUnary >=> get >=> eval >=> return . toHiValue

-- | Evaluates binary function with given extractors from 'HiValue'
-- and evaluator, which result can be wrapped in 'HiValue'.
evalBinary
  :: (HiMonad m, HiConstructable c)
  => (HiValue -> ExceptT HiError m a) -- ^ get first argument
  -> (HiValue -> ExceptT HiError m b) -- ^ get second argument
  -> (a -> b -> ExceptT HiError m c)  -- ^ evaluate needed operation
  -> [HiValue]                        -- ^ arguments
  -> ExceptT HiError m HiValue        -- ^ result
evalBinary get1 get2 eval args = do
  (v1, v2) <- requireBinary args
  a1 <- get1 v1
  a2 <- get2 v2
  toHiValue <$> eval a1 a2

-- | Requires 3 elements in list and, if not, throws 'HiErrorArityMismatch'.
requireTernary :: HiMonad m => [a] -> ExceptT HiError m (a, a, a)
requireTernary = \case
  [a1, a2, a3] -> return (a1, a2, a3)
  _ -> throwE HiErrorArityMismatch

-- | Requires 2 elements in list and, if not, throws 'HiErrorArityMismatch'.
requireBinary :: HiMonad m => [a] -> ExceptT HiError m (a, a)
requireBinary = \case
  [a1, a2] -> return (a1, a2)
  _        -> throwE HiErrorArityMismatch

-- | Requires 1 element in list and, if not, throws 'HiErrorArityMismatch'.
requireUnary :: HiMonad m => [a] -> ExceptT HiError m a
requireUnary = \case
  [a1] -> return a1
  _    -> throwE HiErrorArityMismatch
