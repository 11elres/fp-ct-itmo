{-# LANGUAGE FlexibleInstances #-}

-- | Module for working with indexable 'HiValue' types.
module HW5.Helpers.Indexable
  ( indexableEvaluators
  , HiIndexable
  ) where

import           Control.Monad.Trans.Except     (ExceptT, throwE)
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.List.NonEmpty             (NonEmpty (..))
import           Data.Maybe                     (fromMaybe)
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as DS
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Prelude                        hiding (drop, length, take)

import           HW5.Base
import           HW5.Helpers.EvalCombinators
import           HW5.Helpers.HiValueTranslators

-- | Class for indexing of indexable 'HiValue' types.
class HiIndexable a where
  empty  :: a
  take   :: Int -> a -> a
  drop   :: Int -> a -> a
  length :: a -> Int
  index  :: a -> Int -> HiValue

instance HiIndexable Text where
  empty  = T.empty
  take   = T.take
  drop   = T.drop
  length = T.length
  index  s i = toHiValue $ T.singleton $ T.index s i

instance HiIndexable (Seq HiValue) where
  empty  = DS.empty
  take   = DS.take
  drop   = DS.drop
  length = DS.length
  index  = DS.index

instance HiIndexable ByteString where
  empty  = BS.empty
  take   = BS.take
  drop   = BS.drop
  length = BS.length
  index b i = toHiValue $ toRational $ BS.index b i

-- | By 'HiIndexable' return list of evaluators: getting by index and slicing.
indexableEvaluators :: (HiConstructable a, HiIndexable a, HiMonad m) => a -> NonEmpty ([HiValue] -> ExceptT HiError m HiValue)
indexableEvaluators s =
    getByIndex s :|
  [ sliceArguments s
  , const $ throwE HiErrorArityMismatch
  ]

-- | Get element from 'HiIndexable' with given index.
getByIndex :: (HiIndexable a, HiMonad m) => a -> [HiValue] -> ExceptT HiError m HiValue
getByIndex s = evalUnaryReturn getInteger $ \idx ->
  if idx >= 0 && idx < fromIntegral (length s)
  then index s $ fromIntegral idx
  else HiValueNull

-- | Slice 'HiIndexable' with given arguments.
sliceArguments :: (HiConstructable a, HiIndexable a, HiMonad m) => a -> [HiValue] -> ExceptT HiError m HiValue
sliceArguments = evalBinaryReturn getSliceIndex getSliceIndex . slice

-- | Auxiliary function for slicing 'HiIndexable'.
slice :: (HiIndexable a) => a -> Maybe Integer -> Maybe Integer -> a
slice s idx1 idx2
  | start > end || start >= len = empty
  | checkStart && checkEnd      = take (end - start) (drop start s)
  | checkStart && not checkEnd  = drop start s
  | not checkStart && checkEnd  = take end s
  | otherwise                   = s
  where
    checkIndex idx = idx >= 0 && idx < len
    checkStart     = checkIndex start
    checkEnd       = checkIndex end
    len            = length s
    up n           = if n < 0 then n + len else n
    start          = up $ fromIntegral $ fromMaybe 0 idx1 :: Int
    end            = up $ fromIntegral $ fromMaybe (fromIntegral len) idx2 :: Int
