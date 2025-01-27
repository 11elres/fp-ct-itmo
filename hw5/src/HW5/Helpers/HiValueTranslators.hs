{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Contains helper functions for convert 'HiValue' and types, wrapped by it.
module HW5.Helpers.HiValueTranslators
  (
  -- * 'HiValue' constructors
    HiConstructable(..)
  -- * 'HiValue' extractors
  , getBool
  , getByte
  , getInteger
  , getInt
  , getNum
  , getString
  , getSliceIndex
  , getFun
  , getList
  , getDict
  , getBytes
  , getTime
  ) where

import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.ByteString            (ByteString)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Ratio                 (denominator, numerator)
import           Data.Sequence              (Seq)
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime)
import           Data.Word                  (Word8)

import           HW5.Base                   (HiAction, HiError (..), HiFun,
                                             HiMonad, HiValue (..))

-- | Class for wrapping types, that is represented by 'HiValue'.
class HiConstructable a where
  toHiValue :: a -> HiValue

instance HiConstructable HiValue where
  toHiValue = id

instance HiConstructable HiFun where
  toHiValue = HiValueFunction

instance HiConstructable Rational where
  toHiValue = HiValueNumber

instance HiConstructable Bool where
  toHiValue = HiValueBool

instance HiConstructable Text where
  toHiValue = HiValueString

instance HiConstructable (Seq HiValue) where
  toHiValue = HiValueList

instance HiConstructable ByteString where
  toHiValue = HiValueBytes

instance HiConstructable UTCTime where
  toHiValue = HiValueTime

instance HiConstructable HiAction where
  toHiValue = HiValueAction

instance HiConstructable (Map.Map HiValue HiValue) where
  toHiValue = HiValueDict

-- | Extracts 'HiFun' from 'HiValue'.
getFun :: HiMonad m => HiValue -> ExceptT HiError m HiFun
getFun = \case
  HiValueFunction f -> return f
  _                 -> throwE HiErrorInvalidArgument

-- | Extracts 'Rational' from 'HiValue'.
getNum :: HiMonad m => HiValue -> ExceptT HiError m Rational
getNum = \case
  HiValueNumber n -> return n
  _               -> throwE HiErrorInvalidArgument

-- | Extracts 'Integer' from 'HiValue'.
getInteger :: HiMonad m => HiValue -> ExceptT HiError m Integer
getInteger = getIntegral (const True)

-- | Extracts 'Int' from 'HiValue'.
getInt :: HiMonad m => HiValue -> ExceptT HiError m Int
getInt = getIntegral (\n -> n >= toInteger (minBound :: Int) && n <= toInteger (maxBound :: Int))

-- | Extracts 'Word8' from 'HiValue'.
getByte :: HiMonad m => HiValue -> ExceptT HiError m Word8
getByte = getIntegral (\n -> n >= 0 && n <= 255)

-- | Auxiliary function for extracting 'Integral' types from 'HiValue'.
getIntegral :: (Integral a, HiMonad m) => (Integer -> Bool) -> HiValue -> ExceptT HiError m a
getIntegral predicate = \case
  HiValueNumber n | denominator n == 1 && predicate (numerator n)
    -> return $ fromIntegral $ numerator n
  _ -> throwE HiErrorInvalidArgument

-- | Extracts acceptable slice index from 'HiValue'.
getSliceIndex :: HiMonad m => HiValue -> ExceptT HiError m (Maybe Integer)
getSliceIndex = \case
  HiValueNumber n
    | denominator n == 1 -> return $ Just $ numerator n
  HiValueNull            -> return Nothing
  _                      -> throwE HiErrorInvalidArgument

-- | Extracts 'Bool' from 'HiValue'.
getBool :: HiMonad m => HiValue -> ExceptT HiError m Bool
getBool = \case
  HiValueBool b -> return b
  _             -> throwE HiErrorInvalidArgument

-- | Extracts 'Text' from 'HiValue'.
getString :: HiMonad m => HiValue -> ExceptT HiError m Text
getString = \case
  HiValueString s -> return s
  _               -> throwE HiErrorInvalidArgument

-- | Extracts 'Seq' from 'HiValue'.
getList :: HiMonad m => HiValue -> ExceptT HiError m (Seq HiValue)
getList = \case
  HiValueList l -> return l
  _             -> throwE HiErrorInvalidArgument

-- | Extracts 'ByteString' from 'HiValue'.
getBytes :: HiMonad m => HiValue -> ExceptT HiError m ByteString
getBytes = \case
  HiValueBytes b -> return b
  _              -> throwE HiErrorInvalidArgument

-- | Extracts 'UTCTime' from 'HiValue'.
getTime :: HiMonad m => HiValue -> ExceptT HiError m UTCTime
getTime = \case
  HiValueTime t -> return t
  _             -> throwE HiErrorInvalidArgument

-- | Extracts 'Map' from 'HiValue'.
getDict :: HiMonad m => HiValue -> ExceptT HiError m (Map HiValue HiValue)
getDict = \case
  HiValueDict d -> return d
  _             -> throwE HiErrorInvalidArgument
