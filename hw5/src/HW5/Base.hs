{-# LANGUAGE DeriveGeneric #-}

module HW5.Base
  (
  -- * Types
    HiError(..)
  , HiExpr(..)
  , HiFun(..)
  , HiValue(..)
  , HiAction(..)
  -- * Classes
  , HiMonad(..)
  , HiShow(..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.ByteString (ByteString)
import           Data.Map        (Map)
import           Data.Sequence   (Seq)
import           Data.Text       (Text)
import           Data.Time       (UTCTime)
import           GHC.Generics    (Generic)

-- | Actions, that can be performed by 'HiMonad' 's instance.
data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Generic, Ord, Show)

-- | Class for performing some hi-language actions.
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

-- | Representation for acceptable hi-language function names.
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunNot
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Enum, Eq, Generic, Ord, Show)

-- | Hi-language values representation.
data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Generic, Ord, Show)

-- | Hi-language expression representation.
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

-- | Evaluation hi-language errors.
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

instance Serialise HiValue where
instance Serialise HiFun where
instance Serialise HiAction where

-- | Auxiliary class for reading-writing some hi-language elements.
class HiShow a where
  hiShow :: a -> String

instance HiShow HiFun where
  hiShow HiFunDiv            = "div"
  hiShow HiFunMul            = "mul"
  hiShow HiFunAdd            = "add"
  hiShow HiFunSub            = "sub"
  hiShow HiFunNot            = "not"
  hiShow HiFunAnd            = "and"
  hiShow HiFunOr             = "or"
  hiShow HiFunLessThan       = "less-than"
  hiShow HiFunGreaterThan    = "greater-than"
  hiShow HiFunEquals         = "equals"
  hiShow HiFunNotLessThan    = "not-less-than"
  hiShow HiFunNotGreaterThan = "not-greater-than"
  hiShow HiFunNotEquals      = "not-equals"
  hiShow HiFunIf             = "if"
  hiShow HiFunLength         = "length"
  hiShow HiFunToUpper        = "to-upper"
  hiShow HiFunToLower        = "to-lower"
  hiShow HiFunReverse        = "reverse"
  hiShow HiFunTrim           = "trim"
  hiShow HiFunList           = "list"
  hiShow HiFunRange          = "range"
  hiShow HiFunFold           = "fold"
  hiShow HiFunPackBytes      = "pack-bytes"
  hiShow HiFunUnpackBytes    = "unpack-bytes"
  hiShow HiFunEncodeUtf8     = "encode-utf8"
  hiShow HiFunDecodeUtf8     = "decode-utf8"
  hiShow HiFunZip            = "zip"
  hiShow HiFunUnzip          = "unzip"
  hiShow HiFunSerialise      = "serialise"
  hiShow HiFunDeserialise    = "deserialise"
  hiShow HiFunRead           = "read"
  hiShow HiFunWrite          = "write"
  hiShow HiFunMkDir          = "mkdir"
  hiShow HiFunChDir          = "cd"
  hiShow HiFunParseTime      = "parse-time"
  hiShow HiFunRand           = "rand"
  hiShow HiFunEcho           = "echo"
  hiShow HiFunCount          = "count"
  hiShow HiFunKeys           = "keys"
  hiShow HiFunValues         = "values"
  hiShow HiFunInvert         = "invert"

instance HiShow HiAction where
  hiShow (HiActionRead _)    = "read"
  hiShow (HiActionWrite _ _) = "write"
  hiShow (HiActionMkDir _)   = "mkdir"
  hiShow (HiActionChDir _)   = "cd"
  hiShow HiActionCwd         = "cwd"
  hiShow HiActionNow         = "now"
  hiShow (HiActionRand _ _)  = "rand"
  hiShow (HiActionEcho _)    = "echo"
