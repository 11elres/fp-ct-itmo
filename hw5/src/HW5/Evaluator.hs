{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module HW5.Evaluator
  ( eval
  ) where

import qualified Codec.Compression.Zlib         as Zlib
import           Codec.Serialise                (deserialiseOrFail, serialise)
import           Control.Monad                  (foldM, forM)
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Control.Monad.Trans.Except
import qualified Data.ByteString                as ByteString
import           Data.Either                    (fromRight)
import           Data.Foldable                  (toList)
import           Data.Function                  ((&))
import           Data.List.NonEmpty             (NonEmpty (..), fromList)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Semigroup                 (Semigroup (stimes))
import qualified Data.Sequence                  as Seq
import qualified Data.Text                      as Text
import           Data.Text.Encoding             (decodeUtf8', encodeUtf8)
import           Data.Time                      (addUTCTime, diffUTCTime,
                                                 secondsToNominalDiffTime)
import           Text.Read                      (readMaybe)

import           HW5.Base
import           HW5.Helpers.EvalCombinators
import           HW5.Helpers.HiValueTranslators
import           HW5.Helpers.Indexable

-- | Evaluates 'HiExpr'.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . eval'

-- | Auxiliary function for 'eval'.
eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' = \case
  HiExprValue v               -> return v

  HiExprApply fExpr argsExprs -> do
    f <- eval' fExpr
    argsExprs & case f of
      HiValueFunction f' -> evalFun f'
      HiValueString s    -> evalIndexed s
      HiValueList l      -> evalIndexed l
      HiValueBytes b     -> evalIndexed b
      HiValueDict d      -> evalDict d
      _                  -> const $ throwE HiErrorInvalidFunction

  HiExprRun aExpr             ->
    eval' aExpr >>= \case
      HiValueAction a -> lift $ runAction a
      _               -> throwE HiErrorInvalidArgument

  HiExprDict pairs            ->
    fmap (HiValueDict . Map.fromList) $
      forM pairs $ \(k, v) -> do
        k' <- eval' k
        v' <- eval' v
        return (k', v')

-- | Get element from 'HiIndexable' with given index or slice.
evalIndexed :: (HiConstructable a, HiIndexable a, HiMonad m) => a -> [HiExpr] -> ExceptT HiError m HiValue
evalIndexed = strictEval . indexableEvaluators

-- | Get element from 'HiDict' with given key.
evalDict :: HiMonad m => Map.Map HiValue HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalDict d = strictEval1 $ evalUnaryReturn return (fromMaybe HiValueNull . (`Map.lookup` d))

-- | Applies arguments with given 'HiFun'.
evalFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalFun = \case
  -- Arithmetic-like functions.
  HiFunDiv            -> strictEval $
      evalBinaryReturn getString getString (\s1 s2 -> s1 <> Text.pack "/" <> s2) :|
    [ evalBinary getNum getNum $ \n1 n2 ->
        if n2 == 0 then throwE HiErrorDivideByZero
        else return $ n1 / n2 ]

  HiFunMul            -> strictEval $
      evalBinaryReturn getNum getNum (*) :|
    [ evalBinaryReturn getString getInt    (flip Text.replicate)
    , evalBinaryReturn getList getInteger  (flip stimes)
    , evalBinaryReturn getBytes getInteger (flip stimes)]

  HiFunAdd            -> strictEval $
      evalBinaryReturn getNum getNum (+) :|
    [ evalBinaryReturn getString getString Text.append
    , evalBinaryReturn getList getList (Seq.><)
    , evalBinaryReturn getBytes getBytes ByteString.append
    , evalBinaryReturn getTime getNum $
        \t n -> addUTCTime (secondsToNominalDiffTime $ realToFrac n) t]

  HiFunSub            -> strictEval $
      evalBinaryReturn getNum getNum (-) :|
    [ evalBinaryReturn getTime getTime $
        \t1 t2 -> toRational $ diffUTCTime t1 t2 ]

  -- Logic functions.
  HiFunAnd            -> \exprs -> (do
    (expr1, expr2) <- requireBinary exprs
    a1 <- eval' expr1
    case a1 of
      (HiValueBool False) -> return a1
      HiValueNull         -> return a1
      _                   -> eval' expr2)

  HiFunOr             -> \exprs -> (do
    (expr1, expr2) <- requireBinary exprs
    a1 <- eval' expr1
    case a1 of
      (HiValueBool False) -> eval' expr2
      HiValueNull         -> eval' expr2
      _                   -> return a1)

  HiFunNot            -> strictEval1 $ evalUnaryReturn getBool not

  -- Comparation functions.
  HiFunLessThan       -> compareWith (<)
  HiFunGreaterThan    -> compareWith (>)
  HiFunEquals         -> compareWith (==)
  HiFunNotLessThan    -> compareWith (>=)
  HiFunNotGreaterThan -> compareWith (<=)
  HiFunNotEquals      -> compareWith (/=)

  HiFunIf             -> \exprs -> (do
    (expr1, expr2, expr3) <- requireTernary exprs
    a1 <- eval' expr1
    case a1 of
      HiValueBool b -> if b then eval' expr2 else eval' expr3
      _             -> throwE HiErrorInvalidArgument)

  -- String/list functions.
  HiFunToUpper        -> strictEval1 $ evalUnaryReturn getString Text.toUpper
  HiFunToLower        -> strictEval1 $ evalUnaryReturn getString Text.toLower
  HiFunTrim           -> strictEval1 $ evalUnaryReturn getString Text.strip
  HiFunRange          -> strictEval1 $ evalBinaryReturn getNum getNum (\a b -> Seq.fromList $ toHiValue <$> [a .. b])
  HiFunList           -> fmap (toHiValue . Seq.fromList) . mapM eval'

  HiFunLength         -> strictEval $
      evalUnaryReturn getString (toRational . Text.length) :|
    [ evalUnaryReturn getList (toRational . Seq.length) ]

  HiFunReverse        -> strictEval $
      evalUnaryReturn getString Text.reverse :|
    [ evalUnaryReturn getList Seq.reverse ]

  HiFunFold           -> strictEval1 $ evalBinary getFun getList $ \f -> \case
    Seq.Empty -> throwE HiErrorInvalidArgument
    (h Seq.:<| t) -> foldM (\acc v -> evalFun f [HiExprValue acc, HiExprValue v]) h t

  -- Bytes functions.
  HiFunPackBytes   -> strictEval1 $ evalUnary       getList   (fmap (ByteString.pack . toList) . mapM getByte)
  HiFunUnpackBytes -> strictEval1 $ evalUnaryReturn getBytes  (Seq.fromList . fmap (toHiValue . toRational) . ByteString.unpack)
  HiFunEncodeUtf8  -> strictEval1 $ evalUnaryReturn getString encodeUtf8
  HiFunDecodeUtf8  -> strictEval1 $ evalUnaryReturn getBytes  (either (const HiValueNull) HiValueString . decodeUtf8')
  HiFunZip         -> strictEval1 $ evalUnaryReturn getBytes  (ByteString.toStrict . Zlib.compressWith Zlib.defaultCompressParams
                                                              { Zlib.compressLevel = Zlib.bestCompression } . ByteString.fromStrict)
  HiFunUnzip       -> strictEval1 $ evalUnaryReturn getBytes  (ByteString.toStrict . Zlib.decompress . ByteString.fromStrict)
  HiFunSerialise   -> strictEval1 $ evalUnaryReturn return    (ByteString.toStrict . serialise)
  HiFunDeserialise -> strictEval1 $ evalUnaryReturn getBytes  (fromRight HiValueNull . deserialiseOrFail . ByteString.fromStrict)

  -- Action functions.
  HiFunRead      -> strictEval1 $ evalUnaryReturn getString            (HiActionRead . Text.unpack)
  HiFunWrite     -> strictEval1 $ evalBinaryReturn getString getString (\s1 s2 -> HiActionWrite (Text.unpack s1) (encodeUtf8 s2))
  HiFunMkDir     -> strictEval1 $ evalUnaryReturn getString            (HiActionMkDir . Text.unpack)
  HiFunChDir     -> strictEval1 $ evalUnaryReturn getString            (HiActionChDir . Text.unpack)
  HiFunParseTime -> strictEval1 $ evalUnaryReturn getString            (maybe HiValueNull HiValueTime . readMaybe . Text.unpack)
  HiFunRand      -> strictEval1 $ evalBinaryReturn getInt getInt       HiActionRand
  HiFunEcho      -> strictEval1 $ evalUnaryReturn getString            HiActionEcho

  -- Dict functions.
  HiFunCount  -> strictEval $
      evalUnaryReturn getList   (countOccurrences toList) :|
    [ evalUnaryReturn getString (countOccurrences $ fmap (HiValueString . Text.singleton) . Text.unpack)
    , evalUnaryReturn getBytes  (countOccurrences $ fmap (toHiValue . toRational) . ByteString.unpack) ]
     where
      countOccurrences :: (a -> [HiValue]) -> a -> Map.Map HiValue HiValue
      countOccurrences toHVList xs = Map.map HiValueNumber $ Map.fromListWith (+) $ map (, 1) $ toHVList xs
  HiFunKeys   -> strictEval1 $ evalUnaryReturn getDict (Seq.fromList . Map.keys)
  HiFunValues -> strictEval1 $ evalUnaryReturn getDict (Seq.fromList . Map.elems)
  HiFunInvert -> strictEval1 $ evalUnaryReturn getDict invert
    where
      invert :: Map.Map HiValue HiValue -> Map.Map HiValue HiValue
      invert = fmap (toHiValue . Seq.fromList) . Map.fromListWith (++) . map (\(k, v) -> (v, [k])) . Map.toList

-- | Applies predicate to two 'HiValue' arguments.
compareWith :: HiMonad m => (HiValue -> HiValue -> Bool) -> [HiExpr] -> ExceptT HiError m HiValue
compareWith = strictEval1 . evalBinaryReturn return return

-- | 'strictEval' with one evaluator.
strictEval1 :: HiMonad m => ([HiValue] -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
strictEval1 f = strictEval (f :| [])

-- | Evaluates arguments in '[HiExpr]' and tries to apply given evaluators
-- for get succesful 'HiValue' result from given srgument.
strictEval
  :: HiMonad m
  => NonEmpty ([HiValue] -> ExceptT HiError m HiValue) -- ^ evaluators
  -> [HiExpr]                                          -- ^ arguments
  -> ExceptT HiError m HiValue                         -- ^ first successful result
strictEval evaluators args = do
  args' <- mapM eval' args
  strictEval' evaluators args'
    where
      strictEval'
        :: HiMonad m
        => NonEmpty ([HiValue] -> ExceptT HiError m HiValue)
        -> [HiValue]
        -> ExceptT HiError m HiValue
      strictEval' (f :| fs) args' = catchE (f args') (\e -> if null fs then throwE e
                                                            else strictEval' (fromList fs) args')
