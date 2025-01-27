{-# LANGUAGE LambdaCase #-}

module HW5.Pretty
  ( prettyValue
  ) where

import           Data.Ratio                    (denominator, numerator)
import           Data.Scientific               (fromRationalRepetendUnlimited,
                                                toRealFloat)
import           Numeric                       (showFFloat)
import           Prettyprinter                 (Doc, Pretty (pretty),
                                                concatWith, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)

import           Data.ByteString               (ByteString, unpack)
import           Data.Foldable                 (toList)
import qualified Data.Map                      as Map
import           Data.Word                     (Word8)
import           HW5.Base                      (HiAction (..), HiFun (..),
                                                HiShow (..), HiValue (..))

-- | Renders 'HiValue' into 'Doc'.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case
  HiValueDict     d -> prettyJoin "{" "}" ", " $ (\(k, v) -> prettyValue k <+> pretty ":" <+> prettyValue v) <$> Map.toList d
  HiValueTime     t -> pretty $ "parse-time(" <> show (show t) <> ")"
  HiValueAction   a -> prettyAction a
  HiValueBytes    b -> prettyBytes b
  HiValueList     l -> prettyJoin "[" "]" ", " (prettyValue <$> toList l)
  HiValueString   s -> pretty $ show s
  HiValueNull       -> pretty "null"
  HiValueBool     b -> pretty $ if b then "true" else "false"
  HiValueNumber   r -> prettyRational r
  HiValueFunction f -> prettyFunction f

-- | Renders 'HiAction' into 'Doc'.
prettyAction :: HiAction -> Doc AnsiStyle
prettyAction action = pretty (hiShow action) <>
  case action of
    HiActionRead fp      -> pretty $ "(" <> show fp <> ")"
    HiActionWrite fp bs  -> pretty ("(" <> show fp <> ", ") <> prettyBytes bs <> pretty ")"
    HiActionMkDir fp     -> pretty $ "(" <> show fp <> ")"
    HiActionChDir fp     -> pretty $ "(" <> show fp <> ")"
    HiActionRand from to -> pretty $ "(" <> show from <> ", " <> show to <> ")"
    HiActionEcho s       -> pretty $ "(" <> show s <> ")"
    _                    -> pretty ""

-- | Renders 'ByteString' into 'Doc'.
prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes b = prettyJoin "[#" "#]" " " (pretty . word8ToHexPair <$> unpack b)
  where
    word8ToHexPair :: Word8 -> String
    word8ToHexPair w = [hexDigit (w `div` 16), hexDigit (w `mod` 16)]

    hexDigit :: Word8 -> Char
    hexDigit n
      | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral (n - 10))

-- | Join 'Doc's with 'String' separator, start and end.
prettyJoin :: String -> String -> String -> [Doc AnsiStyle] -> Doc AnsiStyle
prettyJoin start end sep items = pretty start <+> concatWith (\x y -> x <> pretty sep <> y) items <+> pretty end

-- | Renders 'Rational' into 'Doc'.
prettyRational :: Rational -> Doc AnsiStyle
prettyRational x = pretty $
  let
    (n, d) = (numerator x, denominator x)
    (q, r) = quotRem n d
  in
    if d == 1 then show n
    else case fromRationalRepetendUnlimited x of
      (s, Nothing) -> showFFloat Nothing (toRealFloat s :: Double) ""
      _ | q == 0   -> show r <> "/" <> show d
      _            -> show q <> (if r < 0 then " - " else " + ") <>
                      show (abs r) <> "/" <> show d

-- | Renders 'HiFun' into 'Doc'.
prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction = pretty . hiShow
