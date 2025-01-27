module HW5.Parser
  ( HW5.Parser.parse
  ) where

import           Control.Monad.Combinators.Expr (Operator (InfixL, InfixN, InfixR),
                                                 makeExprParser)
import qualified Data.ByteString                as ByteString
import           Data.Char                      (digitToInt, isAlpha,
                                                 isAlphaNum)
import           Data.List                      (intercalate)
import qualified Data.Text                      as Text
import           Data.Void                      (Void)
import           Data.Word                      (Word8)
import           Text.Megaparsec
import           Text.Megaparsec.Char           (char, hexDigitChar, space1,
                                                 string)
import           Text.Megaparsec.Char.Lexer     (charLiteral, scientific,
                                                 signed, space)

import           HW5.Base

-- | Parses 'String' into 'HiExpr'.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (parseExpr <* eof) ""

type Parser = Parsec Void String

-- | Main 'HiExpr' parser.
parseExpr :: Parser HiExpr
parseExpr = makeExprParser (ignoreSpaces term) operatorsTable

-- | Term parser: a value or a parenthesized expression
-- with optional applications (arguments, dot-access, run action).
term :: Parser HiExpr
term = (try pValue <|> parens "(" ")" parseExpr) >>= pApplication

-- | Table of operators.
operatorsTable :: [[Operator Parser HiExpr]]
operatorsTable =
  [ [ InfixL $ try $ withFun HiFunDiv <$ string "/" <* notFollowedBy (string "=")
    , infixL "*" (withFun HiFunMul)]

  , [ infixL "+" (withFun HiFunAdd)
    , infixL "-" (withFun HiFunSub)]

  , [ infixN "<=" (withFun HiFunNotGreaterThan)
    , infixN ">=" (withFun HiFunNotLessThan)
    , infixN "<"  (withFun HiFunLessThan)
    , infixN ">"  (withFun HiFunGreaterThan)
    , infixN "==" (withFun HiFunEquals)
    , infixN "/=" (withFun HiFunNotEquals)]

  , [ infixR "&&" (withFun HiFunAnd)]
  , [ infixR "||" (withFun HiFunOr)]
  ]

-- | Constructs 'HiExpr' with given function and arguments.
withFun :: HiFun -> HiExpr -> HiExpr -> HiExpr
withFun f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]

-- | Auxiliary function.
infixL, infixR, infixN :: MonadParsec e s m => Tokens s -> (a -> a -> a) -> Operator m a
infixL = infixOp InfixL
infixR = infixOp InfixR
infixN = infixOp InfixN

-- | Auxiliary infix operator constructor.
infixOp
  :: MonadParsec e s m
  => (m (a -> a -> a) -> Operator m a) -- ^ Infix operator combinator
  -> Tokens s                          -- ^ Representation of the operator
  -> (a -> a -> a)                     -- ^ Expression constructor
  -> Operator m a                      -- ^ Returning operator
infixOp infixConstruct name f = infixConstruct $ try $ f <$ string name

-- | Parses applications (arguments, dot-access, run action) and apply it to given 'HiExpr'.
pApplication :: HiExpr -> Parser HiExpr
pApplication f =
      (try (pArguments f) >>= pApplication)
  <|> (try (pDotAccess f) >>= pApplication)
  <|> (try (pRun f) >>= pApplication)
  <|> return f

-- | Parses a run and apply it to given 'HiExpr'.
pRun :: HiExpr -> Parser HiExpr
pRun f = HiExprRun f <$ char '!'

-- | Parses a dot access and apply it to given 'HiExpr'.
pDotAccess :: HiExpr -> Parser HiExpr
pDotAccess f = fmap
  (HiExprApply f . return . HiExprValue . HiValueString . Text.pack . intercalate "-")
  (char '.' *> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'))

-- | Parses arguments and apply it to given 'HiExpr'.
pArguments :: HiExpr -> Parser HiExpr
pArguments f = fmap (HiExprApply f) $ ignoreSpaces $
  parens "(" ")" (parseExpr `sepBy` ignoreSpaces (char ','))

-- | Parses values.
pValue :: Parser HiExpr
pValue =
      try pDict
  <|> try pList
  <|> fmap HiExprValue (try pFunction
                    <|> try pNumber
                    <|> try pBool
                    <|> try pNull
                    <|> try pString
                    <|> try pByteString
                    <|> pAction)

-- | Parses a 'HiExprDict'.
pDict :: Parser HiExpr
pDict = HiExprDict <$> parens "{" "}" (pEntry `sepBy` ignoreSpaces (char ','))
  where
    pEntry :: Parser (HiExpr, HiExpr)
    pEntry = do
      key   <- parseExpr
      _     <- ignoreSpaces $ char ':'
      value <- parseExpr
      return (key, value)

-- | Parses a no-argument 'HiAction's to a 'HiValue'.
pAction :: Parser HiValue
pAction = HiValueAction <$> choice
  [ HiActionNow <$ string "now"
  , HiActionCwd <$ string "cwd"]

-- | Parses a 'HiValueBytes'.
pByteString :: Parser HiValue
pByteString = HiValueBytes . ByteString.pack <$>
  parens "[#" "#]" (pByte `sepEndBy` space1 <* sc)
  where
    pByte :: Parser Word8
    pByte = do
      c1 <- pHex
      c2 <- pHex
      return $ fromIntegral $ c1 * 16 + c2
    pHex :: Parser Int
    pHex = digitToInt <$> hexDigitChar

-- | Parses a 'HiExprList'.
pList :: Parser HiExpr
pList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$>
  parens "[" "]" (parseExpr `sepBy` ignoreSpaces (char ','))

-- | Parses a 'HiValueString'.
pString :: Parser HiValue
pString = fmap (HiValueString . Text.pack) $
  char '"' >> manyTill charLiteral (char '"')

-- | Parses a 'HiValueNull'.
pNull :: Parser HiValue
pNull = HiValueNull <$ string "null"

-- | Parses a 'HiValueBool'.
pBool :: Parser HiValue
pBool = HiValueBool <$> choice
  [ True <$ string "true"
  , False <$ string "false"]

-- | Parses a 'HiValueNumber'.
pNumber :: Parser HiValue
pNumber = HiValueNumber . toRational <$> signed sc scientific

-- | Parses a 'HiFun'.
pFunction :: Parser HiValue
pFunction = fmap HiValueFunction $ choice $
  map (\x -> x <$ string (hiShow x)) [HiFunDiv ..]

-- | Parses an expression between two delimiters.
parens :: String -> String -> Parser a -> Parser a
parens open close = between (string open <* sc) (sc *> string close)

-- | Parses value by given parser, ignoring any spaces around.
ignoreSpaces :: Parser a -> Parser a
ignoreSpaces = between sc sc

-- | Parses any number of spaces.
sc :: Parser ()
sc = space space1 empty empty


