{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  ) where

import Numeric.Natural ( Natural ) 
import Control.Applicative ( Alternative(..), optional )
import Control.Monad ( MonadPlus, msum )
import Data.Char ( isDigit, isSpace, digitToInt ) 
import Data.Maybe ( fromMaybe )
import Data.Foldable ( Foldable(foldr', foldl') ) 

import HW4.Types
import HW4.T1 ( ExceptState(..) ) 

-- | Parsing error.
data ParseError = ErrorAtPos Natural -- ^ Error with position where the parsing error occurred
  deriving Show

-- | Parser that parse exemplar of given type by begin of 'String'.
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Try parse 'String' by 'Parser'.
runP
  :: Parser a             -- ^ 'Parser'
  -> String               -- ^ 'Parser' input
  -> Except ParseError a  -- ^ Either result if succesful parsing or 'ParseError'
runP (P es) s = case runES es (0, s) of
  Error e -> Error e
  -- Success (a :# _) -> Success a
  Success (a :# (n, s')) -> case s' of
    "" -> Success a
    _  -> Error $ ErrorAtPos n

-- pChar isn't using as basic combinator below, 
-- because with it we got not good parse messages
-- e.g. runP (mfilter (=='a') pChar) "b" --> Error (ErrorAtPos 1)

-- | Parse any 'Char'.
pChar :: Parser Char
pChar = satisfy $ const True

-- | Construct 'Parser' by predicate of 'Char'. 
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = P $ ES $ \(pos, s) ->
  case s of
    (c:cs) | predicate c -> Success (c :# (pos + 1, cs))
    _                    -> Error (ErrorAtPos pos)

-- | Construct 'Parser' that parse given 'Char'.
char :: Char -> Parser Char
char c = satisfy (== c)

-- | Construct 'Parser' that parse given 'String'.
string :: String -> Parser String
string = traverse char

-- | Check end of string. Raise parsing error in 'Parser' if string isn't ended.
pEof :: Parser ()
pEof = P $ ES $ \(n, s) -> case s of
    "" -> Success $ () :# (n, s)
    _  -> Error $ ErrorAtPos n

-- | Raise parsing error in 'Parser'.
parseError :: Parser a
parseError = P $ ES $ \(n, _) -> Error $ ErrorAtPos n

instance Alternative Parser where
  empty = parseError
  (P es1) <|> (P es2) = P $ ES $ \s -> case runES es1 s of
    Error _         -> runES es2 s
    suc@(Success _) -> suc

-- No methods
instance MonadPlus Parser

{-

Simple description of grammar
Everywhere spaces are ignoring, exclude case: 
  unaryOperator <some spaces required> <non-parantheses operand>

    Expr ::= AddSubtract

    AddSubtract  ::= MultiplyDivide AddSubtract'
    AddSubtract' ::= ('+' | '-') MultiplyDivide AddSubtract' 
                    | <eps>

    MultiplyDivide  ::= NotBinary MultiplyDivide'
    MultiplyDivide' ::= ('*' | '/') NotBinary MultiplyDivide' 
                      | <eps>

    NotBinary ::= ('abs' | 'signum') NotBinary 
                | '(' E ')' 
                | Number

    Number   ::= Integral | Integral '.' Integral
    Integral ::= [0-9]+

-}

-- | Parse by 'Bool' some or any spaces.
pSomeOrManySpaces :: Bool -> Parser String
pSomeOrManySpaces b = (if b then some else many) (satisfy isSpace)

-- | Parse zero or more spaces.
pSpaces :: Parser String
pSpaces = pSomeOrManySpaces False

parseExpr :: String -> Except ParseError Expr
parseExpr = runP $ pExpr <* pEof

-- | Parse any correct 'Expr'.
pExpr :: Parser Expr
pExpr = pAddSubtract <* pSpaces

-- | Parse 'Expr' of Add/Subtract operations and acceptable operands.
pAddSubtract :: Parser Expr
pAddSubtract = pMultiplyDivide >>= pAddSubtract'

-- | Supportive function.
pAddSubtract' :: Expr -> Parser Expr
pAddSubtract' a = do
                op <- pSpaces >> msum [Add <$ char '+',
                                       Sub <$ char '-']
                b  <- pMultiplyDivide
                pAddSubtract' $ Op $ op a b
          <|> return a

-- | Parse 'Expr' of Multiply/Divide operations and acceptable operands.
pMultiplyDivide :: Parser Expr
pMultiplyDivide = pUnaryNullary False >>= pMultiplyDivide'

-- | Supportive function.
pMultiplyDivide' :: Expr -> Parser Expr
pMultiplyDivide' a = do
                op <- pSpaces *>
                  msum [Mul <$ char '*',
                        Div <$ char '/']
                b <- pUnaryNullary False
                pMultiplyDivide' $ Op $ op a b
          <|> return a

-- | Parse 'Expr' which is unary operation, 'Expr' in parentheses or number.
pUnaryNullary :: Bool -> Parser Expr
pUnaryNullary unaryOperand = unaryOperation <|> parentheses <|> number
  where
    unaryOperation = do
      op <- pSomeOrManySpaces unaryOperand >>
        msum [Abs <$ string "abs",
              Sgn <$ string "signum"]
      a  <- pUnaryNullary True
      return $ Op $ op a

    parentheses = do
      _    <- pSpaces >> char '('
      expr <- pExpr
      _    <- pSpaces >> char ')'
      return expr

    number = pSomeOrManySpaces unaryOperand >> pNumber

-- | Parse digit 'Char'.
pDigit :: Parser Char
pDigit = satisfy isDigit

-- | Parse number for 'Expr'.
pNumber :: Parser Expr
pNumber = do
  intPart  <- some pDigit
  fracPart <- fmap (fromMaybe "") (optional $ char '.' >> some pDigit)
  return $ Val $
    let
      -- strings here consists of digits, so we can use digitToInt
      integer = foldl' (\acc c -> acc * 10 + toInteger (digitToInt c)) 0 intPart   
      fractional = foldr' (\x acc -> (fromIntegral (digitToInt x) + acc) / 10) 0 fracPart
    in fromIntegral integer + fractional
