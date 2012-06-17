{-# LANGUAGE BangPatterns, FlexibleInstances, TypeSynonymInstances #-}

-- |
-- Module      :  Data.Attoparsec.Text
-- Copyright   :  Bryan O'Sullivan 2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for 'Text' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.Text
    (
    -- * Differences from Parsec
    -- $parsec

    -- * Incremental input
    -- $incremental

    -- * Performance considerations
    -- $performance

    -- * Parser types
      Parser
    , Result
    , T.IResult(..)
    , I.compareResults

    -- * Running parsers
    , parse
    , feed
    , I.parseOnly
    , parseWith
    , parseTest

    -- ** Result conversion
    , maybeResult
    , eitherResult

    -- * Combinators
    , (I.<?>)
    , I.try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual characters
    , I.char
    , I.anyChar
    , I.notChar
    , I.satisfy
    , I.satisfyWith
    , I.skip
    , I.peekChar

    -- ** Special character parsers
    , digit
    , letter
    , space

    -- ** Character classes
    , I.inClass
    , I.notInClass

    -- * Efficient string handling
    , I.string
    , I.stringCI
    , I.asciiCI
    , skipSpace
    , I.skipWhile
    , I.scan
    , I.take
    , I.takeWhile
    , I.takeWhile1
    , I.takeTill

    -- ** String combinators
    -- $specalt
    , (.*>)
    , (<*.)

    -- ** Consume all remaining input
    , I.takeText
    , I.takeLazyText

    -- * Text parsing
    , I.endOfLine
    , isEndOfLine
    , isHorizontalSpace

    -- * Numeric parsers
    , decimal
    , hexadecimal
    , signed
    , double
    , Number(..)
    , number
    , rational

    -- * State observation and manipulation functions
    , I.endOfInput
    , I.atEnd
    ) where

import Control.Applicative ((<$>), (*>), (<*), (<|>))
import Data.Attoparsec.Combinator
import Data.Attoparsec.Number (Number(..))
import Data.Attoparsec.Text.Internal ((<?>), Parser, Result, parse, takeWhile1)
import Data.Bits (Bits, (.|.), shiftL)
import Data.Char (isAlpha, isDigit, isSpace, ord)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64, Word)
import qualified Data.Attoparsec.Internal as I
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.Attoparsec.Text.Internal as I
import qualified Data.Text as T

-- $parsec
--
-- Compared to Parsec 3, Attoparsec makes several tradeoffs.  It is
-- not intended for, or ideal for, all possible uses.
--
-- * While Attoparsec can consume input incrementally, Parsec cannot.
--   Incremental input is a huge deal for efficient and secure network
--   and system programming, since it gives much more control to users
--   of the library over matters such as resource usage and the I/O
--   model to use.
--
-- * Much of the performance advantage of Attoparsec is gained via
--   high-performance parsers such as 'I.takeWhile' and 'I.string'.
--   If you use complicated combinators that return lists of
--   characters, there is less performance difference between the two
--   libraries.
--
-- * Unlike Parsec 3, Attoparsec does not support being used as a
--   monad transformer.
--
-- * Attoparsec is specialised to deal only with strict 'Text'
--   input.  Efficiency concerns rule out both lists and lazy text.
--   The usual use for lazy text would be to allow consumption of very
--   large input without a large footprint.  For this need,
--   Attoparsec's incremental input provides an excellent substitute,
--   with much more control over when input takes place.  If you must
--   use lazy text, see the 'Lazy' module, which feeds lazy chunks to
--   a regular parser.
--
-- * Parsec parsers can produce more helpful error messages than
--   Attoparsec parsers.  This is a matter of focus: Attoparsec avoids
--   the extra book-keeping in favour of higher performance.

-- $incremental
--
-- Attoparsec supports incremental input, meaning that you can feed it
-- a 'Text' that represents only part of the expected total amount
-- of data to parse. If your parser reaches the end of a fragment of
-- input and could consume more input, it will suspend parsing and
-- return a 'T.Partial' continuation.
--
-- Supplying the 'T.Partial' continuation with another string will
-- resume parsing at the point where it was suspended. You must be
-- prepared for the result of the resumed parse to be another
-- 'Partial' continuation.
--
-- To indicate that you have no more input, supply the 'Partial'
-- continuation with an 'T.empty' 'Text'.
--
-- Remember that some parsing combinators will not return a result
-- until they reach the end of input.  They may thus cause 'T.Partial'
-- results to be returned.
--
-- If you do not need support for incremental input, consider using
-- the 'I.parseOnly' function to run your parser.  It will never
-- prompt for more input.

-- $performance
--
-- If you write an Attoparsec-based parser carefully, it can be
-- realistic to expect it to perform within a factor of 2 of a
-- hand-rolled C parser (measuring megabytes parsed per second).
--
-- To actually achieve high performance, there are a few guidelines
-- that it is useful to follow.
--
-- Use the 'Text'-oriented parsers whenever possible,
-- e.g. 'I.takeWhile1' instead of 'many1' 'I.anyChar'.  There is
-- about a factor of 100 difference in performance between the two
-- kinds of parser.
--
-- For very simple character-testing predicates, write them by hand
-- instead of using 'I.inClass' or 'I.notInClass'.  For instance, both
-- of these predicates test for an end-of-line character, but the
-- first is much faster than the second:
--
-- >endOfLine_fast c = c == '\r' || c == '\n'
-- >endOfLine_slow   = inClass "\r\n"
--
-- Make active use of benchmarking and profiling tools to measure,
-- find the problems with, and improve the performance of your parser.

-- | If a parser has returned a 'Partial' result, supply it with more
-- input.
feed :: Result r -> Text -> Result r
feed f@(T.Fail _ _ _) _ = f
feed (T.Partial k) d    = k d
feed (T.Done bs r) d    = T.Done (T.append bs d) r
{-# INLINE feed #-}

-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => I.Parser a -> Text -> IO ()
parseTest p s = print (parse p s)

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
parseWith :: Monad m =>
             (m Text)
          -- ^ An action that will be executed to provide the parser
          -- with more input, if necessary.  The action must return an
          -- 'T.empty' string when there is no more input available.
          -> I.Parser a
          -> Text
          -- ^ Initial input for the parser.
          -> m (Result a)
parseWith refill p s = step $ parse p s
  where step (T.Partial k) = (step . k) =<< refill
        step r           = return r
{-# INLINE parseWith #-}

-- | Convert a 'Result' value to a 'Maybe' value. A 'Partial' result
-- is treated as failure.
maybeResult :: Result r -> Maybe r
maybeResult (T.Done _ r) = Just r
maybeResult _            = Nothing

-- | Convert a 'Result' value to an 'Either' value. A 'Partial' result
-- is treated as failure.
eitherResult :: Result r -> Either String r
eitherResult (T.Done _ r)     = Right r
eitherResult (T.Fail _ _ msg) = Left msg
eitherResult _                = Left "Result: incomplete input"

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'
{-# INLINE isEndOfLine #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'
{-# INLINE isHorizontalSpace #-}

-- | Parse and decode an unsigned hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string.
hexadecimal :: (Integral a, Bits a) => Parser a
hexadecimal = T.foldl' step 0 `fmap` takeWhile1 isHexDigit
  where
    isHexDigit c = (c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F')
    step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. fromIntegral (w - 48)
             | w >= 97             = (a `shiftL` 4) .|. fromIntegral (w - 87)
             | otherwise           = (a `shiftL` 4) .|. fromIntegral (w - 55)
      where w = ord c
{-# SPECIALISE hexadecimal :: Parser Int #-}
{-# SPECIALISE hexadecimal :: Parser Int8 #-}
{-# SPECIALISE hexadecimal :: Parser Int16 #-}
{-# SPECIALISE hexadecimal :: Parser Int32 #-}
{-# SPECIALISE hexadecimal :: Parser Int64 #-}
{-# SPECIALISE hexadecimal :: Parser Integer #-}
{-# SPECIALISE hexadecimal :: Parser Word #-}
{-# SPECIALISE hexadecimal :: Parser Word8 #-}
{-# SPECIALISE hexadecimal :: Parser Word16 #-}
{-# SPECIALISE hexadecimal :: Parser Word32 #-}
{-# SPECIALISE hexadecimal :: Parser Word64 #-}

-- | Parse and decode an unsigned decimal number.
decimal :: Integral a => Parser a
decimal = T.foldl' step 0 `fmap` takeWhile1 isDecimal
  where step a c = a * 10 + fromIntegral (ord c - 48)
{-# SPECIALISE decimal :: Parser Int #-}
{-# SPECIALISE decimal :: Parser Int8 #-}
{-# SPECIALISE decimal :: Parser Int16 #-}
{-# SPECIALISE decimal :: Parser Int32 #-}
{-# SPECIALISE decimal :: Parser Int64 #-}
{-# SPECIALISE decimal :: Parser Integer #-}
{-# SPECIALISE decimal :: Parser Word #-}
{-# SPECIALISE decimal :: Parser Word8 #-}
{-# SPECIALISE decimal :: Parser Word16 #-}
{-# SPECIALISE decimal :: Parser Word32 #-}
{-# SPECIALISE decimal :: Parser Word64 #-}

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'
{-# INLINE isDecimal #-}

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
signed :: Num a => Parser a -> Parser a
{-# SPECIALISE signed :: Parser Int -> Parser Int #-}
{-# SPECIALISE signed :: Parser Int8 -> Parser Int8 #-}
{-# SPECIALISE signed :: Parser Int16 -> Parser Int16 #-}
{-# SPECIALISE signed :: Parser Int32 -> Parser Int32 #-}
{-# SPECIALISE signed :: Parser Int64 -> Parser Int64 #-}
{-# SPECIALISE signed :: Parser Integer -> Parser Integer #-}
signed p = (negate <$> (I.char '-' *> p))
       <|> (I.char '+' *> p)
       <|> p

-- | Parse a rational number.
--
-- This parser accepts an optional leading sign character, followed by
-- at least one decimal digit.  The syntax similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ or
-- @\'e\'@ /not/ followed by a number is not consumed.
--
-- Examples with behaviour identical to 'read', if you feed an empty
-- continuation to the first result:
--
-- >rational "3"     == Done 3.0 ""
-- >rational "3.1"   == Done 3.1 ""
-- >rational "3e4"   == Done 30000.0 ""
-- >rational "3.1e4" == Done 31000.0, ""
--
-- Examples with behaviour identical to 'read':
--
-- >rational ".3"    == Fail "input does not start with a digit"
-- >rational "e3"    == Fail "input does not start with a digit"
--
-- Examples of differences from 'read':
--
-- >rational "3.foo" == Done 3.0 ".foo"
-- >rational "3e"    == Done 3.0 "e"
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
rational :: Fractional a => Parser a
{-# SPECIALIZE rational :: Parser Double #-}
{-# SPECIALIZE rational :: Parser Float #-}
{-# SPECIALIZE rational :: Parser Rational #-}
rational = floaty $ \real frac fracDenom -> fromRational $
                     real % 1 + frac % fracDenom

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'rational'.
--
-- /Note/: This function is almost ten times faster than 'rational',
-- but is slightly less accurate.
--
-- The 'Double' type supports about 16 decimal places of accuracy.
-- For 94.2% of numbers, this function and 'rational' give identical
-- results, but for the remaining 5.8%, this function loses precision
-- around the 15th decimal place.  For 0.001% of numbers, this
-- function will lose precision at the 13th or 14th decimal place.
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
double :: Parser Double
double = floaty asDouble

asDouble :: Integer -> Integer -> Integer -> Double
asDouble real frac fracDenom =
    fromIntegral real + fromIntegral frac / fromIntegral fracDenom
{-# INLINE asDouble #-}

-- | Parse a number, attempting to preserve both speed and precision.
--
-- The syntax accepted by this parser is the same as for 'rational'.
--
-- /Note/: This function is almost ten times faster than 'rational'.
-- On integral inputs, it gives perfectly accurate answers, and on
-- floating point inputs, it is slightly less accurate than
-- 'rational'.
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
number :: Parser Number
number = floaty $ \real frac fracDenom ->
         if frac == 0 && fracDenom == 0
         then I real
         else D (asDouble real frac fracDenom)
{-# INLINE number #-}

-- | Parse a single digit, as recognised by 'isDigit'.
digit :: Parser Char
digit = I.satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parse a letter, as recognised by 'isAlpha'.
letter :: Parser Char
letter = I.satisfy isAlpha <?> "letter"
{-# INLINE letter #-}

-- | Parse a space character, as recognised by 'isSpace'.
space :: Parser Char
space = I.satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = I.skipWhile isSpace
{-# INLINE skipSpace #-}

-- $specalt
--
-- The '.*>' and '<*.' combinators are intended for use with the
-- @OverloadedStrings@ language extension.  They simplify the common
-- task of matching a statically known string, then immediately
-- parsing something else.
--
-- An example makes this easier to understand:
--
-- @{-\# LANGUAGE OverloadedStrings #-}
--
-- shoeSize = \"Shoe size: \" '.*>' 'decimal'
-- @
--
-- If we were to try to use '*>' above instead, the type checker would
-- not be able to tell which 'IsString' instance to use for the text
-- in quotes.  We would have to be explicit, using either a type
-- signature or the 'I.string' parser.

-- | Type-specialized version of '*>' for 'Text'.
(.*>) :: Text -> Parser a -> Parser a
s .*> f = I.string s *> f

-- | Type-specialized version of '<*' for 'Text'.
(<*.) :: Parser a -> Text -> Parser a
f <*. s = f <* I.string s

data T = T !Integer !Int

floaty :: Fractional a => (Integer -> Integer -> Integer -> a) -> Parser a
{-# INLINE floaty #-}
floaty f = do
  !positive <- ((== '+') <$> I.satisfy (\c -> c == '-' || c == '+')) <|>
               return True
  real <- decimal
  let tryFraction = do
        _ <- I.satisfy (=='.')
        ds <- I.takeWhile isDigit
        case I.parseOnly decimal ds of
                Right n -> return $ T n (T.length ds)
                _       -> fail "no digits after decimal"
  T fraction fracDigits <- tryFraction <|> return (T 0 0)
  let e c = c == 'e' || c == 'E'
  power <- (I.satisfy e *> signed decimal) <|> return (0::Int)
  let n = if fracDigits == 0
          then if power == 0
               then fromIntegral real
               else fromIntegral real * (10 ^^ power)
          else if power == 0
               then f real fraction (10 ^ fracDigits)
               else f real fraction (10 ^ fracDigits) * (10 ^^ power)
  return $ if positive
           then n
           else -n
