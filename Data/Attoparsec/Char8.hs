-- |
-- Module      :  Data.Attoparsec.Char8
-- Copyright   :  Bryan O'Sullivan 2007-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient, character-oriented combinator parsing for
-- 'B.ByteString' strings, loosely based on the Parsec library.

module Data.Attoparsec.Char8
    (
    -- * Character encodings
    -- $encodings

    -- * Parser types
      Parser
    , A.Result(..)

    -- * Running parsers
    , A.parse
    , A.parseTest
    , A.parseWith
    , A.feed

    -- * Combinators
    , (I.<?>)
    , I.try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual characters
    , satisfy
    , char
    , anyChar
    , char8
    , notChar

    -- ** Special character parsers
    , digit
    , letter_iso8859_15
    , letter_ascii
    , space

    -- ** Fast predicates
    , isDigit
    , isDigit_w8
    , isAlpha_iso8859_15
    , isAlpha_ascii
    , isSpace

    -- *** Character classes
    , inClass
    , notInClass

    -- * Efficient string handling
    , I.string
    , stringCI
    , skipSpace
    , skipWhile
    , I.take
    , takeTill
    , takeWhile
    , takeWhile1

    -- * Text parsing
    , I.endOfLine
    , isEndOfLine
    , isHorizontalSpace

    -- * Numeric parsers
    , decimal
    , hexadecimal
    , signed
    --, double

    -- * State observation and manipulation functions
    , I.endOfInput
    , I.ensure
    ) where

import Control.Applicative ((*>), (<$>), (<|>))
import Data.Attoparsec.Combinator
import Data.Attoparsec.FastSet (charClass, memberChar)
import Data.Attoparsec.Internal (Parser, (<?>))
import Data.ByteString.Internal (c2w, w2c)
import Data.Word (Word8)
import Prelude hiding (takeWhile)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Internal as I
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B

-- $encodings
--
-- This module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or
-- ISO-8859-15.  It /does not/ make any attempt to deal with character
-- encodings, multibyte characters, or wide characters.  In
-- particular, all attempts to use characters above code point U+00FF
-- will give wrong answers.
--
-- Code points below U+0100 are simply translated to and from their
-- numeric values, so e.g. the code point U+00A4 becomes the byte
-- @0xA4@ (which is the Euro symbol in ISO-8859-15, but the generic
-- currency sign in ISO-8859-1).  Haskell 'Char' values above U+00FF
-- are truncated, so e.g. U+1D6B7 is truncated to the byte @0xB7@.

-- ASCII-specific but fast, oh yes.
toLower :: Word8 -> Word8
toLower w | w >= 65 && w <= 90 = w + 32
          | otherwise          = w

-- | Satisfy a literal string, ignoring case.
stringCI :: B.ByteString -> Parser B.ByteString
stringCI = I.stringTransform (B8.map toLower)
{-# INLINE stringCI #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser B.ByteString
takeWhile1 p = I.takeWhile1 (p . w2c)
{-# INLINE takeWhile1 #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
satisfy :: (Char -> Bool) -> Parser Char
satisfy = I.satisfyWith w2c
{-# INLINE satisfy #-}

-- | Match a letter, in the ISO-8859-15 encoding.
letter_iso8859_15 :: Parser Char
letter_iso8859_15 = satisfy isAlpha_iso8859_15 <?> "letter_iso8859_15"
{-# INLINE letter_iso8859_15 #-}

-- | Match a letter, in the ASCII encoding.
letter_ascii :: Parser Char
letter_ascii = satisfy isAlpha_ascii <?> "letter_ascii"
{-# INLINE letter_ascii #-}

-- | A fast alphabetic predicate for the ISO-8859-15 encoding
--
-- /Note/: For all character encodings other than ISO-8859-15, and
-- almost all Unicode code points above U+00A3, this predicate gives
-- /wrong answers/.
isAlpha_iso8859_15 :: Char -> Bool
isAlpha_iso8859_15 c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                       (c >= '\166' && moby c)
  where moby = notInClass "\167\169\171-\179\182\183\185\187\191\215\247"
        {-# NOINLINE moby #-}
{-# INLINE isAlpha_iso8859_15 #-}

-- | A fast alphabetic predicate for the ASCII encoding
--
-- /Note/: For all character encodings other than ASCII, and
-- almost all Unicode code points above U+007F, this predicate gives
-- /wrong answers/.
isAlpha_ascii :: Char -> Bool
isAlpha_ascii c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
{-# INLINE isAlpha_ascii #-}

-- | Parse a single digit.
digit :: Parser Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | A fast digit predicate.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

-- | A fast digit predicate.
isDigit_w8 :: Word8 -> Bool
isDigit_w8 w = w >= 48 && w <= 57
{-# INLINE isDigit_w8 #-}

-- | Match any character.
anyChar :: Parser Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

-- | Fast predicate for matching a space character.
--
-- /Note/: This predicate only gives correct answers for the ASCII
-- encoding.  For instance, it does not recognise U+00A0 (non-breaking
-- space) as a space character, even though it is a valid ISO-8859-15
-- byte.
isSpace :: Char -> Bool
isSpace c = c `B.elem` spaces
    where spaces = B.pack " \n\r\t\v\f"
          {-# NOINLINE spaces #-}
{-# INLINE isSpace #-}

-- | Parse a space character.
--
-- /Note/: This parser only gives correct answers for the ASCII
-- encoding.  For instance, it does not recognise U+00A0 (non-breaking
-- space) as a space character, even though it is a valid ISO-8859-15
-- byte.
space :: Parser Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Match a specific character.
char :: Char -> Parser Char
char c = satisfy (== c) <?> [c]
{-# INLINE char #-}

-- | Match a specific character, but return its 'Word8' value.
char8 :: Char -> Parser Word8
char8 c = I.satisfy (== c2w c) <?> [c]
{-# INLINE char8 #-}

-- | Match any character except the given one.
notChar :: Char -> Parser Char
notChar c = satisfy (/= c) <?> "not " ++ [c]
{-# INLINE notChar #-}

-- | Match any character in a set.
--
-- >vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- >halfAlphabet = inClass "a-nA-N"
--
-- To add a literal \'-\' to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Char -> Bool
inClass s = (`memberChar` mySet)
    where mySet = charClass s
{-# INLINE inClass #-}

-- | Match any character not in a set.
notInClass :: String -> Char -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeWhile :: (Char -> Bool) -> Parser B.ByteString
takeWhile p = I.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeTill :: (Char -> Bool) -> Parser B.ByteString
takeTill p = I.takeTill (p . w2c)
{-# INLINE takeTill #-}

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = I.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = skipWhile isSpace >> return ()
{-# INLINE skipSpace #-}

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 13 || w == 10
{-# INLINE isEndOfLine #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Word8 -> Bool
isHorizontalSpace w = w == 32 || w == 9
{-# INLINE isHorizontalSpace #-}

{-
-- | Parse a Double.  The position counter is not updated.
double :: Parser Double
double = numeric "Double" readDouble
-}

-- | Parse and decode an unsigned hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string.
hexadecimal :: Integral a => Parser a
{-# SPECIALISE hexadecimal :: Parser Int #-}
hexadecimal = B8.foldl' step 0 `fmap` I.takeWhile1 isHexDigit
  where isHexDigit w = (w >= 48 && w <= 57) || (x >= 97 && x <= 102)
            where x = toLower w
        step a w | w >= 48 && w <= 57  = a * 16 + fromIntegral (w - 48)
                 | otherwise           = a * 16 + fromIntegral (x - 87)
            where x = toLower w

-- | Parse and decode an unsigned decimal number.
decimal :: Integral a => Parser a
{-# SPECIALISE decimal :: Parser Int #-}
decimal = B8.foldl' step 0 `fmap` I.takeWhile1 isDig
  where isDig w  = w >= 48 && w <= 57
        step a w = a * 10 + fromIntegral (w - 48)

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
signed :: Num a => Parser a -> Parser a
{-# SPECIALISE signed :: Parser Int -> Parser Int #-}
signed p = (negate <$> (char8 '-' *> p))
       <|> (char8 '+' *> p)
       <|> p
