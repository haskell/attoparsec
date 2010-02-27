-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Char8
-- Copyright   :  Bryan O'Sullivan 2007-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient, character-oriented parser combinators for
-- 'B.ByteString' strings, loosely based on the Parsec library.
-- 
-- /Note/: This module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or
-- ISO-8859-15.  It /does not/ deal with character encodings,
-- multibyte characters, or wide characters.  Any attempts to use
-- characters above code point 255 will give wrong answers.
-----------------------------------------------------------------------------
module Data.Attoparsec.Char8
    (
    -- * Parser types
      Parser
    , A.Result(..)

    -- * Running parsers
    , A.parse
    , A.parseTest
    , A.feed

    -- * Combinators
    , (I.<?>)
    , I.try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual characters
    , anyChar
    , char
    , char8
    , digit
    , letter
    , notChar
    , space
    , satisfy

    -- ** Character classes
    , inClass
    , notInClass

    -- * Efficient string handling
    , I.string
    , stringCI
    , skipSpace
    , skipWhile
    , take
    , takeTill
    , takeWhile
    , takeWhile1

    -- * Text parsing
    , I.endOfLine

    -- * Numeric parsers
    --, int
    --, integer
    --, double

    -- * State observation and manipulation functions
    , I.endOfInput
    , I.ensure
    ) where

import Data.Attoparsec.Combinator
import Data.Attoparsec.FastSet (charClass, memberChar)
import Data.Attoparsec.Internal (Parser, (<?>))
import Data.ByteString.Internal (c2w, w2c)
import Data.ByteString.Lex.Double (readDouble)
import Data.Char (toLower)
import Data.Word (Word8)
import Prelude hiding (takeWhile)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Internal as I
import qualified Data.ByteString.Char8 as B

-- | Satisfy a literal string, ignoring case.
stringCI :: B.ByteString -> Parser B.ByteString
stringCI = I.stringTransform (B.map toLower)
{-# INLINE stringCI #-}

takeWhile1 :: (Char -> Bool) -> Parser B.ByteString
takeWhile1 p = I.takeWhile1 (p . w2c)
{-# INLINE takeWhile1 #-}

-- | Character parser.
satisfy :: (Char -> Bool) -> Parser Char
satisfy = I.satisfyWith w2c
{-# INLINE satisfy #-}

letter :: Parser Char
letter = satisfy isLetter <?> "letter"
  where isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
{-# INLINE letter #-}

digit :: Parser Char
digit = satisfy isDigit <?> "digit"
  where isDigit c = c >= '0' && c <= '9'
{-# INLINE digit #-}

anyChar :: Parser Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

isSpace :: Char -> Bool
isSpace c = c `B.elem` spaces
    where spaces = B.pack " \n\r\t\v\f"

space :: Parser Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Match a specific character.
char :: Char -> Parser Char
char c = satisfy (== c) <?> [c]
{-# INLINE char #-}

-- | Match a specific character.
char8 :: Char -> Parser Word8
char8 c = I.satisfy (== c2w c) <?> [c]
{-# INLINE char8 #-}

-- | Match any character except the given one.
notChar :: Char -> Parser Char
notChar c = satisfy (/= c) <?> "not " ++ [c]
{-# INLINE notChar #-}

-- | Match any character in a set.
--
-- > vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- > halfAlphabet = inClass "a-nA-N"
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

-- | Consume characters while the predicate succeeds.
takeWhile :: (Char -> Bool) -> Parser B.ByteString
takeWhile p = I.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

-- | Consume characters while the predicate fails.
takeTill :: (Char -> Bool) -> Parser B.ByteString
takeTill p = I.takeTill (p . w2c)
{-# INLINE takeTill #-}

-- | Skip over characters while the predicate succeeds.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = I.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = skipWhile isSpace >> return ()
{-# INLINE skipSpace #-}

{-
numeric :: String -> (B.ByteString -> Maybe (a,B.ByteString)) -> Parser a
numeric desc f = do
  s <- getInput
  case f s of
    Nothing -> fail desc
    Just (i,s') -> setInput s' >> return i
                   
-- | Parse an integer.  The position counter is not updated.
int :: Parser Int
int = numeric "Int" B.readInt

-- | Parse an integer.  The position counter is not updated.
integer :: Parser Integer
integer = numeric "Integer" B.readInteger

-- | Parse a Double.  The position counter is not updated.
double :: Parser Double
double = numeric "Double" readDouble
-}
