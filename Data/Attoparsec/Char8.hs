{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Char8
-- Copyright   :  Daan Leijen 1999-2001, Jeremy Shaw 2006, Bryan O'Sullivan 2007-2009
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient, character-oriented parser combinators for lazy
-- 'LB.ByteString' strings, loosely based on the Parsec library.
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
      ParseError
    , Parser

    -- * Running parsers
    , parse
    , parseAt
    , parseTest

    -- * Combinators
    , (<?>)
    , try

    -- * Parsing individual characters
    , anyChar
    , char
    , digit
    , letter
    , notChar
    , space
    , satisfy

    -- ** Character classes
    , inClass
    , notInClass

    -- * Efficient string handling
    , string
    , stringCI
    , skipSpace
    , skipWhile
    , takeAll
    , takeCount
    , takeTill
    , takeWhile
    , takeWhile1

    -- ** Combinators
    , match
    , notEmpty

    -- * Text parsing
    , endOfLine

    -- * Numeric parsers
    , int
    , integer
    , double

    -- * State observation functions
    , endOfInput
    , getConsumed
    , getInput
    , lookAhead

    -- * Combinators
    , module Data.Attoparsec.Combinator
    ) where

import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit, isLetter, isSpace, toLower)
import Data.Attoparsec.FastSet
    (FastSet, charClass, memberChar, set)
import qualified Data.Attoparsec.Internal as I
import Data.Attoparsec.Combinator
import Data.Attoparsec.Internal
    (Parser, ParseError, (<?>), parse, parseAt, parseTest, try, endOfInput,
     lookAhead, string,
     getInput, getConsumed, takeAll, takeCount, notEmpty, match,
     endOfLine, setInput)
import Data.ByteString.Lex.Lazy.Double (readDouble)
import Prelude hiding (takeWhile)

-- | Satisfy a literal string, ignoring case.
stringCI :: LB.ByteString -> Parser LB.ByteString
stringCI = I.stringTransform (LB.map toLower)
{-# INLINE stringCI #-}

takeWhile1 :: (Char -> Bool) -> Parser LB.ByteString
takeWhile1 p = I.takeWhile1 (p . w2c)
{-# INLINE takeWhile1 #-}

numeric :: String -> (LB.ByteString -> Maybe (a,LB.ByteString)) -> Parser a
numeric desc f = do
  s <- getInput
  case f s of
    Nothing -> fail desc
    Just (i,s') -> setInput s' >> return i
                   
-- | Parse an integer.  The position counter is not updated.
int :: Parser Int
int = numeric "Int" LB.readInt

-- | Parse an integer.  The position counter is not updated.
integer :: Parser Integer
integer = numeric "Integer" LB.readInteger

-- | Parse a Double.  The position counter is not updated.
double :: Parser Double
double = numeric "Double" readDouble

#define PARSER Parser
#include "Char8Boilerplate.h"
