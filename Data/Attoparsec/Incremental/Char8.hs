{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Incremental.Char8
-- Copyright   :  Bryan O'Sullivan 2009
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient, character-oriented, and incremental parser
-- combinators for lazy 'L.ByteString' strings, loosely based on the
-- Parsec library.
-- 
-- /Note/: This module is intended for parsing text that is
-- represented using an 8-bit character set, e.g. ASCII or
-- ISO-8859-15.  It /does not/ deal with character encodings,
-- multibyte characters, or wide characters.  Any attempts to use
-- characters above code point 255 will give wrong answers.
-----------------------------------------------------------------------------
module Data.Attoparsec.Incremental.Char8
    (
    -- * Parser types
      Parser
    , Result(..)

    -- * Running parsers
    , parse
    , parseWith
    , parseTest
    , feed

    -- * Combinators
    , (<?>)
    , try

    -- * Parsing individual characters
    , satisfy
    , letter
    , digit
    , anyChar
    , space
    , char
    , notChar

    -- ** Character classes
    , inClass
    , notInClass

    -- * Efficient string handling
    , string
    , skipSpace
    , skipWhile
    , takeCount
    , takeTill
    , takeWhile

    -- * Text parsing
    , endOfLine

    -- * Numeric parsers
    , int
    , integer
    , double

    -- * State observation and manipulation functions
    , endOfInput
    , pushBack
    , yield

    -- * Combinators
    , module Data.Attoparsec.Combinator
    ) where

import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit, isLetter, isSpace)
import Data.Attoparsec.FastSet (charClass, memberChar)
import qualified Data.Attoparsec.Incremental as I
import Data.Attoparsec.Incremental
    (Parser, Result(..), (<?>), endOfInput, feed, parse, parseWith, parseTest,
     pushBack, string, takeCount, try, yield)
import Data.ByteString.Lex.Lazy.Double (readDouble)
import Prelude hiding (takeWhile)
import Data.Attoparsec.Combinator

numeric :: String -> (Char -> Bool)
         -> (LB.ByteString -> Maybe (a,LB.ByteString)) -> Parser r a
numeric desc p f = do
  s <- takeWhile p
  case f s of
    Nothing -> pushBack s >> fail desc
    Just (i,s') -> pushBack s' >> return i
                   
isIntegral :: Char -> Bool
isIntegral c = isDigit c || c == '-'

-- | Parse an 'Int'.
int :: Parser r Int
int = numeric "Int" isIntegral LB.readInt

-- | Parse an 'Integer'.
integer :: Parser r Integer
integer = numeric "Integer" isIntegral LB.readInteger

-- | Parse a 'Double'.
double :: Parser r Double
double = numeric "Double" isDouble readDouble
    where isDouble c = isIntegral c || c == 'e' || c == '+'

-- | Match the end of a line.  This may be any of a newline character,
-- a carriage return character, or a carriage return followed by a newline.
endOfLine :: Parser r ()
endOfLine = (char '\n' *> pure ()) <|> (string crlf *> pure ())
    where crlf = LB.pack "\r\n"

#define PARSER Parser r
#include "../Char8Boilerplate.h"
