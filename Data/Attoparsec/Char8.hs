{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Char8
-- Copyright   :  Daan Leijen 1999-2001, Jeremy Shaw 2006, Bryan O'Sullivan 2007-2008
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for lazy 'LB.ByteString'
-- strings, loosely based on 'Text.ParserCombinators.Parsec'.
-- 
-----------------------------------------------------------------------------
module Data.Attoparsec.Char8
    (
    -- * Parser
      ParseError
    , Parser

    -- * Running parsers
    , parse
    , parseAt
    , parseTest

    -- * Combinators
    , (<?>)

    -- * Things vaguely like those in @Parsec.Combinator@ (and @Parsec.Prim@)
    , try
    , eof
    , lookAhead
    , peek

    -- * Things like in @Parsec.Char@
    , satisfy
    , letter
    , digit
    , anyChar
    , space
    , char
    , notChar
    , string
    , stringCI

    -- * Parser converters.
    , eitherP

    -- * Numeric parsers.
    , int
    , integer
    , double

    -- * Miscellaneous functions.
    , getInput
    , getConsumed
    , takeWhile
    , takeWhile1
    , takeTill
    , takeAll
    , takeCount
    , skipWhile
    , skipSpace
    , notEmpty
    , match
    , inClass
    , notInClass
    , endOfLine
    ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit, isLetter, isSpace, toLower)
import Data.Attoparsec.FastSet
    (FastSet, memberChar, set)
import qualified Data.Attoparsec.Internal as I
import Data.Attoparsec.Internal
    (Parser, ParseError, (<?>), parse, parseAt, parseTest, try, eof,
     lookAhead, peek, string,
     eitherP, getInput, getConsumed, takeAll, takeCount, notEmpty, match,
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
