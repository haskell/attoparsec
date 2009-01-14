{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Incremental.Char8
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
module Data.Attoparsec.Incremental.Char8
    (
    -- * Parser
      Parser
    , Result(..)

    -- * Running parsers
    , parse

    -- * Combinators
    , (<?>)

    -- * Things vaguely like those in @Parsec.Combinator@ (and @Parsec.Prim@)
    , pushBack

    -- * Things like in @Parsec.Char@
    , satisfy
    , letter
    , digit
    , anyChar
    , space
    , char
    , notChar
    , string

    -- * Numeric parsers.
    , int
    , integer
    , double

    -- * Miscellaneous functions.
    , takeWhile
    , takeTill
    , takeCount
    , skipWhile
    , skipSpace
    , inClass
    , notInClass
    ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit, isLetter, isSpace)
import Data.Attoparsec.FastSet
    (FastSet, memberChar, set)
import qualified Data.Attoparsec.Incremental as I
import Data.Attoparsec.Incremental
    (Parser, Result(..), (<?>), parse, pushBack,
     string, takeCount)
import Data.ByteString.Lex.Lazy.Double (readDouble)
import Prelude hiding (takeWhile)

numeric :: String -> (Char -> Bool)
         -> (LB.ByteString -> Maybe (a,LB.ByteString)) -> Parser r a
numeric desc p f = do
  s <- takeWhile p
  case f s of
    Nothing -> pushBack s >> fail desc
    Just (i,s') -> pushBack s' >> return i
                   
isIntegral :: Char -> Bool
isIntegral c = isDigit c || c == '-'

-- | Parse an integer.  The position counter is not updated.
int :: Parser r Int
int = numeric "Int" isIntegral LB.readInt

-- | Parse an integer.  The position counter is not updated.
integer :: Parser r Integer
integer = numeric "Integer" isIntegral LB.readInteger

-- | Parse a Double.  The position counter is not updated.
double :: Parser r Double
double = numeric "Double" isDouble readDouble
    where isDouble c = isIntegral c || c == 'e' || c == '+'

#define PARSER Parser r
#include "../Char8Boilerplate.h"
