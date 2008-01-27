-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Attoparsec
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
module Data.ParserCombinators.Attoparsec
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
    , manyTill
    , eof
    , skipMany
    , skipMany1
    , count
    , lookAhead
    , peek
    , sepBy
    , sepBy1

    -- * Things like in @Parsec.Char@
    , satisfy
    , anyWord8
    , word8
    , notWord8
    , string
    , stringTransform

    -- * Parser converters.
    , eitherP

    -- * Miscellaneous functions.
    , getInput
    , getConsumed
    , takeWhile
    , takeWhile1
    , takeTill
    , takeAll
    , skipWhile
    , notEmpty
    , match
    ) where

import Data.ParserCombinators.Attoparsec.Internal
import Prelude hiding (takeWhile)
