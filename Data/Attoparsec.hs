-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec
-- Copyright   :  Daan Leijen 1999-2001, Jeremy Shaw 2006, Bryan O'Sullivan 2007-2008
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for lazy 'ByteString'
-- strings, loosely based on 'Text.ParserCombinators.Parsec'.
-- 
-----------------------------------------------------------------------------
module Data.Attoparsec
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
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , anyWord8
    , notWord8
    , word8
    , satisfy

    -- ** Byte classes
    , inClass
    , notInClass

    -- * Efficient string handling
    , string
    , skipWhile
    , stringTransform
    , takeAll
    , takeTill
    , takeWhile
    , takeWhile1

    -- ** Combinators
    , match
    , notEmpty

    -- * State observation functions
    , endOfInput
    , getConsumed
    , getInput
    , lookAhead
    ) where

import Data.Attoparsec.Combinator
import Data.Attoparsec.Internal
import Prelude hiding (takeWhile)
