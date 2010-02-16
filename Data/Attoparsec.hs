-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec
-- Copyright   :  Bryan O'Sullivan 2007-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'ByteString' strings,
-- loosely based on 'Text.ParserCombinators.Parsec'.
-- 
-----------------------------------------------------------------------------
module Data.Attoparsec
    (
    -- * Parser types
      Parser
    , Result

    -- * Running parsers
    , parse
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
    , takeTill
    , takeWhile
    , takeWhile1

    -- * State observation functions
    , endOfInput
    ) where

import Data.Attoparsec.Combinator
import Data.Attoparsec.Internal
import Prelude hiding (takeWhile)
import qualified Data.ByteString as B

parseTest :: (Show a) => Parser a -> B.ByteString -> IO ()
parseTest p s = print (parse p s)
