-- |
-- Module      :  Data.Attoparsec.ByteString.Lazy
-- Copyright   :  Bryan O'Sullivan 2010, 2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for lazy 'ByteString'
-- strings, loosely based on the Parsec library.
--
-- This is essentially the same code as in the 'Data.Attoparsec'
-- module, only with a 'parse' function that can consume a lazy
-- 'ByteString' incrementally, and a 'Result' type that does not allow
-- more input to be fed in.  Think of this as suitable for use with a
-- lazily read file, e.g. via 'L.readFile' or 'L.hGetContents'.
--
-- Behind the scenes, strict 'B.ByteString' values are still used
-- internally to store parser input and manipulate it efficiently.
-- High-performance parsers such as 'string' still expect strict
-- 'B.ByteString' parameters.

module Data.Attoparsec.ByteString.Lazy
    (
      Result
    , module Data.Attoparsec.ByteString
    -- * Running parsers
    , parse
    , parseTest
    -- ** Result conversion
    , maybeResult
    , eitherResult
    ) where

import Data.ByteString.Lazy.Internal (ByteString(..), chunk)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.ByteString
    hiding (Result, eitherResult, maybeResult,
            parse, parseWith, parseTest)

-- | The result of a parse.
type Result = IResult ByteString

-- | Run a parser and return its result.
parse :: A.Parser a -> ByteString -> Result a
parse p = handle (A.parse p)
  where
    go (T.Fail x stk msg) ys      = Fail (chunk x ys) stk msg
    go (T.Done x r) ys            = Done (chunk x ys) r
    go (T.Partial k) (Chunk y ys) = go (k y) ys
    go (T.Partial k) _empty       = Partial (handle k)

    handle k s = case s of
                   Chunk x xs -> go (k x) xs
                   empty      -> go (k B.empty) empty
    {-# INLINE handle #-}

-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => A.Parser a -> ByteString -> IO ()
parseTest p s = print (parse p s)

-- | Convert a 'Result' value to a 'Maybe' value.
maybeResult :: Result r -> Maybe r
maybeResult (Done _ r) = Just r
maybeResult _          = Nothing

-- | Convert a 'Result' value to an 'Either' value.
eitherResult :: Result r -> Either String r
eitherResult (Done _ r)     = Right r
eitherResult (Fail _ _ msg) = Left msg
eitherResult _              = Left "Result: incomplete input"
