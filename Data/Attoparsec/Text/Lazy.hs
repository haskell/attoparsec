-- |
-- Module      :  Data.Attoparsec.Text.Lazy
-- Copyright   :  Bryan O'Sullivan 2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for lazy 'Text'
-- strings, loosely based on the Parsec library.
--
-- This is essentially the same code as in the 'Data.Attoparsec.Text'
-- module, only with a 'parse' function that can consume a lazy
-- 'Text' incrementally, and a 'Result' type that does not allow
-- more input to be fed in.  Think of this as suitable for use with a
-- lazily read file, e.g. via 'L.readFile' or 'L.hGetContents'.
--
-- Behind the scenes, strict 'T.Text' values are still used
-- internally to store parser input and manipulate it efficiently.
-- High-performance parsers such as 'string' still expect strict
-- 'T.Text' parameters.

module Data.Attoparsec.Text.Lazy
    (
      Result
    , module Data.Attoparsec.Text
    -- * Running parsers
    , parse
    , feed
    , parseOnly
    , parseTest
    -- ** Result conversion
    , maybeResult
    , eitherResult
    ) where

import Data.Text.Lazy.Internal (Text(..), chunk)
import qualified Data.Text.Lazy as L
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Data.Attoparsec.Text
    hiding (Result, eitherResult, feed, maybeResult,
            parse, parseOnly, parseWith, parseTest)

-- | The result of a parse.
type Result = IResult Text

-- | Run a parser and return its result.
parse :: A.Parser a -> Text -> Result a
parse p = handle (A.parse p)
  where
    go (T.Fail x stk msg) ys      = Fail (chunk x ys) stk msg
    go (T.Done x r) ys            = Done (chunk x ys) r
    go (T.Partial k) (Chunk y ys) = go (k y) ys
    go (T.Partial k) _empty       = Partial (handle k)

    handle k s = case s of
                   Chunk x xs -> go (k x) xs
                   empty      -> go (k T.empty) empty
    {-# INLINE handle #-}

-- | If a parser has returned a 'T.Partial' result, supply it with more
-- input.
feed :: Result a -> Text -> Result a
feed f@(T.Fail _ _ _) _ = f
feed (T.Partial k) d    = k d
feed (T.Done bs r) d    = T.Done (L.append bs d) r
{-# INLINE feed #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: A.Parser a -> Text -> Either String a
parseOnly p s = eitherResult (feed (parse p s) L.empty)
{-# INLINE parseOnly #-}

-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => A.Parser a -> Text -> IO ()
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
