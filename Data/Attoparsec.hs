-- |
-- Module      :  Data.Attoparsec
-- Copyright   :  Bryan O'Sullivan 2007-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for 'B.ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec
    (
    -- * Performance
    -- $performance

    -- * Parser types
      I.Parser
    , Result(..)

    -- ** Typeclass instances
    -- $instances

    -- * Running parsers
    , parse
    , feed
    , parseWith
    , parseTest

    -- * Combinators
    , (I.<?>)
    , I.try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , I.word8
    , I.anyWord8
    , I.notWord8
    , I.satisfy
    , I.satisfyWith

    -- ** Byte classes
    , I.inClass
    , I.notInClass

    -- * Efficient string handling
    , I.string
    , I.skipWhile
    , I.take
    , I.takeWhile
    , I.takeWhile1
    , I.takeTill

    -- * State observation and manipulation functions
    , I.endOfInput
    , I.ensure
    ) where

import Control.Applicative (Alternative(..), Applicative)
import Control.Monad (MonadPlus(..))
import Data.Attoparsec.Combinator
import Prelude hiding (takeWhile)
import qualified Data.Attoparsec.Internal as I
import qualified Data.ByteString as B

-- $performance
--
-- If you write an Attoparsec-based parser carefully, it can be
-- entirely reasonable to expect it to perform within a factor of 2 of
-- a hand-rolled C parser (measuring megabytes parsed per second).
--
-- To actually achieve high performance, there are a few guidelines
-- that it is useful to follow.
--
-- Use the 'B.ByteString'-oriented parsers whenever possible,
-- e.g. 'I.takeWhile1' instead of 'many1' 'I.anyWord8'.  There is
-- about a factor of 100 difference in performance between the two.
--
-- For very simple byte-testing predicates, write them by hand instead
-- of using 'I.inClass' or 'I.notInClass'.  For instance, both of
-- these predicates test for an end-of-line byte, but the first is
-- much faster than the second:
--
-- >endOfLine_fast w = w == 13 || w == 10
-- >endOfLine_slow   = inClass "\r\n"
--
-- Make active use of benchmarking and profiling tools to measure,
-- find the problems with, and improve the performance of your parser.

-- $instances
--
-- The 'I.Parser' type is an instance of the following classes:
--
-- * 'Monad', where 'fail' throws an exception (i.e. fails) with an
--   error message.
--
-- * 'Functor' and 'Applicative', which follow the usual definitions.
--
-- * 'MonadPlus', where 'mzero' fails (with no error message) and
--   'mplus' executes the right-hand parser if the left-hand one
--   fails.
--
-- * 'Alternative', which follows 'MonadPlus'.
--
-- The 'Result' type is an instance of 'Functor', where 'fmap'
-- transforms the value in a 'Done' result.

-- | The result of a parse.
data Result r = Fail !B.ByteString [String] String
              -- ^ The parse failed.  The 'B.ByteString' is the input
              -- that had not yet been consumed when the failure
              -- occurred.  The @[@'String'@]@ is a list of contexts
              -- in which the error occurred.  The 'String' is the
              -- message describing the error, if any.
              | Partial (B.ByteString -> Result r)
              -- ^ Supply this continuation with more input so that
              -- the parser can resume.  To indicate that no more
              -- input is available, use an 'B.empty' string.
              | Done !B.ByteString r
              -- ^ The parse succeeded.  The 'B.ByteString' is the
              -- input that had not yet been consumed (if any) when
              -- the parse succeeded.

instance Show r => Show (Result r) where
    show (Fail bs stk msg) =
        "Fail " ++ show bs ++ " " ++ show stk ++ " " ++ show msg
    show (Partial _)       = "Partial _"
    show (Done bs r)       = "Done " ++ show bs ++ " " ++ show r

-- | If a parser has returned a 'Partial' result, supply it with more
-- input.
feed :: Result r -> B.ByteString -> Result r
feed f@(Fail _ _ _) _ = f
feed (Partial k) d    = k d
feed (Done bs r) d    = Done (B.append bs d) r

fmapR :: (a -> b) -> Result a -> Result b
fmapR _ (Fail st stk msg) = Fail st stk msg
fmapR f (Partial k)       = Partial (fmapR f . k)
fmapR f (Done bs r)       = Done bs (f r)

instance Functor Result where
    fmap = fmapR

-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => I.Parser a -> B.ByteString -> IO ()
parseTest p s = print (parse p s)

translate :: I.Result a -> Result a
translate (I.Fail st stk msg) = Fail (I.input st) stk msg
translate (I.Partial k)       = Partial (translate . k)
translate (I.Done st r)       = Done (I.input st) r

-- | Run a parser and return its result.
parse :: I.Parser a -> B.ByteString -> Result a
parse m s = translate (I.parse m s)
{-# INLINE parse #-}

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
parseWith :: Monad m =>
             (m B.ByteString)
          -- ^ An action that will be executed to provide the parser
          -- with more input, if necessary.  The action must return an
          -- 'B.empty' string when there is no more input available.
          -> I.Parser a
          -> B.ByteString
          -- ^ Initial input for the parser.
          -> m (Result a)
parseWith refill p s = step $ I.parse p s
  where step (I.Fail st stk msg) = return $! Fail (I.input st) stk msg
        step (I.Partial k)       = (step . k) =<< refill
        step (I.Done st r)       = return $! Done (I.input st) r
