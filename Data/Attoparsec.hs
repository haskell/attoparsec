-- |
-- Module      :  Data.Attoparsec
-- Copyright   :  Bryan O'Sullivan 2007-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'B.ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec
    (
    -- * Parser types
      I.Parser
    , Result(..)

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
    , I.anyWord8
    , I.notWord8
    , I.word8
    , I.satisfy

    -- ** Byte classes
    , I.inClass
    , I.notInClass

    -- * Efficient string handling
    , I.string
    , I.skipWhile
    , I.stringTransform
    , I.take
    , I.takeTill
    , I.takeWhile
    , I.takeWhile1

    -- * State observation and manipulation functions
    , I.endOfInput
    , I.ensure
    ) where

import Data.Attoparsec.Combinator
import Prelude hiding (takeWhile)
import qualified Data.Attoparsec.Internal as I
import qualified Data.ByteString as B

-- | The result of a parse.
data Result r = Fail !B.ByteString [String] String
              -- ^ The parse failed.  The 'B.ByteString' is the input
              -- that had not yet been consumed when the failure
              -- occurred.  The @[@'String'@]@ is a list of contexts
              -- in which the error occurred.  The 'String' is the
              -- message describing the error, if any.
              | Partial (B.ByteString -> Result r)
              -- ^ Pass this continuation more input so that the
              -- parser can resume.  Pass it an 'B.empty' string to
              -- indicate that no more input is available.
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
