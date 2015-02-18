{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Data.ByteString.Unsafe
#endif
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

-- |
-- Module      :  Data.Attoparsec.Zepto
-- Copyright   :  Bryan O'Sullivan 2007-2014
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A tiny, highly specialized combinator parser for 'B.ByteString'
-- strings.
--
-- While the main attoparsec module generally performs well, this
-- module is particularly fast for simple non-recursive loops that
-- should not normally result in failed parses.
--
-- /Warning/: on more complex inputs involving recursion or failure,
-- parsers based on this module may be as much as /ten times slower/
-- than regular attoparsec! You should /only/ use this module when you
-- have benchmarks that prove that its use speeds your code up.
module Data.Attoparsec.Zepto
    (
      Parser
    , parse
    , atEnd
    , string
    , take
    , takeWhile
    ) where

import Data.Word (Word8)
import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)
import Prelude hiding (take, takeWhile)

newtype S = S {
      input :: ByteString
    }

data Result a = Fail String
              | OK !a

-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
newtype Parser a = Parser {
      runParser :: S -> (# Result a, S #)
    }

instance Functor Parser where
    fmap f m = Parser $ \s -> case runParser m s of
                                (# OK a, s' #)     -> (# OK (f a), s' #)
                                (# Fail err, s' #) -> (# Fail err, s' #)
    {-# INLINE fmap #-}

instance Monad Parser where
    return a = Parser $ \s -> (# OK a, s #)
    {-# INLINE return #-}

    m >>= k   = Parser $ \s -> case runParser m s of
                                 (# OK a, s' #) -> runParser (k a) s'
                                 (# Fail err, s' #) -> (# Fail err, s' #)
    {-# INLINE (>>=) #-}

    fail msg = Parser $ \s -> (# Fail msg, s #)

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}

    mplus a b = Parser $ \s ->
                case runParser a s of
                  (# ok@(OK _), s' #) -> (# ok, s' #)
                  (# _, _ #) -> case runParser b s of
                                   (# ok@(OK _), s'' #) -> (# ok, s'' #)
                                   (# err, s'' #) -> (# err, s'' #)
    {-# INLINE mplus #-}

instance Applicative Parser where
    pure   = return
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

gets :: (S -> a) -> Parser a
gets f = Parser $ \s -> (# OK (f s), s #)
{-# INLINE gets #-}

put :: S -> Parser ()
put s = Parser $ \_ -> (# OK (), s #)
{-# INLINE put #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p bs = case runParser p (S bs) of
               (# OK a, _ #) -> Right a
               (# Fail err, _ #) -> Left err

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

-- | Consume input while the predicate returns 'True'.
takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile p = do
  (h,t) <- gets (B.span p . input)
  put (S t)
  return h
{-# INLINE takeWhile #-}

-- | Consume @n@ bytes of input.
take :: Int -> Parser ByteString
take !n = do
  s <- gets input
  if B.length s >= n
    then put (S (B.unsafeDrop n s)) >> return (B.unsafeTake n s)
    else fail "insufficient input"
{-# INLINE take #-}

-- | Match a string exactly.
string :: ByteString -> Parser ()
string s = do
  i <- gets input
  if s `B.isPrefixOf` i
    then put (S (B.unsafeDrop (B.length s) i)) >> return ()
    else fail "string"
{-# INLINE string #-}

-- | Indicate whether the end of the input has been reached.
atEnd :: Parser Bool
atEnd = do
  i <- gets input
  return $! B.null i
{-# INLINE atEnd #-}
