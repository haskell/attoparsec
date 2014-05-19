{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, OverloadedStrings,
    Rank2Types, RecordWildCards, TypeFamilies #-}
-- |
-- Module      :  Data.Attoparsec.Internal.Types
-- Copyright   :  Bryan O'Sullivan 2007-2014
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators, loosely based on the Parsec
-- library.

module Data.Attoparsec.Internal.Types
    (
      Parser(..)
    , Failure
    , Success
    , Pos(..)
    , IResult(..)
    , More(..)
    , (<>)
    ) where

import Control.Applicative (Alternative(..), Applicative(..), (<$>))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Prelude hiding (getChar, succ)

newtype Pos = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

-- | The result of a parse.  This is parameterised over the type @t@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult t r =
    Fail t [String] String
    -- ^ The parse failed.  The 't' parameter is the input that had
    -- not yet been consumed when the failure occurred.  The
    -- @[@'String'@]@ is a list of contexts in which the error
    -- occurred.  The 'String' is the message describing the error, if
    -- any.
  | Partial (t -> IResult t r)
    -- ^ Supply this continuation with more input so that the parser
    -- can resume.  To indicate that no more input is available, use
    -- an empty string.
  | Done t r
    -- ^ The parse succeeded.  The 't' parameter is the input that had
    -- not yet been consumed (if any) when the parse succeeded.

instance (Show t, Show r) => Show (IResult t r) where
    show (Fail t stk msg) =
      unwords [ "Fail", show t, show stk, show msg]
    show (Partial _)          = "Partial _"
    show (Done t r)       = unwords ["Done", show t, show r]

instance (NFData t, NFData r) => NFData (IResult t r) where
    rnf (Fail t stk msg) = rnf t `seq` rnf stk `seq` rnf msg
    rnf (Partial _)  = ()
    rnf (Done t r)   = rnf t `seq` rnf r
    {-# INLINE rnf #-}

instance Functor (IResult t) where
    fmap _ (Fail t stk msg) = Fail t stk msg
    fmap f (Partial k)      = Partial (fmap f . k)
    fmap f (Done t r)   = Done t (f r)

-- | The core parser type.  This is parameterised over the type @t@ of
-- string being processed.
--
-- This type is an instance of the following classes:
--
-- * 'Monad', where 'fail' throws an exception (i.e. fails) with an
--   error message.
--
-- * 'Functor' and 'Applicative', which follow the usual definitions.
--
-- * 'MonadPlus', where 'mzero' fails (with no error message) and
--   'mplus' executes the right-hand parser if the left-hand one
--   fails.  When the parser on the right executes, the input is reset
--   to the same state as the parser on the left started with. (In
--   other words, Attoparsec is a backtracking parser that supports
--   arbitrary lookahead.)
--
-- * 'Alternative', which follows 'MonadPlus'.
newtype Parser t a = Parser {
      runParser :: forall r. t -> Pos -> More
                -> Failure t   r
                -> Success t a r
                -> IResult t r
    }

type Failure t   r = t -> Pos -> More -> [String] -> String
                   -> IResult t r
type Success t a r = t -> Pos -> More -> a -> IResult t r

-- | Have we read all available input?
data More = Complete | Incomplete
            deriving (Eq, Show)

instance Monoid More where
    mappend c@Complete _ = c
    mappend _ m          = m
    mempty               = Incomplete

instance Monad (Parser t) where
    fail err = Parser $ \t pos more lose _succ -> lose t pos more [] msg
      where msg = "Failed reading: " ++ err
    {-# INLINE fail #-}

    return v = Parser $ \t pos more _lose succ -> succ t pos more v
    {-# INLINE return #-}

    m >>= k = Parser $ \t !pos more lose succ ->
        let succ' t' !pos' more' a = runParser (k a) t' pos' more' lose succ
        in runParser m t pos more lose succ'
    {-# INLINE (>>=) #-}

plus :: (Monoid t) => Parser t a -> Parser t a -> Parser t a
plus f g = Parser $ \t pos more lose succ ->
  let lose' t' _pos' more' _ctx _msg = runParser g t' pos more' lose succ
  in runParser f t pos more lose' succ

instance (Monoid t) => MonadPlus (Parser t) where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus = plus

instance Functor (Parser t) where
    fmap f p = Parser $ \t pos more lose succ ->
      let succ' t' pos' more' a = succ t' pos' more' (f a)
      in runParser p t pos more lose succ'
    {-# INLINE fmap #-}

apP :: Parser t (a -> b) -> Parser t a -> Parser t b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative (Parser t) where
    pure   = return
    {-# INLINE pure #-}
    (<*>)  = apP
    {-# INLINE (<*>) #-}

    -- These definitions are equal to the defaults, but this
    -- way the optimizer doesn't have to work so hard to figure
    -- that out.
    (*>)   = (>>)
    {-# INLINE (*>) #-}
    x <* y = x >>= \a -> y >> return a
    {-# INLINE (<*) #-}

instance (Monoid t) => Monoid (Parser t a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = plus
    {-# INLINE mappend #-}

instance (Monoid t) => Alternative (Parser t) where
    empty = fail "empty"
    {-# INLINE empty #-}

    (<|>) = plus
    {-# INLINE (<|>) #-}

    many v = many_v
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v
    {-# INLINE many #-}

    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE some #-}

(<>) :: (Monoid m) => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
