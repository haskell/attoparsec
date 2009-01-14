{-# LANGUAGE BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Combinator
-- Copyright   :  Daan Leijen 1999-2001, Bryan O'Sullivan 2009
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Useful parser combinators, similar to Parsec.
-- 
-----------------------------------------------------------------------------
module Data.Attoparsec.Combinator
    (
      choice
    , count
    , option
    , many1
    , manyTill
    , sepBy
    , sepBy1
    , skipMany
    , skipMany1
    , module Control.Applicative
    ) where

import Control.Applicative

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser.
choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- > priority  = option 0 (digitToInt <$> digit)
option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x

-- | @many1 p@ applies the parser @p@ /one/ or more times. Returns a
-- list of the returned values of @p@.
--
-- >  word  = many1 letter
many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p  = p `sepBy` (symbol ",")
sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p  = p `sepBy` (symbol ",")
sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = scan
    where scan = liftA2 (:) p ((s *> scan) <|> pure [])

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@.
-- This parser can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill anyChar (try (string "-->"))
--
-- Note the overlapping parsers @anyChar@ and @string \"<!--\"@, and
-- therefore the use of the 'try' combinator.
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan

-- | Skip zero or more instances of the parser.
skipMany :: Alternative f => f a -> f ()
skipMany p = scan
    where scan = (p *> scan) <|> pure ()

-- | Skip one or more instances of the parser.
skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p

-- | Apply the given parser repeatedly, returning every parse result.
count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p)
{-# INLINE count #-}
