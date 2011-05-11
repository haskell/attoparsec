{-# LANGUAGE BangPatterns, CPP #-}
-- |
-- Module      :  Data.Attoparsec.Combinator
-- Copyright   :  Daan Leijen 1999-2001, Bryan O'Sullivan 2009-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Useful parser combinators, similar to those provided by Parsec.
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
    , eitherP

    -- * Inlined implementations of existing functions
    --
    -- These are exact duplicates of functions already exported by the
    -- 'Control.Applicative' module, but whose definitions are
    -- inlined.  In many cases, this leads to 2x performance
    -- improvements.
    , many
    ) where

import Control.Applicative (Alternative, Applicative(..), empty, liftA2,
                            (<|>), (*>), (<$>))
import Data.Attoparsec.Internal.Types (Parser)

-- | @choice ps@ tries to apply the actions in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- action.
choice :: Alternative f => [f a] -> f a
{-# SPECIALIZE choice :: [Parser a] -> Parser a #-}
choice = foldr (<|>) empty

-- | @option x p@ tries to apply action @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- > priority  = option 0 (digitToInt <$> digit)
option :: Alternative f => a -> f a -> f a
{-# SPECIALIZE option :: a -> Parser a -> Parser a #-}
option x p = p <|> pure x

-- | @many1 p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@.
--
-- >  word  = many1 letter
many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)
{-# INLINE many1 #-}

-- | @sepBy p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy` (symbol ",")
sepBy :: Alternative f => f a -> f s -> f [a]
{-# SPECIALIZE sepBy :: Parser a -> Parser s -> Parser [a] #-}
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []

-- | @sepBy1 p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy` (symbol ",")
sepBy1 :: Alternative f => f a -> f s -> f [a]
{-# SPECIALIZE sepBy1 :: Parser a -> Parser s -> Parser [a] #-}
sepBy1 p s = scan
    where scan = liftA2 (:) p ((s *> scan) <|> pure [])

-- | @manyTill p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill anyChar (try (string "-->"))
--
-- Note the overlapping parsers @anyChar@ and @string \"<!--\"@, and
-- therefore the use of the 'try' combinator.
manyTill :: Alternative f => f a -> f b -> f [a]
{-# SPECIALIZE manyTill :: Parser a -> Parser b -> Parser [a] #-}
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan

-- | Skip zero or more instances of an action.
skipMany :: Alternative f => f a -> f ()
{-# SPECIALIZE skipMany :: Parser a -> Parser () #-}
skipMany p = scan
    where scan = (p *> scan) <|> pure ()

-- | Skip one or more instances of an action.
skipMany1 :: Alternative f => f a -> f ()
{-# SPECIALIZE skipMany1 :: Parser a -> Parser () #-}
skipMany1 p = p *> skipMany p

-- | Apply the given action repeatedly, returning every result.
count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p)
{-# INLINE count #-}

-- | Combine two alternatives.
eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINE eitherP #-}

-- | Zero or more.
many :: (Alternative f) => f a -> f [a]
many v = many_v
    where many_v = some_v <|> pure []
	  some_v = (:) <$> v <*> many_v
{-# INLINE many #-}
