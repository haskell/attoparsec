{-# LANGUAGE BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Combinator
-- Copyright   :  Bryan O'Sullivan 2009
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Useful parser combinators, similar to Parsec.
-- 
-----------------------------------------------------------------------------
module Data.Attoparsec.Combinator
    (
      choice
    , count
    , many
    , many1
    , manyTill
    , sepBy
    , sepBy1
    , skipMany
    , skipMany1
    ) where

import Control.Applicative

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty

many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)

sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []

sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure [])

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
