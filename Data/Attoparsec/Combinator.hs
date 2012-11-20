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
    , many'
    , many1
    , many1'
    , manyTill
    , manyTill'
    , sepBy
    , sepBy'
    , sepBy1
    , sepBy1'
    , skipMany
    , skipMany1
    , eitherP
    ) where

import Control.Applicative (Alternative(..), Applicative(..), empty, liftA2,
                            (<|>), (*>), (<$>))
#if !MIN_VERSION_base(4,2,0)
import Control.Applicative (many)
#endif

#if __GLASGOW_HASKELL__ >= 700
import Data.Attoparsec.Internal.Types (Parser)
import qualified Data.Attoparsec.Zepto as Z
import Data.ByteString (ByteString)
import Data.Text (Text)
#endif

-- | @choice ps@ tries to apply the actions in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- action.
choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE choice :: [Parser ByteString a] -> Parser ByteString a #-}
{-# SPECIALIZE choice :: [Parser Text a] -> Parser Text a #-}
{-# SPECIALIZE choice :: [Z.Parser a] -> Z.Parser a #-}
#endif

-- | @option x p@ tries to apply action @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- > priority  = option 0 (digitToInt <$> digit)
option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE option :: a -> Parser ByteString a -> Parser ByteString a #-}
{-# SPECIALIZE option :: a -> Parser Text a -> Parser Text a #-}
{-# SPECIALIZE option :: a -> Z.Parser a -> Z.Parser a #-}
#endif

-- | @many' p@ applies the action @p@ /zero/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many' letter
many' :: (Alternative f, Monad f) => f a -> f [a]
many' p = many_p
  where many_p = some_p <|> pure []
        some_p = do
            !a <- p
            as <- many_p
            return (a : as)
{-# INLINE many' #-}

-- | @many1 p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@.
--
-- >  word  = many1 letter
many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)
{-# INLINE many1 #-}

-- | @many1' p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many1' letter
many1' :: (Alternative f, Monad f) => f a -> f [a]
many1' p = do
    !a <- p
    as <- many' p
    return (a : as)
{-# INLINE many1' #-}

-- | @sepBy p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy` (symbol ",")
sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE sepBy :: Parser ByteString a -> Parser ByteString s
                     -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}
#endif

-- | @sepBy' p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy'` (symbol ",")
sepBy' :: (Alternative f, Monad f) => f a -> f s -> f [a]
sepBy' p s = scan <|> pure []
  where
    scan = do
        !a <- p
        as <- (s *> sepBy1' p s) <|> pure []
        return (a : as)
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE sepBy' :: Parser ByteString a -> Parser ByteString s
                      -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy' :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy' :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}
#endif

-- | @sepBy1 p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy1` (symbol ",")
sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = scan
    where scan = liftA2 (:) p ((s *> scan) <|> pure [])
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE sepBy1 :: Parser ByteString a -> Parser ByteString s
                      -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy1 :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy1 :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}
#endif

-- | @sepBy1' p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy1'` (symbol ",")
sepBy1' :: (Alternative f, Monad f) => f a -> f s -> f [a]
sepBy1' p s = scan
    where scan = do !a <- p
                    as <- (s *> scan) <|> pure []
                    return (a : as)
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE sepBy1' :: Parser ByteString a -> Parser ByteString s
                       -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy1' :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy1' :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}
#endif

-- | @manyTill p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill anyChar (try (string "-->"))
--
-- Note the overlapping parsers @anyChar@ and @string \"<!--\"@, and
-- therefore the use of the 'try' combinator.
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE manyTill :: Parser ByteString a -> Parser ByteString b
                        -> Parser ByteString [a] #-}
{-# SPECIALIZE manyTill :: Parser Text a -> Parser Text b -> Parser Text [a] #-}
{-# SPECIALIZE manyTill :: Z.Parser a -> Z.Parser b -> Z.Parser [a] #-}
#endif

-- | @manyTill' p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill' anyChar (try (string "-->"))
--
-- Note the overlapping parsers @anyChar@ and @string \"<!--\"@, and
-- therefore the use of the 'try' combinator. The value returned by @p@
-- is forced to WHNF.
manyTill' :: (Alternative f, Monad f) => f a -> f b -> f [a]
manyTill' p end = scan
    where scan = (end *> pure []) <|> scan'
          scan' = do
              !a <- p
              as <- scan
              return (a : as)
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE manyTill' :: Parser ByteString a -> Parser ByteString b
                         -> Parser ByteString [a] #-}
{-# SPECIALIZE manyTill' :: Parser Text a -> Parser Text b -> Parser Text [a] #-}
{-# SPECIALIZE manyTill' :: Z.Parser a -> Z.Parser b -> Z.Parser [a] #-}
#endif

-- | Skip zero or more instances of an action.
skipMany :: Alternative f => f a -> f ()
skipMany p = scan
    where scan = (p *> scan) <|> pure ()
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE skipMany :: Parser ByteString a -> Parser ByteString () #-}
{-# SPECIALIZE skipMany :: Parser Text a -> Parser Text () #-}
{-# SPECIALIZE skipMany :: Z.Parser a -> Z.Parser () #-}
#endif

-- | Skip one or more instances of an action.
skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE skipMany1 :: Parser ByteString a -> Parser ByteString () #-}
{-# SPECIALIZE skipMany1 :: Parser Text a -> Parser Text () #-}
{-# SPECIALIZE skipMany1 :: Z.Parser a -> Z.Parser () #-}
#endif

-- | Apply the given action repeatedly, returning every result.
count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p)
{-# INLINE count #-}

-- | Combine two alternatives.
eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINE eitherP #-}
