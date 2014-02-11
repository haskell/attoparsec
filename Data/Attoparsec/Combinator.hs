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
    -- * Combinators
      try
    , (<?>)
    , choice
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
    -- * Parsing individual chunk elements
    , satisfyElem
    -- * State observation and manipulation functions
    , endOfInput
    , atEnd
    ) where

import Control.Applicative (Alternative(..), Applicative(..), empty, liftA2,
                            (<|>), (*>), (<$>))
import Control.Monad (MonadPlus(..))
#if !MIN_VERSION_base(4,2,0)
import Control.Applicative (many)
#endif

import Data.Attoparsec.Internal.Types
import Data.Attoparsec.Internal
#if __GLASGOW_HASKELL__ >= 700
import qualified Data.Attoparsec.Zepto as Z
import Data.ByteString (ByteString)
import Data.Text (Text)
#endif

-- | Attempt a parse, and if it fails, rewind the input so that no
-- input appears to have been consumed.
--
-- This combinator is provided for compatibility with Parsec.
-- Attoparsec parsers always backtrack on failure.
try :: Parser t a -> Parser t a
try p = p
{-# INLINE try #-}

-- | Name the parser, in case failure occurs.
(<?>) :: Parser t a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser t a
p <?> msg0 = Parser $ \i0 a0 m0 kf ks ->
             let kf' i a m strs msg = kf i a m (msg0:strs) msg
             in runParser p i0 a0 m0 kf' ks
{-# INLINE (<?>) #-}
infix 0 <?>

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

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)
{-# INLINE liftM2' #-}

-- | @many' p@ applies the action @p@ /zero/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many' letter
many' :: (MonadPlus m) => m a -> m [a]
many' p = many_p
  where many_p = some_p `mplus` return []
        some_p = liftM2' (:) p many_p
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
many1' :: (MonadPlus m) => m a -> m [a]
many1' p = liftM2' (:) p (many' p)
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
sepBy' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy' p s = scan `mplus` return []
  where scan = liftM2' (:) p ((s >> sepBy1' p s) `mplus` return [])
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
sepBy1' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy1' p s = scan
    where scan = liftM2' (:) p ((s >> scan) `mplus` return [])
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
manyTill' :: (MonadPlus m) => m a -> m b -> m [a]
manyTill' p end = scan
    where scan = (end >> return []) `mplus` liftM2' (:) p scan
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

-- | The parser @satisfyElem p@ succeeds for any chunk element for which the
-- predicate @p@ returns 'True'. Returns the element that is
-- actually parsed.
--
-- >digit = satisfyElem isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
satisfyElem :: Chunk t => (ChunkElem t -> Bool) -> Parser t (ChunkElem t)
satisfyElem p = do
  c <- ensure 1
  let !h = unsafeChunkHead c
  if p h
    then put (unsafeChunkTail c) >> return h
    else fail "satisfyElem"
{-# INLINE satisfyElem #-}

-- | Match only if all input has been consumed.
endOfInput :: Chunk t => Parser t ()
endOfInput = Parser $ \i0 a0 m0 kf ks ->
             if nullChunk (unI i0)
             then if m0 == Complete
                  then ks i0 a0 m0 ()
                  else let kf' i1 a1 m1 _ _ = addS i0 a0 m0 i1 a1 m1 $
                                              \ i2 a2 m2 -> ks i2 a2 m2 ()
                           ks' i1 a1 m1 _   = addS i0 a0 m0 i1 a1 m1 $
                                              \ i2 a2 m2 -> kf i2 a2 m2 []
                                                            "endOfInput"
                       in  runParser demandInput i0 a0 m0 kf' ks'
             else kf i0 a0 m0 [] "endOfInput"
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE endOfInput :: Parser ByteString () #-}
{-# SPECIALIZE endOfInput :: Parser Text () #-}
#endif

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Chunk t => Parser t Bool
atEnd = not <$> wantInput
{-# INLINE atEnd #-}
