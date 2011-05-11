{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings, RecordWildCards #-}
-- |
-- Module      :  Data.Attoparsec.Internal.Types
-- Copyright   :  Bryan O'Sullivan 2007-2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'B.ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.Internal.Types
    (
      Parser(..)
    , Failure
    , Success
    , Result(..)
    , Input(..)
    , Added(..)
    , More(..)
    , addS
    , noAdds
    , (+++)
    ) where

import Control.Applicative (Alternative(..), Applicative(..))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(..))
import Data.Monoid (Monoid(..))
import Prelude hiding (getChar, take, takeWhile)
import qualified Data.ByteString.Char8 as B

-- | The result of a parse.
data Result r = Fail B.ByteString [String] String
              -- ^ The parse failed.  The 'B.ByteString' is the input
              -- that had not yet been consumed when the failure
              -- occurred.  The @[@'String'@]@ is a list of contexts
              -- in which the error occurred.  The 'String' is the
              -- message describing the error, if any.
              | Partial (B.ByteString -> Result r)
              -- ^ Supply this continuation with more input so that
              -- the parser can resume.  To indicate that no more
              -- input is available, use an 'B.empty' string.
              | Done B.ByteString r
              -- ^ The parse succeeded.  The 'B.ByteString' is the
              -- input that had not yet been consumed (if any) when
              -- the parse succeeded.

instance Show r => Show (Result r) where
    show (Fail bs stk msg) =
        "Fail " ++ show bs ++ " " ++ show stk ++ " " ++ show msg
    show (Partial _)       = "Partial _"
    show (Done bs r)       = "Done " ++ show bs ++ " " ++ show r

instance (NFData r) => NFData (Result r) where
    rnf (Fail _ _ _) = ()
    rnf (Partial _)  = ()
    rnf (Done _ r)   = rnf r
    {-# INLINE rnf #-}

fmapR :: (a -> b) -> Result a -> Result b
fmapR _ (Fail st stk msg) = Fail st stk msg
fmapR f (Partial k)       = Partial (fmapR f . k)
fmapR f (Done bs r)       = Done bs (f r)

instance Functor Result where
    fmap = fmapR
    {-# INLINE fmap #-}

newtype Input = I {unI :: B.ByteString}
newtype Added = A {unA :: B.ByteString}

-- | The 'Parser' type is a monad.
newtype Parser a = Parser {
      runParser :: forall r. Input -> Added -> More
                -> Failure   r
                -> Success a r
                -> Result r
    }

type Failure   r = Input -> Added -> More -> [String] -> String -> Result r
type Success a r = Input -> Added -> More -> a -> Result r

-- | Have we read all available input?
data More = Complete | Incomplete
            deriving (Eq, Show)

addS :: Input -> Added -> More
     -> Input -> Added -> More
     -> (Input -> Added -> More -> r) -> r
addS i0 a0 m0 _i1 a1 m1 f =
    let !i = I (unI i0 +++ unA a1)
        a  = A (unA a0 +++ unA a1)
        !m = m0 <> m1
    in f i a m
  where
    Complete <> _ = Complete
    _ <> Complete = Complete
    _ <> _        = Incomplete
{-# INLINE addS #-}

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP m g =
    Parser $ \i0 a0 m0 kf ks -> runParser m i0 a0 m0 kf $
                                \i1 a1 m1 a -> runParser (g a) i1 a1 m1 kf ks
{-# INLINE bindP #-}

returnP :: a -> Parser a
returnP a = Parser (\i0 a0 m0 _kf ks -> ks i0 a0 m0 a)
{-# INLINE returnP #-}

instance Monad Parser where
    return = returnP
    (>>=)  = bindP
    fail   = failDesc

noAdds :: Input -> Added -> More
       -> (Input -> Added -> More -> r) -> r
noAdds i0 _a0 m0 f = f i0 (A B.empty) m0
{-# INLINE noAdds #-}

plus :: Parser a -> Parser a -> Parser a
plus a b = Parser $ \i0 a0 m0 kf ks ->
           let kf' i1 a1 m1 _ _ = addS i0 a0 m0 i1 a1 m1 $
                                  \ i2 a2 m2 -> runParser b i2 a2 m2 kf ks
           in  noAdds i0 a0 m0 $ \i2 a2 m2 -> runParser a i2 a2 m2 kf' ks
{-# INLINE plus #-}

instance MonadPlus Parser where
    mzero = failDesc "mzero"
    {-# INLINE mzero #-}
    mplus = plus

fmapP :: (a -> b) -> Parser a -> Parser b
fmapP p m = Parser $ \i0 a0 m0 f k ->
            runParser m i0 a0 m0 f $ \i1 a1 s1 a -> k i1 a1 s1 (p a)
{-# INLINE fmapP #-}

instance Functor Parser where
    fmap = fmapP
    {-# INLINE fmap #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative Parser where
    pure   = returnP
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

instance Monoid (Parser a) where
    mempty  = failDesc "mempty"
    {-# INLINE mempty #-}
    mappend = plus
    {-# INLINE mappend #-}

instance Alternative Parser where
    empty = failDesc "empty"
    {-# INLINE empty #-}
    (<|>) = plus
    {-# INLINE (<|>) #-}

failDesc :: String -> Parser a
failDesc err = Parser (\i0 a0 m0 kf _ks -> kf i0 a0 m0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

(+++) :: B.ByteString -> B.ByteString -> B.ByteString
(+++) = B.append
{-# INLINE (+++) #-}
