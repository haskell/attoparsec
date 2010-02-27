{-# LANGUAGE Rank2Types, RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'bB.ByteString' strings,
-- loosely based on 'Text.ParserCombinators.Parsec'.
-- 
-----------------------------------------------------------------------------
module Data.Attoparsec.Internal
    (
    -- * Parser types
      Parser
    , Result(..)

    -- * Running parsers
    , parse
    , parseAll
    , feed

    -- * Combinators
    , (<?>)
    , try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , satisfy
    , satisfyWith
    , anyWord8
    , word8
    , notWord8

    -- ** Byte classes
    , inClass
    , notInClass

    -- * Parsing more complicated structures
    , storable

    -- * Efficient string handling
    , skipWhile
    , string
    , stringTransform
    , take
    , takeTill
    , takeWhile
    , takeWhile1

    -- * State observation and manipulation functions
    , endOfInput
    , ensure

    -- * Utilities
    , endOfLine
    ) where

import Control.Applicative (Alternative(..), Applicative(..), (<$>))
import Control.Monad (MonadPlus(..), when)
import Data.Attoparsec.Combinator
import Data.Attoparsec.FastSet (charClass, memberWord8)
import Data.Monoid (Monoid(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Prelude hiding (getChar, take, takeWhile)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

data Result r = Fail Input Added More [String] String
              | Partial (B.ByteString -> Result r)
              | Done Input Added More r

type Input = B.ByteString
type Added = B.ByteString

type S a = Input -> Added -> More -> a

-- | The Parser monad is an Exception and State monad.
newtype Parser a = Parser {
      runParser :: forall r. S
                   (Failure   r
                    -> Success a r
                    -> Result r)
    }

type Failure   r = S ([String] -> String -> Result r)
type Success a r = S (a -> Result r)

data More = Complete | Incomplete
            deriving (Eq, Show)

plusMore :: More -> More -> More
plusMore Complete _ = Complete
plusMore _ Complete = Complete
plusMore _ _        = Incomplete
{-# INLINE plusMore #-}

instance Monoid More where
    mempty  = Incomplete
    mappend = plusMore

instance Show r => Show (Result r) where
    show (Fail _ _ _ stack msg) = "Fail " ++ show stack ++ " " ++ show msg
    show (Partial _) = "Partial _"
    show (Done bs _ _ r) = "Done " ++ show bs ++ " " ++ show r

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP m g =
    Parser (\s0 a0 c0 kf ks -> runParser m s0 a0 c0 kf (\s1 a1 c1 a -> runParser (g a) s1 a1 c1 kf ks))
{-# INLINE bindP #-}

returnP :: a -> Parser a
returnP a = Parser (\s0 a0 c0 _kf ks -> ks s0 a0 c0 a)
{-# INLINE returnP #-}

instance Monad Parser where
    return = returnP
    (>>=)  = bindP
    fail   = failDesc

plus :: Parser a -> Parser a -> Parser a
plus a b = Parser $ \s0 a0 c0 kf ks ->
           let kf' s1 a1 c1 _ _ = runParser b (s0 +++ a1) (a0 +++ a1) (mappend c0 c1) kf ks
           in  runParser a s0 B.empty c0 kf' ks
{-# INLINE plus #-}

instance MonadPlus Parser where
    mzero = failDesc "mzero"
    mplus = plus

fmapP :: (a -> b) -> Parser a -> Parser b
fmapP p m = Parser (\s0 a0 c0 f k -> runParser m s0 a0 c0 f (\s1 a1 c1 a -> k s1 a1 c1 (p a)))
{-# INLINE fmapP #-}

instance Functor Parser where
    fmap = fmapP

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative Parser where
    pure  = returnP
    (<*>) = apP

instance Alternative Parser where
    empty = failDesc "empty"
    (<|>) = mplus

failDesc :: String -> Parser a
failDesc err = Parser (\s0 a0 c0 kf _ks -> kf s0 a0 c0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

ensure :: Int -> Parser ()
ensure n = Parser $ \s0 a0 c0 kf ks ->
    if B.length s0 >= n
    then ks s0 a0 c0 ()
    else runParser (requireInput >> ensure n) s0 a0 c0 kf ks

requireInput :: Parser ()
requireInput = Parser $ \s0 a0 c0 kf ks ->
    if c0 == Complete
    then kf s0 a0 c0 ["requireInput"] "not enough bytes"
    else Partial $ \s ->
         if B.null s
         then kf s0 a0 Complete ["requireInput"] "not enough bytes"
         else ks (s0 +++ s) (a0 +++ s) Incomplete ()

get :: Parser B.ByteString
get  = Parser $ \s0 a0 c0 _kf ks -> ks s0 a0 c0 s0

put :: B.ByteString -> Parser ()
put s = Parser $ \_s0 a0 c0 _kf ks -> ks s a0 c0 ()

take :: Int -> Parser B.ByteString
take n = takeWith n (const True)

(+++) :: B.ByteString -> B.ByteString -> B.ByteString
(+++) = B.append
{-# INLINE (+++) #-}

try :: Parser a -> Parser a
try p = Parser $ \s0 a0 c0 kf ks ->
        let kf' s1 a1 c1 = kf (s0 +++ a1) (a0 +++ a1) (mappend c0 c1)
        in runParser p s0 B.empty c0 kf' ks

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  ensure 1
  s <- get
  let w = B.unsafeHead s
  if p w
    then put (B.unsafeTail s) >> return w
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | Character parser.
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  ensure 1
  s <- get
  let c = f (B.unsafeHead s)
  if p c
    then put (B.unsafeTail s) >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

storable :: Storable a => Parser a
storable = hack undefined
 where
  hack :: Storable b => b -> Parser b
  hack dummy = do
    (fp,o,_) <- B.toForeignPtr `fmapP` take (sizeOf dummy)
    return . B.inlinePerformIO . withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

takeWith :: Int -> (B.ByteString -> Bool) -> Parser B.ByteString
takeWith n p = do
  ensure n
  s <- get
  let (h,t) = B.splitAt n s
  if p h
    then put t >> return h
    else failDesc "takeWith"

string :: B.ByteString -> Parser B.ByteString
string s = takeWith (B.length s) (==s)
{-# INLINE string #-}

stringTransform :: (B.ByteString -> B.ByteString) -> B.ByteString
                -> Parser B.ByteString
stringTransform f s = takeWith (B.length s) ((==s) . f)
{-# INLINE stringTransform #-}

skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = do
  (`when` requireInput) =<< B.null <$> get
  t <- B8.dropWhile p <$> get
  put t
  if B.null t
    then (skipWhile p <|> return ())
    else return ()

takeTill :: (Word8 -> Bool) -> Parser B.ByteString
takeTill p = takeWhile (not . p)
{-# INLINE takeTill #-}

takeWhile :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile p = go
 where
  go = do
    (`when` requireInput) =<< B.null <$> get
    (h,t) <- B8.span p <$> get
    put t
    if B.null t
      then (h+++) `fmapP` (go <|> return B.empty)
      else return h

takeWhile1 :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile1 p = do
  (`when` requireInput) =<< B.null <$> get
  (h,t) <- B8.span p <$> get
  when (B.null h) $ failDesc "takeWhile1"
  put t
  if B.null t
    then (h+++) `fmapP` (takeWhile p <|> return B.empty)
    else return h

-- | Match any character in a set.
--
-- > vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- > halfAlphabet = inClass "a-nA-N"
--
-- To add a literal \'-\' to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Word8 -> Bool
inClass s = (`memberWord8` mySet)
    where mySet = charClass s
{-# INLINE inClass #-}

-- | Match any character not in a set.
notInClass :: String -> Word8 -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Match any byte.
anyWord8 :: Parser Word8
anyWord8 = satisfy $ const True
{-# INLINE anyWord8 #-}

-- | Match a specific byte.
word8 :: Word8 -> Parser Word8
word8 c = satisfy (== c) <?> show c
{-# INLINE word8 #-}

-- | Match any byte except the given one.
notWord8 :: Word8 -> Parser Word8
notWord8 c = satisfy (/= c) <?> "not " ++ show c
{-# INLINE notWord8 #-}

endOfInput :: Parser ()
endOfInput = Parser $ \s0 a0 c0 kf ks ->
             if B.null s0
             then if c0 == Complete
                  then ks s0 a0 c0 ()
                  else let kf' s1 a1 c1 _ _ = ks (s0 +++ a1) (a0 +++ a1) (mappend c0 c1) ()
                           ks' s1 a1 c1 _   = kf (s0 +++ a1) (a0 +++ a1) (mappend c0 c1) [] "endOfInput"
                       in  runParser requireInput s0 a0 c0 kf' ks'
             else kf s0 a0 c0 [] "endOfInput"
                                               
endOfLine :: Parser ()
endOfLine = (word8 10 >> return ()) <|> (string (B.pack "\r\n") >> return ())

--- | Name the parser, in case failure occurs.
(<?>) :: Parser a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser a
p <?> _msg = p
{-# INLINE (<?>) #-}
infix 0 <?>

failK :: Failure a
failK st0 stack msg = Fail st0 stack msg

successK :: Success a a
successK state a = Done state a

parse :: Parser a -> B.ByteString -> Result a
parse m s = runParser m s B.empty Incomplete failK successK
{-# INLINE parse #-}
              
feed :: Result r -> B.ByteString -> Result r
feed f@(Fail _ _ _ _ _) _ = f
feed (Partial k) d = k d
feed (Done s a c r) d = Done (s +++ d) a c r

parseAll :: Parser a -> [B.ByteString] -> Result a
parseAll p ss = case ss of
                  []     -> go (parse p B.empty) []
                  (c:cs) -> go (parse p c) cs
  where go (Partial k) (c:cs) = go (k c) cs
        go (Partial k) []     = k B.empty
        go r           _      = r
