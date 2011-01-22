{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings, RecordWildCards #-}
-- |
-- Module      :  Data.Attoparsec.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2010
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'B.ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.Internal
    (
    -- * Parser types
      Parser
    , Result(..)
    , S(input)

    -- * Running parsers
    , parse

    -- * Combinators
    , (<?>)
    , try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , satisfy
    , satisfyWith
    , anyWord8
    , skip
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
    , scan
    , takeWhile
    , takeWhile1
    , takeTill

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
import Foreign.Storable (Storable(peek, sizeOf), peekByteOff)
import Prelude hiding (getChar, take, takeWhile)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

data Result r = Fail S [String] String
              | Partial (B.ByteString -> Result r)
              | Done S r

-- | The 'Parser' type is a monad.
newtype Parser a = Parser {
      runParser :: forall r. S
                -> Failure   r
                -> Success a r
                -> Result r
    }

type Failure   r = S -> [String] -> String -> Result r
type Success a r = S -> a -> Result r

-- | Have we read all available input?
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

data S = S {
      input  :: !B.ByteString
    , _added :: B.ByteString
    , more  :: !More
    } deriving (Show)

instance Show r => Show (Result r) where
    show (Fail _ stack msg) = "Fail " ++ show stack ++ " " ++ show msg
    show (Partial _) = "Partial _"
    show (Done bs r) = "Done " ++ show bs ++ " " ++ show r

addS :: S -> S -> S
addS (S s0 a0 c0) (S _s1 a1 c1) = S (s0 +++ a1) (a0 +++ a1) (mappend c0 c1)
{-# INLINE addS #-}

instance Monoid S where
    mempty  = S B.empty B.empty Incomplete
    mappend = addS

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP m g =
    Parser (\st0 kf ks -> runParser m st0 kf (\s a -> runParser (g a) s kf ks))
{-# INLINE bindP #-}

returnP :: a -> Parser a
returnP a = Parser (\st0 _kf ks -> ks st0 a)
{-# INLINE returnP #-}

instance Monad Parser where
    return = returnP
    (>>=)  = bindP
    fail   = failDesc

noAdds :: S -> S
noAdds (S s0 _a0 c0) = S s0 B.empty c0
{-# INLINE noAdds #-}

plus :: Parser a -> Parser a -> Parser a
plus a b = Parser $ \st0 kf ks ->
           let kf' st1 _ _ = runParser b (mappend st0 st1) kf ks
               !st2 = noAdds st0
           in  runParser a st2 kf' ks
{-# INLINE plus #-}

instance MonadPlus Parser where
    mzero = failDesc "mzero"
    {-# INLINE mzero #-}
    mplus = plus

fmapP :: (a -> b) -> Parser a -> Parser b
fmapP p m = Parser (\st0 f k -> runParser m st0 f (\s a -> k s (p a)))
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
    pure   = returnP
    (<*>)  = apP
    
    -- These definitions are equal to the defaults, but this
    -- way the optimizer doesn't have to work so hard to figure
    -- that out.
    (*>)   = (>>)
    x <* y = x >>= \a -> y >> return a

instance Monoid (Parser a) where
    mempty  = failDesc "mempty"
    {-# INLINE mempty #-}
    mappend = plus

instance Alternative Parser where
    empty = failDesc "empty"
    {-# INLINE empty #-}
    (<|>) = plus

failDesc :: String -> Parser a
failDesc err = Parser (\st0 kf _ks -> kf st0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

-- | Succeed only if at least @n@ bytes of input are available.
ensure :: Int -> Parser ()
ensure n = Parser $ \st0@(S s0 _a0 _c0) kf ks ->
    if B.length s0 >= n
    then ks st0 ()
    else runParser (demandInput >> ensure n) st0 kf ks

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: S -> (S -> Result r) -> (S -> Result r) -> Result r
prompt (S s0 a0 _c0) kf ks = Partial $ \s ->
    if B.null s
    then kf $! S s0 a0 Complete
    else ks $! S (s0 +++ s) (a0 +++ s) Incomplete

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Parser ()
demandInput = Parser $ \st0 kf ks ->
    if more st0 == Complete
    then kf st0 ["demandInput"] "not enough bytes"
    else prompt st0 (\st -> kf st ["demandInput"] "not enough bytes") (`ks` ())

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: Parser Bool
wantInput = Parser $ \st0@(S s0 _a0 c0) _kf ks ->
  case () of
    _ | not (B.null s0) -> ks st0 True
      | c0 == Complete  -> ks st0 False
      | otherwise       -> prompt st0 (`ks` False) (`ks` True)

get :: Parser B.ByteString
get  = Parser (\st0 _kf ks -> ks st0 (input st0))

put :: B.ByteString -> Parser ()
put s = Parser (\(S _s0 a0 c0) _kf ks -> ks (S s a0 c0) ())

(+++) :: B.ByteString -> B.ByteString -> B.ByteString
(+++) = B.append
{-# INLINE (+++) #-}

-- | Attempt a parse, and if it fails, rewind the input so that no
-- input appears to have been consumed.
--
-- This combinator is useful in cases where a parser might consume
-- some input before failing, i.e. the parser needs arbitrary
-- lookahead.  The downside to using this combinator is that it can
-- retain input for longer than is desirable.
try :: Parser a -> Parser a
try p = Parser $ \st0 kf ks ->
        runParser p (noAdds st0) (kf . mappend st0) ks

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  ensure 1
  s <- get
  let w = B.unsafeHead s
  if p w
    then put (B.unsafeTail s) >> return w
    else fail "satisfy"

-- | The parser @skip p@ succeeds for any byte for which the predicate
-- @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit w = w >= 48 && w <= 57
skip :: (Word8 -> Bool) -> Parser ()
skip p = do
  ensure 1
  s <- get
  if p (B.unsafeHead s)
    then put (B.unsafeTail s)
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  ensure 1
  s <- get
  let c = f (B.unsafeHead s)
  if p c
    then put (B.unsafeTail s) >> return c
    else fail "satisfyWith"

storable :: Storable a => Parser a
storable = hack undefined
 where
  hack :: Storable b => b -> Parser b
  hack dummy = do
    (fp,o,_) <- B.toForeignPtr `fmapP` take (sizeOf dummy)
    return . B.inlinePerformIO . withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

-- | Consume @n@ bytes of input, but succeed only if the predicate
-- returns 'True'.
takeWith :: Int -> (B.ByteString -> Bool) -> Parser B.ByteString
takeWith n p = do
  ensure n
  s <- get
  let (h,t) = B.splitAt n s
  if p h
    then put t >> return h
    else failDesc "takeWith"

-- | Consume exactly @n@ bytes of input.
take :: Int -> Parser B.ByteString
take n = takeWith n (const True)
{-# INLINE take #-}

-- | @string s@ parses a sequence of bytes that identically match
-- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- input if it fails (even if a partial match).
--
-- /Note/: The behaviour of this parser is different to that of the
-- similarly-named parser in Parsec, as this one is all-or-nothing.
-- To illustrate the difference, the following parser will fail under
-- Parsec given an input of @"for"@:
--
-- >string "foo" <|> string "for"
--
-- The reason for its failure is that that the first branch is a
-- partial match, and will consume the letters @\'f\'@ and @\'o\'@
-- before failing.  In Attoparsec, the above parser will /succeed/ on
-- that input, because the failed first branch will consume nothing.
string :: B.ByteString -> Parser B.ByteString
string s = takeWith (B.length s) (==s)
{-# INLINE string #-}

stringTransform :: (B.ByteString -> B.ByteString) -> B.ByteString
                -> Parser B.ByteString
stringTransform f s = takeWith (B.length s) ((==f s) . f)
{-# INLINE stringTransform #-}

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = go
 where
  go = do
    input <- wantInput
    when input $ do
      t <- B8.dropWhile p <$> get
      put t
      when (B.null t) go
{-# INLINE skipWhile #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeTill :: (Word8 -> Bool) -> Parser B.ByteString
takeTill p = takeWhile (not . p)
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeWhile :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile p = (B.concat . reverse) `fmap` go []
 where
  go acc = do
    input <- wantInput
    if input
      then do
        (h,t) <- B8.span p <$> get
        put t
        if B.null t
          then go (h:acc)
          else return (h:acc)
      else return acc

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
scan :: s -> (s -> Word8 -> Maybe s) -> Parser B.ByteString
scan s0 p = (B.concat . reverse) `fmap` go [] s0
 where
  go acc s1 = do
    input <- wantInput
    if input
      then do
        let scanner (B.PS fp off len) =
              withForeignPtr fp $ \ptr -> do
                let inner !i !s | i == off+len = return (i-off,s)
                                | otherwise = do
                                            w <- peekByteOff ptr i
                                            case p s w of
                                              Just s' -> inner (i+1) s'
                                              Nothing -> return (i-off,s)
                (i,s') <- inner off s1
                return (B.PS fp off i, B.PS fp (off+i) (len-i),s')
        (h,t,s') <- (unsafePerformIO . scanner) <$> get
        put t
        if B.null t
          then go (h:acc) s'
          else return (h:acc)
      else return acc
{-# INLINE scan #-}    

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
takeWhile1 :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile1 p = do
  (`when` demandInput) =<< B.null <$> get
  (h,t) <- B8.span p <$> get
  when (B.null h) $ failDesc "takeWhile1"
  put t
  if B.null t
    then (h+++) `fmapP` takeWhile p
    else return h

-- | Match any byte in a set.
--
-- >vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- >halfAlphabet = inClass "a-nA-N"
--
-- To add a literal @\'-\'@ to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Word8 -> Bool
inClass s = (`memberWord8` mySet)
    where mySet = charClass s
{-# INLINE inClass #-}

-- | Match any byte not in a set.
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

-- | Match only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = Parser $ \st0@S{..} kf ks ->
             if B.null input
             then if more == Complete
                  then ks st0 ()
                  else let kf' st1 _ _ = ks (mappend st0 st1) ()
                           ks' st1 _   = kf (mappend st0 st1) [] "endOfInput"
                       in  runParser demandInput st0 kf' ks'
             else kf st0 [] "endOfInput"
                                               
-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

--- | Name the parser, in case failure occurs.
(<?>) :: Parser a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser a
p <?> msg = Parser $ \s kf ks -> runParser p s (\s' strs m -> kf s' (msg:strs) m) ks
{-# INLINE (<?>) #-}
infix 0 <?>

-- | Terminal failure continuation.
failK :: Failure a
failK st0 stack msg = Fail st0 stack msg

-- | Terminal success continuation.
successK :: Success a a
successK state a = Done state a

-- | Run a parser.
parse :: Parser a -> B.ByteString -> Result a
parse m s = runParser m (S s B.empty Incomplete) failK successK
{-# INLINE parse #-}
