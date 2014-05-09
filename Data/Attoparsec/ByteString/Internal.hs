{-# LANGUAGE BangPatterns, Rank2Types, OverloadedStrings,
    RecordWildCards, MagicHash, UnboxedTuples #-}
-- |
-- Module      :  Data.Attoparsec.ByteString.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'B.ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.ByteString.Internal
    (
    -- * Parser types
      Parser
    , Result

    -- * Running parsers
    , parse
    , parseOnly

    -- * Combinators
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , satisfy
    , satisfyWith
    , anyWord8
    , skip
    , word8
    , notWord8

    -- ** Lookahead
    , peekWord8
    , peekWord8'

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
    , runScanner
    , takeWhile
    , takeWhile1
    , takeTill

    -- ** Consume all remaining input
    , takeByteString
    , takeLazyByteString

    -- * Utilities
    , endOfLine
    , endOfInput
    , match
    , atEnd
    ) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (when)
import Data.Attoparsec.ByteString.FastSet (charClass, memberWord8)
import Data.Attoparsec.Combinator ((<?>))
import Data.Attoparsec.Internal.Types hiding (Parser, Failure, Success)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, minusPtr, plusPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Prelude hiding (getChar, succ, take, takeWhile)
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B

import GHC.Base (realWorld#)
import GHC.IO (IO(IO))

type Parser = T.Parser B.ByteString
type Result = IResult B.ByteString
type Failure r = T.Failure B.ByteString r
type Success a r = T.Success B.ByteString a r

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  c <- ensure 1
  let !h = B.unsafeHead c
  if p h
    then advance 1 >> return h
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @skip p@ succeeds for any byte for which the predicate
-- @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit w = w >= 48 && w <= 57
skip :: (Word8 -> Bool) -> Parser ()
skip p = do
  s <- ensure 1
  if p (B.unsafeHead s)
    then advance 1
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  s <- ensure 1
  let c = f $! B.unsafeHead s
  if p c
    then advance 1 >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

storable :: Storable a => Parser a
storable = hack undefined
 where
  hack :: Storable b => b -> Parser b
  hack dummy = do
    (fp,o,_) <- B.toForeignPtr `fmap` take (sizeOf dummy)
    return . B.inlinePerformIO . withForeignPtr fp $ \p ->
        peek (castPtr $ p `plusPtr` o)

-- | Consume @n@ bytes of input, but succeed only if the predicate
-- returns 'True'.
takeWith :: Int -> (B.ByteString -> Bool) -> Parser B.ByteString
takeWith n0 p = do
  let n = max n0 0
  s <- ensure n
  if p s
    then advance n >> return s
    else fail "takeWith"

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
-- Parsec given an input of @\"for\"@:
--
-- >string "foo" <|> string "for"
--
-- The reason for its failure is that the first branch is a
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
    t <- B8.takeWhile p <$> get
    advance (B.length t)
    eoc <- endOfChunk
    when eoc $ do
      input <- wantInput
      when input go
{-# INLINE skipWhile #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
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
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
takeWhile :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile p = (B.concat . reverse) `fmap` go []
 where
  go acc = do
    s <- B8.takeWhile p <$> get
    advance (B.length s)
    eoc <- endOfChunk
    if eoc
      then do
        input <- wantInput
        if input
          then go (s:acc)
          else return (s:acc)
      else return (s:acc)
{-# INLINE takeWhile #-}

takeRest :: Parser [B.ByteString]
takeRest = go []
 where
  go acc = do
    input <- wantInput
    if input
      then do
        s <- get
        advance (B.length s)
        go (s:acc)
      else return (reverse acc)

-- | Consume all remaining input and return it as a single string.
takeByteString :: Parser B.ByteString
takeByteString = B.concat `fmap` takeRest

-- | Consume all remaining input and return it as a single string.
takeLazyByteString :: Parser L.ByteString
takeLazyByteString = L.fromChunks `fmap` takeRest

data T s = T {-# UNPACK #-} !Int s

scan_ :: (s -> [B.ByteString] -> Parser r) -> s -> (s -> Word8 -> Maybe s)
         -> Parser r
scan_ f s0 p = go [] s0
 where
  go acc s1 = do
    let scanner (B.PS fp off len) =
          withForeignPtr fp $ \ptr0 -> do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
                inner ptr !s
                  | ptr < end = do
                    w <- peek ptr
                    case p s w of
                      Just s' -> inner (ptr `plusPtr` 1) s'
                      _       -> done (ptr `minusPtr` start) s
                  | otherwise = done (ptr `minusPtr` start) s
                done !i !s = return (T i s)
            inner start s1
    bs <- get
    let T i s' = inlinePerformIO $ scanner bs
        !h = B.unsafeTake i bs
    advance i
    eoc <- endOfChunk
    if eoc
      then do
        input <- wantInput
        if input
          then go (h:acc) s'
          else f s' (h:acc)
      else f s' (h:acc)
{-# INLINE scan_ #-}

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
scan :: s -> (s -> Word8 -> Maybe s) -> Parser B.ByteString
scan = scan_ $ \_ chunks ->
  case chunks of
    [x] -> return x
    xs  -> return $! B.concat $ reverse xs
{-# INLINE scan #-}

-- | Like 'scan', but generalized to return the final state of the
-- scanner.
runScanner :: s -> (s -> Word8 -> Maybe s) -> Parser (B.ByteString, s)
runScanner = scan_ $ \s xs -> return (B.concat (reverse xs), s)
{-# INLINE runScanner #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
takeWhile1 :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile1 p = do
  (`when` demandInput) =<< endOfChunk
  s <- B8.takeWhile p <$> get
  let len = B.length s
  if len == 0
    then fail "takeWhile1"
    else do
      advance len
      eoc <- endOfChunk
      if eoc
        then (s<>) `fmap` takeWhile p
        else return s

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
          {-# NOINLINE mySet #-}
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

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
peekWord8 :: Parser (Maybe Word8)
peekWord8 = T.Parser $ \t pos@(Pos pos_) more _lose succ ->
  case () of
    _| pos_ < B.length t ->
       let !w = B.unsafeIndex t pos_
       in succ t pos more (Just w)
     | more == Complete ->
       succ t pos more Nothing
     | otherwise ->
       let succ' t' pos' more' = let !w = B.unsafeIndex t pos_
                                 in succ t' pos' more' (Just w)
           lose' t' pos' more' = succ t' pos' more' Nothing
       in prompt t pos more lose' succ'
{-# INLINE peekWord8 #-}

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
peekWord8' :: Parser Word8
peekWord8' = do
  s <- ensure 1
  return $! B.unsafeHead s
{-# INLINE peekWord8' #-}

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

-- | Terminal failure continuation.
failK :: Failure a
failK t (Pos pos) _more stack msg = Fail (B.unsafeDrop pos t) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: Success a a
successK t (Pos pos) _more a = Done (B.unsafeDrop pos t) a
{-# INLINE successK #-}

-- | Run a parser.
parse :: Parser a -> B.ByteString -> Result a
parse m s = T.runParser m s (Pos 0) Incomplete failK successK
{-# INLINE parse #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: Parser a -> B.ByteString -> Either String a
parseOnly m s = case T.runParser m s (Pos 0) Complete failK successK of
                  Fail _ _ err -> Left err
                  Done _ a     -> Right a
                  _            -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
{-# INLINE inlinePerformIO #-}

get :: Parser ByteString
get = T.Parser $ \t pos more _lose succ ->
  succ t pos more (B.unsafeDrop (fromPos pos) t)
{-# INLINE get #-}

endOfChunk :: Parser Bool
endOfChunk = T.Parser $ \t pos more _lose succ ->
  succ t pos more (fromPos pos == B.length t)
{-# INLINE endOfChunk #-}

advance :: Int -> Parser ()
advance n = T.Parser $ \t pos more _lose succ ->
  succ t (pos + Pos n) more ()
{-# INLINE advance #-}

ensureSuspended :: Int -> ByteString -> Pos -> More
                -> Failure r
                -> Success ByteString r
                -> Result r
ensureSuspended n t pos more lose succ =
    runParser (demandInput >> go) t pos more lose succ
  where go = T.Parser $ \t' pos' more' lose' succ' ->
          if lengthAtLeast pos' n t'
          then succ' t' pos' more' (substring pos (Pos n) t')
          else runParser (demandInput >> go) t' pos' more' lose' succ'

-- | If at least @n@ elements of input are available, return the
-- current input, otherwise fail.
ensure :: Int -> Parser ByteString
ensure n = T.Parser $ \t pos more lose succ ->
    if lengthAtLeast pos n t
    then succ t pos more (substring pos (Pos n) t)
    -- The uncommon case is kept out-of-line to reduce code size:
    else ensureSuspended n t pos more lose succ
-- Non-recursive so the bounds check can be inlined:
{-# INLINE ensure #-}

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: ByteString -> Pos -> More
       -> (ByteString -> Pos -> More -> IResult ByteString r)
       -> (ByteString -> Pos -> More -> IResult ByteString r)
       -> IResult ByteString r
prompt t pos _more lose succ = Partial $ \s ->
  if B.null s
  then lose t pos Complete
  else succ (t <> s) pos Incomplete

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Parser ()
demandInput = T.Parser $ \t pos more lose succ ->
  case more of
    Complete -> lose t pos more [] "not enough input"
    _ -> let lose' t' pos' more' = lose t' pos' more' [] "not enough input"
             succ' t' pos' more' = succ t' pos' more' ()
         in prompt t pos more lose' succ'

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: Parser Bool
wantInput = T.Parser $ \t pos more _lose succ ->
  case () of
    _ | lengthAtLeast pos 1 t -> succ t pos more True
      | more == Complete -> succ t pos more False
      | otherwise       -> let lose' t' pos' more' = succ t' pos' more' False
                               succ' t' pos' more' = succ t' pos' more' True
                           in prompt t pos more lose' succ'
-- | Match only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = T.Parser $ \t pos more lose succ ->
  case () of
    _| lengthAtLeast pos 1 t -> lose t pos more [] "endOfInput"
     | more == Complete -> succ t pos more ()
     | otherwise ->
       let lose' t' pos' more' _ctx _msg = succ t' pos' more' ()
           succ' t' pos' more' _a = lose t' pos' more' [] "endOfInput"
       in  runParser demandInput t pos more lose' succ'

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
match :: Parser a -> Parser (ByteString, a)
match p = T.Parser $ \t pos more lose succ ->
  let succ' t' pos' more' a =
        succ t' pos' more' (substring pos (pos'-pos) t', a)
  in runParser p t pos more lose succ'

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Parser Bool
atEnd = not <$> wantInput
{-# INLINE atEnd #-}

lengthAtLeast :: Pos -> Int -> ByteString -> Bool
lengthAtLeast (Pos pos) n bs = B.length bs >= pos + n
{-# INLINE lengthAtLeast #-}

substring :: Pos -> Pos -> ByteString -> ByteString
substring (Pos pos) (Pos n) bs = B.unsafeTake n (B.unsafeDrop pos bs)
{-# INLINE substring #-}
