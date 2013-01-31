{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, GADTs, OverloadedStrings,
    Rank2Types, RecordWildCards, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Data.Attoparsec.Text.Internal
-- Copyright   :  Bryan O'Sullivan 2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'T.Text' strings, loosely
-- based on the Parsec library.

module Data.Attoparsec.Text.Internal
    (
    -- * Parser types
      Parser
    , Result

    -- * Running parsers
    , parse
    , parseOnly

    -- * Combinators
    , (<?>)
    , try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual characters
    , satisfy
    , satisfyWith
    , anyChar
    , skip
    , char
    , notChar
    , peekChar

    -- ** Character classes
    , inClass
    , notInClass

    -- * Efficient string handling
    , skipWhile
    , string
    , stringCI
    , asciiCI
    , take
    , scan
    , takeWhile
    , takeWhile1
    , takeTill

    -- ** Consume all remaining input
    , takeText
    , takeLazyText

    -- * State observation and manipulation functions
    , endOfInput
    , atEnd

    -- * Utilities
    , endOfLine
    ) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (when)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Internal.Types hiding (Parser, Input, Added, Failure, Success)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text)
import Prelude hiding (getChar, take, takeWhile)
import Data.Char (chr, ord)
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.Attoparsec.Text.FastSet as Set
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as L

type Parser = T.Parser Text
type Result = IResult Text
type Input = T.Input Text
type Added = T.Added Text
type Failure r = T.Failure Text r
type Success a r = T.Success Text a r

instance (a ~ Text) => IsString (Parser a) where
    fromString = string . T.pack

lengthAtLeast :: T.Text -> Int -> Bool
lengthAtLeast t@(T.Text _ _ len) n = (len `div` 2) >= n || T.length t >= n
{-# INLINE lengthAtLeast #-}

-- | If at least @n@ characters of input are available, return the
-- current input, otherwise fail.
ensure :: Int -> Parser Text
ensure !n = T.Parser $ \i0 a0 m0 kf ks ->
    if lengthAtLeast (unI i0) n
    then ks i0 a0 m0 (unI i0)
    else runParser (demandInput >> go n) i0 a0 m0 kf ks
  where
    go n' = T.Parser $ \i0 a0 m0 kf ks ->
        if lengthAtLeast (unI i0) n'
        then ks i0 a0 m0 (unI i0)
        else runParser (demandInput >> go n') i0 a0 m0 kf ks
{-# INLINE ensure #-}

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: Input -> Added -> More
       -> (Input -> Added -> More -> Result r)
       -> (Input -> Added -> More -> Result r)
       -> Result r
prompt i0 a0 _m0 kf ks = Partial $ \s ->
    if T.null s
    then kf i0 a0 Complete
    else ks (i0 <> I s) (a0 <> A s) Incomplete

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Parser ()
demandInput = T.Parser $ \i0 a0 m0 kf ks ->
    if m0 == Complete
    then kf i0 a0 m0 ["demandInput"] "not enough input"
    else let kf' i a m = kf i a m ["demandInput"] "not enough input"
             ks' i a m = ks i a m ()
         in prompt i0 a0 m0 kf' ks'

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: Parser Bool
wantInput = T.Parser $ \i0 a0 m0 _kf ks ->
  case () of
    _ | not (T.null (unI i0)) -> ks i0 a0 m0 True
      | m0 == Complete  -> ks i0 a0 m0 False
      | otherwise       -> let kf' i a m = ks i a m False
                               ks' i a m = ks i a m True
                           in prompt i0 a0 m0 kf' ks'

get :: Parser Text
get  = T.Parser $ \i0 a0 m0 _kf ks -> ks i0 a0 m0 (unI i0)

put :: Text -> Parser ()
put s = T.Parser $ \_i0 a0 m0 _kf ks -> ks (I s) a0 m0 ()

-- | Attempt a parse, and if it fails, rewind the input so that no
-- input appears to have been consumed.
--
-- This combinator is provided for compatibility with Parsec.
-- Attoparsec parsers always backtrack on failure.
try :: Parser a -> Parser a
try p = p
{-# INLINE try #-}

unsafeHead :: Text -> Char
unsafeHead = T.head

unsafeTail :: Text -> Text
unsafeTail = T.tail

unsafeTake :: Int -> Text -> Text
unsafeTake = T.take

unsafeDrop :: Int -> Text -> Text
unsafeDrop = T.drop

-- | The parser @satisfy p@ succeeds for any character for which the
-- predicate @p@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- ensure 1
  let !w = unsafeHead s
  if p w
    then put (unsafeTail s) >> return w
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @skip p@ succeeds for any character for which the
-- predicate @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
skip :: (Char -> Bool) -> Parser ()
skip p = do
  s <- ensure 1
  if p (unsafeHead s)
    then put (unsafeTail s)
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a character, and succeeds
-- if the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed character that was parsed.
satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  s <- ensure 1
  let c = f $! unsafeHead s
  if p c
    then let !t = unsafeTail s
         in put t >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

-- | Consume @n@ characters of input, but succeed only if the
-- predicate returns 'True'.
takeWith :: Int -> (Text -> Bool) -> Parser Text
takeWith n p = do
  s <- ensure n
  let (h,t) = T.splitAt n s
  if p h
    then put t >> return h
    else fail "takeWith"

-- | Consume exactly @n@ characters of input.
take :: Int -> Parser Text
take n = takeWith n (const True)
{-# INLINE take #-}

-- | @string s@ parses a sequence of characters that identically match
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
string :: Text -> Parser Text
string s = takeWith (T.length s) (==s)
{-# INLINE string #-}

-- | Satisfy a literal string, ignoring case.
--
-- Note: this function is currently quite inefficient. Unicode case
-- folding can change the length of a string (\"&#223;\" becomes
-- "ss"), which makes a simple, efficient implementation tricky.  We
-- have (for now) chosen simplicity over efficiency.
stringCI :: Text -> Parser Text
stringCI s = go 0
  where
    go !n
      | n > T.length fs = fail "stringCI"
      | otherwise = do
      t <- ensure n
      let h = unsafeTake n t
      if T.toCaseFold h == fs
        then put (unsafeDrop n t) >> return h
        else go (n+1)
    fs = T.toCaseFold s
{-# INLINE stringCI #-}
{-# DEPRECATED stringCI "this is very inefficient, use asciiCI instead" #-}

-- | Satisfy a literal string, ignoring case for characters in the ASCII range.
asciiCI :: Text -> Parser Text
asciiCI input = do
  t <- ensure n
  let h = unsafeTake n t
  if asciiToLower h == s
    then put (unsafeDrop n t) >> return h
    else fail "asciiCI"
  where
    n = T.length input
    s = asciiToLower input

    -- convert letters in the ASCII range to lower-case
    asciiToLower = T.map f
      where
        offset = ord 'a' - ord 'A'
        f c | 'A' <= c && c <= 'Z' = chr (ord c + offset)
            | otherwise            = c
{-# INLINE asciiCI #-}

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = go
 where
  go = do
    t <- T.dropWhile p <$> get
    put t
    when (T.null t) $ do
      input <- wantInput
      when input go
{-# INLINE skipWhile #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first character of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeTill :: (Char -> Bool) -> Parser Text
takeTill p = takeWhile (not . p)
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first character of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = (T.concat . reverse) `fmap` go []
 where
  go acc = do
    (h,t) <- T.span p <$> get
    put t
    if T.null t
      then do
        input <- wantInput
        if input
          then go (h:acc)
          else return (h:acc)
      else return (h:acc)

takeRest :: Parser [Text]
takeRest = go []
 where
  go acc = do
    input <- wantInput
    if input
      then do
        s <- get
        put T.empty
        go (s:acc)
      else return (reverse acc)

-- | Consume all remaining input and return it as a single string.
takeText :: Parser Text
takeText = T.concat `fmap` takeRest

-- | Consume all remaining input and return it as a single string.
takeLazyText :: Parser L.Text
takeLazyText = L.fromChunks `fmap` takeRest

data Scan s = Continue s
            | Finished {-# UNPACK #-} !Int T.Text

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each character of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first character of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
scan :: s -> (s -> Char -> Maybe s) -> Parser Text
scan s0 p = do
  chunks <- go [] s0
  case chunks of
    [x] -> return x
    xs  -> return . T.concat . reverse $ xs
 where
  scanner s !n t =
    case T.uncons t of
      Just (c,t') -> case p s c of
                       Just s' -> scanner s' (n+1) t'
                       Nothing -> Finished n t
      Nothing     -> Continue s
  go acc s = do
    input <- get
    case scanner s 0 input of
      Continue s'  -> do put T.empty
                         more <- wantInput
                         if more
                           then go (input : acc) s'
                           else return (input : acc)
      Finished n t -> put t >> return (T.take n input : acc)
{-# INLINE scan #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one
-- character of input: it will fail if the predicate never returns
-- 'True' or if there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = do
  (`when` demandInput) =<< T.null <$> get
  (h,t) <- T.span p <$> get
  when (T.null h) $ fail "takeWhile1"
  put t
  if T.null t
    then (h<>) `fmap` takeWhile p
    else return h

-- | Match any character in a set.
--
-- >vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- >halfAlphabet = inClass "a-nA-N"
--
-- To add a literal @\'-\'@ to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Char -> Bool
inClass s = (`Set.member` mySet)
    where mySet = Set.charClass s
          {-# NOINLINE mySet #-}
{-# INLINE inClass #-}

-- | Match any character not in a set.
notInClass :: String -> Char -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Match any character.
anyChar :: Parser Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

-- | Match a specific character.
char :: Char -> Parser Char
char c = satisfy (== c) <?> show c
{-# INLINE char #-}

-- | Match any character except the given one.
notChar :: Char -> Parser Char
notChar c = satisfy (/= c) <?> "not " ++ show c
{-# INLINE notChar #-}

-- | Match any character. Returns 'Nothing' if end of input has been
-- reached. Does not consume any input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
peekChar :: Parser (Maybe Char)
peekChar = T.Parser $ \i0 a0 m0 _kf ks ->
           if T.null (unI i0)
           then if m0 == Complete
                then ks i0 a0 m0 Nothing
                else let ks' i a m = let !c = unsafeHead (unI i)
                                     in ks i a m (Just c)
                         kf' i a m = ks i a m Nothing
                     in prompt i0 a0 m0 kf' ks'
           else let !c = unsafeHead (unI i0)
                in ks i0 a0 m0 (Just c)
{-# INLINE peekChar #-}

-- | Match only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = T.Parser $ \i0 a0 m0 kf ks ->
             if T.null (unI i0)
             then if m0 == Complete
                  then ks i0 a0 m0 ()
                  else let kf' i1 a1 m1 _ _ = addS i0 a0 m0 i1 a1 m1 $
                                              \ i2 a2 m2 -> ks i2 a2 m2 ()
                           ks' i1 a1 m1 _   = addS i0 a0 m0 i1 a1 m1 $
                                              \ i2 a2 m2 -> kf i2 a2 m2 []
                                                            "endOfInput"
                       in  runParser demandInput i0 a0 m0 kf' ks'
             else kf i0 a0 m0 [] "endOfInput"

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Parser Bool
atEnd = not <$> wantInput
{-# INLINE atEnd #-}

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

-- | Name the parser, in case failure occurs.
(<?>) :: Parser a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser a
p <?> msg0 = T.Parser $ \i0 a0 m0 kf ks ->
             let kf' i a m strs msg = kf i a m (msg0:strs) msg
             in runParser p i0 a0 m0 kf' ks
{-# INLINE (<?>) #-}
infix 0 <?>

-- | Terminal failure continuation.
failK :: Failure a
failK i0 _a0 _m0 stack msg = Fail (unI i0) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: Success a a
successK i0 _a0 _m0 a = Done (unI i0) a
{-# INLINE successK #-}

-- | Run a parser.
parse :: Parser a -> Text -> Result a
parse m s = runParser m (I s) mempty Incomplete failK successK
{-# INLINE parse #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: Parser a -> Text -> Either String a
parseOnly m s = case runParser m (I s) mempty Complete failK successK of
                  Fail _ _ err -> Left err
                  Done _ a     -> Right a
                  _            -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}
