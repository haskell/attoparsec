{-# LANGUAGE BangPatterns, FlexibleInstances, GADTs, OverloadedStrings,
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
    , module Data.Attoparsec.Combinator

    -- * Parsing individual characters
    , satisfy
    , satisfyWith
    , anyChar
    , skip
    , char
    , notChar

    -- ** Lookahead
    , peekChar
    , peekChar'

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
    , runScanner
    , takeWhile
    , takeWhile1
    , takeTill

    -- ** Consume all remaining input
    , takeText
    , takeLazyText

    -- * Utilities
    , endOfLine
    ) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (when)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Internal.Types hiding (Parser, Failure, Success)
import Data.Attoparsec.Internal
import Data.String (IsString(..))
import Data.Text (Text)
import Prelude hiding (getChar, succ, take, takeWhile)
import Data.Char (chr, ord)
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.Attoparsec.Text.FastSet as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

type Parser = T.Parser Text
type Result = IResult Text
type Failure r = T.Failure Text r
type Success a r = T.Success Text a r

instance (a ~ Text) => IsString (Parser a) where
    fromString = string . T.pack

-- | The parser @satisfy p@ succeeds for any character for which the
-- predicate @p@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
satisfy :: (Char -> Bool) -> Parser Char
satisfy = satisfyElem
{-# INLINE satisfy #-}

-- | The parser @skip p@ succeeds for any character for which the
-- predicate @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
skip :: (Char -> Bool) -> Parser ()
skip p = do
  s <- ensure 1
  if p (T.head s)
    then advance 1
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a character, and succeeds
-- if the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed character that was parsed.
satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  s <- ensure 1
  let c = f $! T.head s
  if p c
    then advance 1 >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

-- | Consume @n@ characters of input, but succeed only if the
-- predicate returns 'True'.
takeWith :: Int -> (Text -> Bool) -> Parser Text
takeWith n p = do
  s <- ensure n
  let h = T.take n s
  if p h
    then advance n >> return h
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
      if T.toCaseFold t == fs
        then advance n >> return t
        else go (n+1)
    fs = T.toCaseFold s
{-# INLINE stringCI #-}
{-# DEPRECATED stringCI "this is very inefficient, use asciiCI instead" #-}

-- | Satisfy a literal string, ignoring case for characters in the ASCII range.
asciiCI :: Text -> Parser Text
asciiCI input = do
  t <- ensure n
  let h = T.take n t
  if asciiToLower h == s
    then advance n >> return h
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
    t <- T.takeWhile p <$> get
    advance (T.length t)
    eoc <- endOfChunk
    when eoc $ do
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
    h <- T.takeWhile p <$> get
    advance (T.length h)
    eoc <- endOfChunk
    if eoc
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
        advance (T.length s)
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

scan_ :: (s -> [Text] -> Parser r) -> s -> (s -> Char -> Maybe s) -> Parser r
scan_ f s0 p = go [] s0
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
      Continue s'  -> do advance (T.length input)
                         more <- wantInput
                         if more
                           then go (input : acc) s'
                           else f s' (input : acc)
      Finished n t -> do advance (T.length input - T.length t)
                         f s (T.take n input : acc)
{-# INLINE scan_ #-}

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
scan = scan_ $ \_ chunks ->
  case chunks of
    [x] -> return x
    xs  -> return . T.concat . reverse $ xs
{-# INLINE scan #-}

-- | Like 'scan', but generalized to return the final state of the
-- scanner.
runScanner :: s -> (s -> Char -> Maybe s) -> Parser (Text, s)
runScanner = scan_ $ \s xs -> return (T.concat (reverse xs), s)
{-# INLINE runScanner #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one
-- character of input: it will fail if the predicate never returns
-- 'True' or if there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = do
  (`when` demandInput) =<< endOfChunk
  h <- T.takeWhile p <$> get
  let len = T.length h
  when (len == 0) $ fail "takeWhile1"
  advance len
  eoc <- endOfChunk
  if eoc
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

-- | Match any character, to perform lookahead. Returns 'Nothing' if
-- end of input has been reached. Does not consume any input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
peekChar :: Parser (Maybe Char)
peekChar = T.Parser $ \t pos more _lose succ ->
  case () of
    _| chunkLengthAtLeast pos 1 t ->
       let !c = T.head (T.drop pos t)
       in succ t pos more (Just c)
     | more == Complete ->
       succ t pos more Nothing
     | otherwise ->
       let succ' t' pos' more' = let !c = T.head (T.drop pos t')
                                 in succ t' pos' more' (Just c)
           lose' t' pos' more' = succ t' pos' more' Nothing
       in prompt t pos more lose' succ'
{-# INLINE peekChar #-}

-- | Match any character, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
peekChar' :: Parser Char
peekChar' = do
  s <- ensure 1
  return $! T.head s
{-# INLINE peekChar' #-}

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

-- | Terminal failure continuation.
failK :: Failure a
failK t pos _more stack msg = Fail (T.drop pos t) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: Success a a
successK t pos _more a = Done (T.drop pos t) a
{-# INLINE successK #-}

-- | Run a parser.
parse :: Parser a -> Text -> Result a
parse m s = runParser m s 0 Incomplete failK successK
{-# INLINE parse #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: Parser a -> Text -> Either String a
parseOnly m s = case runParser m s 0 Complete failK successK of
                  Fail _ _ err -> Left err
                  Done _ a     -> Right a
                  _            -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}
