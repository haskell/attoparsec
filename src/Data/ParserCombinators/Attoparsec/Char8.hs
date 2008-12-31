-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Attoparsec.Char8
-- Copyright   :  Daan Leijen 1999-2001, Jeremy Shaw 2006, Bryan O'Sullivan 2007-2008
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for lazy 'LB.ByteString'
-- strings, loosely based on 'Text.ParserCombinators.Parsec'.
-- 
-----------------------------------------------------------------------------
module Data.ParserCombinators.Attoparsec.Char8
    (
    -- * Parser
      ParseError
    , Parser

    -- * Running parsers
    , parse
    , parseAt
    , parseTest

    -- * Combinators
    , (<?>)

    -- * Things vaguely like those in @Parsec.Combinator@ (and @Parsec.Prim@)
    , try
    , many
    , many1
    , manyTill
    , eof
    , skipMany
    , skipMany1
    , count
    , lookAhead
    , peek
    , sepBy
    , sepBy1

    -- * Things like in @Parsec.Char@
    , satisfy
    , letter
    , digit
    , anyChar
    , space
    , char
    , notChar
    , string
    , stringCI

    -- * Parser converters.
    , eitherP

    -- * Numeric parsers.
    , int
    , integer
    , double

    -- * Miscellaneous functions.
    , getInput
    , getConsumed
    , takeWhile
    , takeWhile1
    , takeTill
    , takeAll
    , takeCount
    , skipWhile
    , skipSpace
    , notEmpty
    , match
    , inClass
    , notInClass
    , endOfLine
    ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Internal (w2c)
import Data.Char (isDigit, isLetter, isSpace, toLower)
import Data.ParserCombinators.Attoparsec.FastSet
    (FastSet, memberChar, set)
import qualified Data.ParserCombinators.Attoparsec.Internal as I
import Data.ParserCombinators.Attoparsec.Internal
    (Parser, ParseError, (<?>), parse, parseAt, parseTest, try, manyTill, eof,
     skipMany, skipMany1, count, lookAhead, peek, sepBy, sepBy1, string,
     eitherP, getInput, getConsumed, takeAll, takeCount, notEmpty, match,
     endOfLine, setInput, many, many1)
import Data.ByteString.Lex.Lazy.Double (readDouble)
import Prelude hiding (takeWhile)

-- | Character parser.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = w2c <$> I.satisfy (p . w2c)
{-# INLINE satisfy #-}

letter :: Parser Char
letter = satisfy isLetter <?> "letter"
{-# INLINE letter #-}

digit :: Parser Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

anyChar :: Parser Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

space :: Parser Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Satisfy a specific character.
char :: Char -> Parser Char
char c = satisfy (== c) <?> [c]
{-# INLINE char #-}

-- | Satisfy a specific character.
notChar :: Char -> Parser Char
notChar c = satisfy (/= c) <?> "not " ++ [c]
{-# INLINE notChar #-}

charClass :: String -> FastSet
charClass = set . SB.pack . go
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""

inClass :: String -> Char -> Bool
inClass s = (`memberChar` myset)
    where myset = charClass s
{-# INLINE inClass #-}

notInClass :: String -> Char -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Satisfy a literal string, ignoring case.
stringCI :: LB.ByteString -> Parser LB.ByteString
stringCI = I.stringTransform (LB.map toLower)
{-# INLINE stringCI #-}

-- | Consume characters while the predicate is true.
takeWhile :: (Char -> Bool) -> Parser LB.ByteString
takeWhile p = I.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

takeTill :: (Char -> Bool) -> Parser LB.ByteString
takeTill p = I.takeTill (p . w2c)
{-# INLINE takeTill #-}

takeWhile1 :: (Char -> Bool) -> Parser LB.ByteString
takeWhile1 p = I.takeWhile1 (p . w2c)
{-# INLINE takeWhile1 #-}

-- | Skip over characters while the predicate is true.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = I.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = takeWhile isSpace >> return ()
{-# INLINE skipSpace #-}

numeric :: String -> (LB.ByteString -> Maybe (a,LB.ByteString)) -> Parser a
numeric desc f = do
  s <- getInput
  case f s of
    Nothing -> fail desc
    Just (i,s') -> setInput s' >> return i
                   
-- | Parse an integer.  The position counter is not updated.
int :: Parser Int
int = numeric "Int" LB.readInt

-- | Parse an integer.  The position counter is not updated.
integer :: Parser Integer
integer = numeric "Integer" LB.readInteger

-- | Parse a Double.  The position counter is not updated.
double :: Parser Double
double = numeric "Double" readDouble
