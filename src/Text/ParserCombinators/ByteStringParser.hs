{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ByteStringParser
-- Copyright   :  (c) Daan Leijen 1999-2001, Jeremy Shaw 2006, Bryan O'Sullivan 2007
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Primitive parser combinators for ByteStrings loosely based on Parsec.
-- 
-----------------------------------------------------------------------------
module Text.ParserCombinators.ByteStringParser
    (
    -- * Parser
      ParseError
    , Parser

    -- * Running parsers
    , parse
    , parseTest

    -- * Combinators
    , succeed
    , (<|>)
    , (<?>)

    -- * Things vaguely like those in @Parsec.Combinator@ (and @Parsec.Prim@)
    , try
    , many
    , many1
    , manyTill
    , eof
    , skipMany
    , skipMany1

    -- * Things like in @Parsec.Char@
    , satisfy
    , letter
    , digit
    , anyChar
    , space
    , char
    , string

    -- * Miscellaneous functions.
    , getInput
    , getConsumed
    , takeWhile
    , skipWhile
    ) where

#ifdef APPLICATIVE_IN_BASE
import Control.Applicative (Applicative(..))
#endif

import Control.Monad (MonadPlus(..), ap)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isDigit, isLetter, isSpace)
import Data.Int (Int64)
import Prelude hiding (takeWhile)

type ParseError = (C.ByteString, String)

-- * Parser Monad

data S = S C.ByteString
           {-# UNPACK #-} !Int64

newtype Parser a = Parser {
      unParser :: S -> Either (C.ByteString, [String]) (a, S)
    }

instance Functor Parser where
    fmap f p =
        Parser $ \s ->
            case unParser p s of
              Right (a, s') -> Right (f a, s')
              Left err -> Left err

instance Monad Parser where
    return a = Parser $ \s -> Right (a, s)
    m >>= f = Parser $ \s ->
              case unParser m s of
                Right (a, s') -> unParser (f a) s'
                Left (s', msgs) -> Left (s', msgs)
    fail err = Parser $ \(S bs _) -> Left (bs, [err])

instance MonadPlus Parser where
    mzero = Parser $ \(S bs _) -> Left (bs, [])
    Parser p1 `mplus` Parser p2 =
        Parser $ \s@(S bs _) -> case p1 s of
                         Left (_, msgs1) -> 
                            case p2 s of
                              Left (_, msgs2) -> Left (bs, (msgs1 ++ msgs2))
                              ok -> ok
                         ok -> ok

#ifdef APPLICATIVE_IN_BASE
instance Applicative Parser where
    pure = return
    (<*>) = ap
#endif

-- | Always succeed.
succeed :: a -> Parser a
succeed = return

infix 0 <?>
infixr 1 <|>

-- | Choice.
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

-- | Name the parser.
(<?>) :: Parser a -> String -> Parser a
p <?> msg =
    Parser $ \s@(S bs _) ->
        case unParser p s of
          (Left _) -> Left (bs, [msg])
          ok -> ok

-- | Get remaining input.
getInput :: Parser C.ByteString
getInput = Parser $ \s@(S bs _) -> Right (bs, s)

-- | Get remaining input.
getConsumed :: Parser Int64
getConsumed = Parser $ \s@(S _ n) -> Right (n, s)


-- | Character parser.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
    Parser $ \(S bs n) ->
        if C.null bs
        then Left (bs, [])
        else let Just (s, bs') = C.uncons bs in
             if f s
                then Right (s, S bs' (n + 1))
                else Left (bs, [])
{-# INLINE satisfy #-}


letter :: Parser Char
letter = satisfy isLetter
{-# INLINE letter #-}

digit :: Parser Char
digit = satisfy isDigit
{-# INLINE digit #-}

anyChar :: Parser Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

space :: Parser Char
space = satisfy isSpace
{-# INLINE space #-}

-- | Satisfy a specific character.

char :: Char -> Parser Char
char c = satisfy (== c) <?> [c]
{-# INLINE char #-}

string :: String -> Parser String
string s = mapM char s <?> show s
{-# INLINE string #-}

count :: Int -> Parser a -> Parser [a]
count n p = sequence (replicate n p)
{-# INLINE count #-}

try :: Parser a -> Parser a
try p = Parser $ \s@(S bs _) ->
        case unParser p s of
          Left (_, msgs) -> Left (bs, msgs)
          ok -> ok

-- | Detect 'end of file'.
eof :: Parser ()
eof = Parser $ \s@(S bs _) -> if C.null bs
                              then Right ((), s)
                              else Left (bs, ["EOF"])

-- | Consume characters while the predicate is true.
takeWhile :: (Char -> Bool) -> Parser C.ByteString
takeWhile f = Parser $ \(S bs n) ->
              let (h, bs') = C.span f bs
              in Right (h, S bs' (n + C.length h))

-- | Skip over characters while the predicate is true.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = takeWhile p >> return ()

-- | Take zero or more instances of the parser.
many ::  Parser a -> Parser [a]
many p = scan id
    where scan f = do x <- p
                      scan (f . (x:))
                 <|> return (f [])

-- | Take one or more instances of the parser.
many1 :: Parser a -> Parser [a]
many1 p =
    do x <- p
       xs <- many p
       return (x:xs)

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = scan
    where scan = do end; return []
               <|>
                 do x <- p
                    xs <- scan
                    return (x:xs)

-- |'skipMany' - skip zero or many instances of the parser
skipMany :: Parser a -> Parser ()
skipMany p = scan
    where
      scan = (p >> scan) <|> return ()

-- |'skipMany1' - skip one or many instances of the parser       
skipMany1 :: Parser  a -> Parser ()
skipMany1 p = p >> skipMany p

-- | Test that a parser returned a non-null ByteString.
notEmpty :: Parser C.ByteString -> Parser C.ByteString 
notEmpty p = Parser $ \s ->
             case unParser p s of
               o@(Right (a, _)) ->
                   if C.null a
                   then Left (a, ["notEmpty"])
                   else o
               x -> x

-- | Parse some input with the given parser and return that input
-- without copying it.
match :: Parser a -> Parser C.ByteString
match p = do bs <- getInput
             start <- getConsumed
             p
             end <- getConsumed
             return (C.take (end - start) bs)

lookAhead :: Parser a -> Parser a

lookAhead p = Parser $ \s@(S bs _) ->
              case unParser p s of
                Left (_, msgs) -> Left (bs, msgs)
                Right (m, _) -> Right (m, s)

-- | Run a parser.
parse :: Parser a -> C.ByteString
      -> Either ParseError (a, C.ByteString)
parse p bs = 
    case unParser p (S bs 0) of
      Left (bs', msg) -> Left (bs', showError msg)
      Right (a, S bs' _) -> Right (a, bs')
    where
      showError [msg] = "Parser error, expected:\n" ++ msg ++ "\n"
      showError msgs = "Parser error, expected one of:\n" ++ unlines msgs

parseTest :: (Show a) => Parser a -> C.ByteString -> IO ()
parseTest p s =
    case parse p s of
      Left (st, msg) -> putStrLn $ msg ++ "\nGot:\n" ++ show st
      Right (r,_) -> print r
