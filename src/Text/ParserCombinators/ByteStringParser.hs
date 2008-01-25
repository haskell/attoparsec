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
-- Simple, efficient parser combinators for lazy 'C.ByteString'
-- values, loosely based on 'Text.ParserCombinators.Parsec'.
-- 
-----------------------------------------------------------------------------
module Text.ParserCombinators.ByteStringParser
    (
    -- * Parser
      ParseError
    , Parser

    -- * Running parsers
    , parse
    , parseAt
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
    , count
    , lookAhead
    , sepBy
    , sepBy1

    -- * Things like in @Parsec.Char@
    , satisfy
    , letter
    , digit
    , anyChar
    , space
    , char
    , string
    , stringCI
    , byteString
    , byteStringCI

    -- * Miscellaneous functions.
    , getInput
    , getConsumed
    , takeWhile
    , takeWhile1
    , takeAll
    , skipWhile
    , skipSpace
    , notEmpty
    , match
    , inClass
    , notInClass
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap, liftM2)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (isDigit, isLetter, isSpace, toLower)
import Data.Int (Int64)
import qualified Data.Set as S
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
{-# INLINE (<|>) #-}

-- | Name the parser.
(<?>) :: Parser a -> String -> Parser a
p <?> msg =
    Parser $ \s@(S bs _) ->
        case unParser p s of
          (Left _) -> Left (bs, [msg])
          ok -> ok
{-# INLINE (<?>) #-}

-- | Get remaining input.
getInput :: Parser C.ByteString
getInput = Parser $ \s@(S bs _) -> Right (bs, s)

-- | Get number of bytes consumed so far.
getConsumed :: Parser Int64
getConsumed = Parser $ \s@(S _ n) -> Right (n, s)

-- | Character parser.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
    Parser $ \(S bs n) ->
           case C.uncons bs of
             Just (s, bs') | f s -> Right (s, S bs' (n + 1))
             _                   -> Left (bs, [])
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

charClass :: String -> S.Set Char
charClass s = S.fromList (go s)
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""

inClass :: String -> Char -> Bool
inClass s = (`S.member` set)
    where set = charClass s

notInClass :: String -> Char -> Bool
notInClass s = (`S.notMember` set)
    where set = charClass s

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = liftM2 (:) p ((s >> sepBy1 p s) <|> return []) <|> return []

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = liftM2 (:) p ((s >> sepBy1 p s) <|> return [])

-- | Satisfy a literal string.
byteString :: C.ByteString -> Parser C.ByteString
byteString s = Parser $ \(S bs n) ->
               let l = C.length s
                   (h, t) = C.splitAt l bs
               in if s == h
                  then Right (s, S t (n + l))
                  else Left (bs, [])
{-# INLINE byteString #-}

-- | Satisfy a literal string.
byteStringCI :: C.ByteString -> Parser C.ByteString
byteStringCI s = Parser $ \(S bs n) ->
               let l = C.length s
                   (h, t) = C.splitAt l bs
               in if ls == C.map toLower h
                  then Right (s, S t (n + l))
                  else Left (bs, [])
    where ls = C.map toLower s
{-# INLINE byteStringCI #-}

string :: String -> Parser String
string s = byteString (C.pack s) >> return s
{-# INLINE string #-}

stringCI :: String -> Parser String
stringCI s = byteStringCI (C.pack s) >> return s
{-# INLINE stringCI #-}

-- | Apply the given parser repeatedly, returning every parse result.
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

takeAll :: Parser C.ByteString
takeAll = Parser $ \(S bs n) -> Right (bs, S C.empty (n + C.length bs))

-- | Consume characters while the predicate is true.
takeWhile :: (Char -> Bool) -> Parser C.ByteString
takeWhile f = Parser $ \(S bs n) ->
              let (h, bs') = C.span f bs
              in Right (h, S bs' (n + C.length h))
{-# INLINE takeWhile #-}

takeWhile1 :: (Char -> Bool) -> Parser C.ByteString
takeWhile1 f = Parser $ \(S bs n) ->
              let (h, bs') = C.span f bs
              in if C.null h
                 then Left (bs, [])
                 else Right (h, S bs' (n + C.length h))
{-# INLINE takeWhile1 #-}

-- | Skip over characters while the predicate is true.
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile p = takeWhile p >> return ()
{-# INLINE skipWhile #-}

-- | Skip over white space.
skipSpace :: Parser ()
skipSpace = takeWhile isSpace >> return ()
{-# INLINE skipSpace #-}

-- | Take zero or more instances of the parser.
many ::  Parser a -> Parser [a]
many p = scan id
    where scan f = do x <- p
                      scan (f . (x:))
                 <|> return (f [])

-- | Take one or more instances of the parser.
many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = scan
    where scan = (end >> return []) <|> liftM2 (:) p scan

-- |'skipMany' - skip zero or many instances of the parser
skipMany :: Parser a -> Parser ()
skipMany p = scan
    where scan = (p >> scan) <|> return ()

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

lookAhead :: Parser a -> Parser (Maybe a)
lookAhead p = Parser $ \s ->
              case unParser p s of
                Right (m, _) -> Right (Just m, s)
                _ -> Right (Nothing, s)

parseAt :: Parser a -> C.ByteString -> Int64
        -> Either ParseError (a, C.ByteString)
parseAt p bs n = 
    case unParser p (S bs n) of
      Left (bs', msg) -> Left (bs', showError msg)
      Right (a, S bs' _) -> Right (a, bs')
    where
      showError [msg] = "Parser error, expected:\n" ++ msg ++ "\n"
      showError msgs = "Parser error, expected one of:\n" ++ unlines msgs

-- | Run a parser.
parse :: Parser a -> C.ByteString
      -> Either ParseError (a, C.ByteString)
parse p bs = parseAt p bs 0

parseTest :: (Show a) => Parser a -> C.ByteString -> IO ()
parseTest p s =
    case parse p s of
      Left (st, msg) -> putStrLn $ msg ++ "\nGot:\n" ++ show st
      Right (r,_) -> print r
