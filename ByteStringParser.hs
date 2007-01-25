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
module Text.ParserCombinators.ByteStringParser where

import Data.Char
import Data.Word
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C

-- * Parser

type ParserError state = (state, String)

-- * Parser Monad

newtype Parser state a = Parser { unParser :: (state -> Either (state,[String]) (a, state)) }

type CharParser = Parser C.ByteString Char

instance Functor (Parser state) where
    fmap f (Parser p) =
        Parser $ \st ->
            case p st of
              Right (a, st') -> Right (f a, st')
              Left err -> Left err

instance Monad (Parser state) where
    return a = Parser (\s -> Right (a,s))
    m >>= f =
        Parser $ \state ->
            let r = (unParser m) state in
            case r of
              Right (a,state') -> unParser (f a) $ state'
              (Left (st, msgs)) -> (Left (st, msgs))

instance MonadPlus (Parser state) where
    mzero = Parser (\st -> (Left (st, [])))
    mplus (Parser p1) (Parser p2) =
        Parser (\s -> case p1 s of
                        (Left (_, msgs1)) -> 
                            case p2 s of
                              Left (_, msgs2) -> Left (s, (msgs1 ++ msgs2))
                              o -> o
                        o -> o
               )

-- |Always succeed
succeed :: a -> Parser state a
succeed = return

-- |Always fail
fail :: Parser state a
fail = Parser (\st ->  Left (st, []))

infix 0 <?>
infixr 1 <|>

-- |choice
(<|>) :: Parser state a -> Parser state a -> Parser state a
(<|>) = mplus

-- |name the parser
(<?>) :: Parser state a -> String -> Parser state a
p <?> msg =
    Parser $ \st ->
        case (unParser p) st of
          (Left _) -> Left (st, [msg])
          ok -> ok

-- |get remaining input
getInput :: Parser C.ByteString C.ByteString
getInput = Parser (\st -> Right (st,st))


-- * Things like in @Parsec.Char@

{-# INLINE satisfy #-}

-- |character parser
satisfy :: (Char -> Bool) -> CharParser

satisfy f =
    Parser $ \bs ->
        if C.null bs
        then Left (bs, [])
        else let (s,ss) = (C.head bs, C.tail bs) in
             if (f s)
                then Right (s,ss)
                else Left (bs, [])

letter :: CharParser

letter = satisfy isLetter

digit :: CharParser

digit = satisfy isDigit

anyChar :: CharParser

anyChar = satisfy $ const True

space :: CharParser

space = satisfy isSpace

-- |satisfy a specific character

char :: Char -> CharParser

char c = satisfy (== c) <?> [c]

string :: String -> Parser C.ByteString String

string = mapM char

count :: Int -> Parser st a -> Parser st [a]

count n p = sequence (replicate n p)

-- * Things vaguely like those in @Parsec.Combinator@ (and @Parsec.Prim@)

try :: Parser st a -> Parser st a

try (Parser p)
    = Parser $ \state -> case p state of
                          (Left (_, msgs)) -> Left (state, msgs)
                          ok -> ok

-- |detect 'end of file'
eOF :: Parser C.ByteString ()
eOF =
    Parser $ \bs -> if C.null bs then Right ((),bs) else (Left (bs, ["EOF"]))

-- |takeWhile take characters while the predicate is true
takeWhile :: (Char -> Bool) -> Parser C.ByteString C.ByteString
takeWhile f =
    Parser $ \bs -> Right (C.span f bs)

-- |skipWhile skip over characters while the predicate is true
skipWhile :: (Char -> Bool) -> Parser C.ByteString ()
skipWhile p =
    Parser $ \bs -> Right ((), C.dropWhile p bs)

-- |'many' - take zero or more instances of the parser
many ::  Parser st a -> Parser st [a]
many p = scan id
    where scan f = do x <- p
                      scan (\xs -> f (x:xs))
                 <|> return (f [])

-- |'many1' - take one or more instances of the parser
many1 :: Parser st a -> Parser st [a]
many1 p =
    do x <- p
       xs <- many p
       return (x:xs)

manyTill :: Parser st a -> Parser st end -> Parser st [a]
manyTill p end = scan
    where scan = do end
                    return []
               <|>
                 do x <- p
                    xs <- scan
                    return (x:xs)

-- |'skipMany' - skip zero or many instances of the parser
skipMany :: Parser st a -> Parser st ()
skipMany p = scan
    where
      scan = (p >> scan) <|> return ()

-- |'skipMany1' - skip one or many instances of the parser       
skipMany1 :: Parser st a -> Parser st ()
skipMany1 p = p >> skipMany p

-- |'notEmpty' - tests that a parser returned a non-null ByteString
notEmpty :: Parser C.ByteString C.ByteString -> Parser C.ByteString C.ByteString 
notEmpty (Parser p) =
    Parser $ \s -> case p s of
                     o@(Right (a, s_)) ->
                         if C.null a
                         then Left (a, ["notEmpty"])
                         else o
                     x -> x

-- | parse some input with the given parser and return that input without copying it
match :: Parser C.ByteString a -> Parser C.ByteString C.ByteString
match p = do start <- getInput
             p
             end <- getInput
             return (C.take (C.length start - C.length end) start)

lookAhead :: Parser C.ByteString a -> Parser C.ByteString a

lookAhead (Parser p)
    = Parser $ \state -> case p state of
                          Left (_, msgs) -> Left (state, msgs)
                          Right (m, _) -> Right (m, state)

-- * Running parsers

-- |'parse' - run a parser
parse :: Parser state a -> state -> Either (ParserError state) (a, state)
parse p s = 
    case ((unParser p) s) of
      Left (st, msg) -> Left (st, showError msg)
      (Right r) -> (Right r)
    where
      showError [msg] = "Parser error, expected:\n" ++ msg ++ "\n"
      showError msgs = "Parser error, expected one of:\n" ++ unlines msgs

parseTest :: Parser st a -> st -> IO ()

parseTest p s =
    case parse p s of
      Left (st, msg) -> putStrLn $ msg ++ "\nGot:\n" ++ show st
      Right (r,_) -> print r
