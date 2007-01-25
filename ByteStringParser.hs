-----------------------------------------------------------------------------
-- |
-- Module      :  ByteStringParser
-- Copyright   :  (c) Daan Leijen 1999-2001, Jeremy Shaw 2006, Bryan O'Sullivan 2007
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  jeremy@n-heptane.com
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
import qualified Data.ByteString.Char8 as C

-- * Parser

type ParserError state = (state, String)

-- * Parser Monad

newtype Parser state a = Parser { unParser :: (state -> Either (state,[String]) (a, state)) }

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
pSucceed :: a -> Parser state a
pSucceed = return

-- |Always fail
pFail :: Parser state a
pFail = Parser (\st ->  Left (st, []))

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

-- |character parser
satisfy :: (Char -> Bool) -> Parser C.ByteString Char
satisfy f =
    Parser $ \bs ->
        if C.null bs
        then Left (bs, [])
        else let (s,ss) = (C.head bs, C.tail bs) in
             if (f s)
                then Right (s,ss)
                else Left (bs, [])

-- |satisfy a specific character
pChar :: Char -> Parser C.ByteString Char
pChar c = satisfy (== c) <?> [c]


-- * Things vaguely like those in @Parsec.Combinator@ (and @Parsec.Prim@)

-- |detect 'end of file'
pEOF :: Parser C.ByteString ()
pEOF =
    Parser $ \bs -> if C.null bs then Right ((),bs) else (Left (bs, ["EOF"]))

-- |pTakeWhile take characters while the predicate is true
pTakeWhile :: (Char -> Bool) -> Parser C.ByteString C.ByteString
pTakeWhile f =
    Parser $ \bs -> Right (C.span f bs)

-- |pSkipWhile skip over characters while the predicate is true
pSkipWhile :: (Char -> Bool) -> Parser C.ByteString ()
pSkipWhile p =
    Parser $ \bs -> Right ((), C.dropWhile p bs)

-- |'pMany' - take zero or more instances of the parser
pMany ::  Parser st a -> Parser st [a]
pMany p 
    = scan id
    where
      scan f = do x <- p
                  scan (\tail -> f (x:tail))
               <|> return (f [])

-- |'pMany1' - take one or more instances of the parser
pMany1 :: Parser st a -> Parser st [a]
pMany1 p =
    do x <- p
       xs <- pMany p
       return (x:xs)

-- |'pSkipMany' - skip zero or many instances of the parser
pSkipMany :: Parser st a -> Parser st ()
pSkipMany p = scan
    where
      scan = (p >> scan) <|> return ()

-- |'pSkipMany1' - skip one or many instances of the parser       
pSkipMany1 :: Parser st a -> Parser st ()
pSkipMany1 p = p >> pSkipMany p

-- |'notEmpty' - tests that a parser returned a non-null ByteString
notEmpty :: Parser C.ByteString C.ByteString -> Parser C.ByteString C.ByteString 
notEmpty (Parser p) =
    Parser $ \s -> case p s of
                     o@(Right (a, s)) ->
                         if C.null a
                         then Left (a, ["notEmpty"])
                         else o
                     x -> x

-- | parse some input with the given parser and return that input without copying it
pMatch :: Parser C.ByteString a -> Parser C.ByteString C.ByteString
pMatch p = do start <- getInput
              p
              end <- getInput
              return (C.take (C.length start - C.length end) start)


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

parseTest p s =
    case parse p s of
      Left (st, msg) -> putStrLn $ msg ++ "\nGot:\n" ++ show st
      Right (r,_) -> print r
