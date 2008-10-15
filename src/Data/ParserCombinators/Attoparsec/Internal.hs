-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Attoparsec.Internal
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
module Data.ParserCombinators.Attoparsec.Internal
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
    , anyWord8
    , word8
    , notWord8
    , string
    , stringTransform

    -- * Parser converters.
    , eitherP

    -- * Miscellaneous functions.
    , getInput
    , getConsumed
    , setInput
    , takeWhile
    , takeWhile1
    , takeTill
    , takeAll
    , skipWhile
    , notEmpty
    , match
    , endOfLine
    ) where

import Control.Applicative (Alternative(..), Applicative(..), (<$>))
import Control.Monad (MonadPlus(..), ap, liftM2)
import Control.Monad.Fix (MonadFix(..))
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Unsafe as U
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Lazy.Internal as LB
import Data.Int (Int64)
import Data.Word (Word8)
import Prelude hiding (takeWhile)

type ParseError = String

-- State invariants:
-- * If the strict bytestring is empty, the entire input is considered
--   to be empty.
-- * Otherwise, the strict bytestring must not be empty.
data S = S {-# UNPACK #-} !SB.ByteString
           LB.ByteString
           {-# UNPACK #-} !Int64

newtype Parser a = Parser {
      unParser :: S -> Either (LB.ByteString, [String]) (a, S)
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
    fail err = Parser $ \(S sb lb _) -> Left (sb +: lb, [err])

instance MonadFix Parser where
    mfix f = Parser $ \s ->
             let r = case r of
                       Right (a, _) -> unParser (f a) s
                       err -> err
             in r

zero :: Parser a
zero = Parser $ \(S sb lb _) -> Left (sb +: lb, [])
{-# INLINE zero #-}

plus :: Parser a -> Parser a -> Parser a
plus p1 p2 =
    Parser $ \s@(S sb lb _) ->
        case unParser p1 s of
          Left (_, msgs1) -> 
              case unParser p2 s of
                Left (_, msgs2) -> Left (sb +: lb, (msgs1 ++ msgs2))
                ok -> ok
          ok -> ok
{-# INLINE plus #-}

instance MonadPlus Parser where
    mzero = zero
    mplus = plus

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Alternative Parser where
    empty = zero
    (<|>) = plus

mkState :: LB.ByteString -> Int64 -> S
mkState s = case s of
              LB.Empty -> S SB.empty s
              LB.Chunk x xs -> S x xs

-- | Turn our chunked representation back into a normal lazy
-- ByteString.
(+:) :: SB.ByteString -> LB.ByteString -> LB.ByteString
sb +: lb | SB.null sb = lb
         | otherwise = LB.Chunk sb lb
{-# INLINE (+:) #-}

infix 0 <?>

-- | Name the parser.
(<?>) :: Parser a -> String -> Parser a
p <?> msg =
    Parser $ \s@(S sb lb _) ->
        case unParser p s of
          (Left _) -> Left (sb +: lb, [msg])
          ok -> ok
{-# INLINE (<?>) #-}

nextChunk :: Parser ()
nextChunk = Parser $ \(S _ lb n) ->
            case lb of
              LB.Chunk sb' lb' -> Right ((), S sb' lb' n)
              LB.Empty -> Left (lb, [])

-- | Get remaining input.
getInput :: Parser LB.ByteString
getInput = Parser $ \s@(S sb lb _) -> Right (sb +: lb, s)

-- | Set the remaining input.
setInput :: LB.ByteString -> Parser ()
setInput bs = Parser $ \(S _ _ n) -> Right ((), mkState bs n)

-- | Get number of bytes consumed so far.
getConsumed :: Parser Int64
getConsumed = Parser $ \s@(S _ _ n) -> Right (n, s)

-- | Character parser.
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p =
    Parser $ \s@(S sb lb n) ->
           case SB.uncons sb of
             Just (c, sb') | p c -> Right (c, S sb' lb (n + 1))
                           | otherwise -> Left (sb +: lb, [])
             Nothing -> unParser (nextChunk >> satisfy p) s
{-# INLINE satisfy #-}

anyWord8 :: Parser Word8
anyWord8 = satisfy $ const True
{-# INLINE anyWord8 #-}

-- | Satisfy a specific character.
word8 :: Word8 -> Parser Word8
word8 c = satisfy (== c) <?> show c
{-# INLINE word8 #-}

-- | Satisfy a specific character.
notWord8 :: Word8 -> Parser Word8
notWord8 c = satisfy (/= c) <?> "not " ++ show c
{-# INLINE notWord8 #-}

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = liftM2 (:) p ((s >> sepBy1 p s) <|> return []) <|> return []

sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy1 p s = liftM2 (:) p ((s >> sepBy1 p s) <|> return [])

-- | Satisfy a literal string.
string :: LB.ByteString -> Parser LB.ByteString
string s = Parser $ \(S sb lb n) ->
           let bs = sb +: lb
               l = LB.length s
               (h,t) = LB.splitAt l bs
           in if s == h
              then Right (s, mkState t (n + l))
              else Left (bs, [])
{-# INLINE string #-}

endOfLine :: Parser ()
endOfLine = Parser $ \(S sb lb n) ->
            let bs = sb +: lb
            in case I.w2c (U.unsafeHead sb) of
                 '\n' -> Right ((), mkState (LB.tail bs) (n + 1))
                 '\r' -> let (h,t) = LB.splitAt 2 bs
                             rn = L8.pack "\r\n"
                         in if h == rn
                            then Right ((), mkState t (n + 2))
                            else Right ((), mkState (LB.tail bs) (n + 1))
                 _ -> Left (bs, ["EOL"])

-- | Satisfy a literal string, after applying a transformation to both
-- it and the matching text.
stringTransform :: (LB.ByteString -> LB.ByteString) -> LB.ByteString
                -> Parser LB.ByteString
stringTransform f s = Parser $ \(S sb lb n) ->
             let bs = sb +: lb
                 l = LB.length s
                 (h, t) = LB.splitAt l bs
             in if fs == f h
                then Right (s, mkState t (n + l))
                else Left (bs, [])
    where fs = f s
{-# INLINE stringTransform #-}

-- | Apply the given parser repeatedly, returning every parse result.
count :: Int -> Parser a -> Parser [a]
count n p = sequence (replicate n p)
{-# INLINE count #-}

try :: Parser a -> Parser a
try p = Parser $ \s@(S sb lb _) ->
        case unParser p s of
          Left (_, msgs) -> Left (sb +: lb, msgs)
          ok -> ok

-- | Detect 'end of file'.
eof :: Parser ()
eof = Parser $ \s@(S sb lb _) -> if SB.null sb && LB.null lb
                                 then Right ((), s)
                                 else Left (sb +: lb, ["EOF"])

takeAll :: Parser LB.ByteString
takeAll = Parser $ \(S sb lb n) ->
          let bs = sb +: lb
          in Right (bs, mkState LB.empty (n + LB.length bs))

-- | Consume characters while the predicate is true.
takeWhile :: (Word8 -> Bool) -> Parser LB.ByteString
takeWhile p =
    Parser $ \(S sb lb n) ->
    case LB.span p (sb +: lb) of
      (h,t) -> Right (h, mkState t (n + LB.length h))
{-# INLINE takeWhile #-}

takeTill :: (Word8 -> Bool) -> Parser LB.ByteString
takeTill p =
  Parser $ \(S sb lb n) ->
  case LB.break p (sb +: lb) of
    (h,t) | LB.null t -> Left (h, [])
          | otherwise -> Right (h, mkState t (n + LB.length h))
{-# INLINE takeTill #-}

takeWhile1 :: (Word8 -> Bool) -> Parser LB.ByteString
takeWhile1 p =
    Parser $ \(S sb lb n) ->
    case LB.span p (sb +: lb) of
      (h,t) | LB.null h -> Left (t, [])
            | otherwise -> Right (h, mkState t (n + LB.length h))
{-# INLINE takeWhile1 #-}

-- | Skip over characters while the predicate is true.
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = takeWhile p >> return ()
{-# INLINE skipWhile #-}

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = scan
    where scan = (end >> return []) <|> liftM2 (:) p scan

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) <|> return []

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- |'skipMany' - skip zero or many instances of the parser
skipMany :: Parser a -> Parser ()
skipMany p = scan
    where scan = (p >> scan) <|> return ()

-- |'skipMany1' - skip one or many instances of the parser       
skipMany1 :: Parser  a -> Parser ()
skipMany1 p = p >> skipMany p

-- | Test that a parser returned a non-null ByteString.
notEmpty :: Parser LB.ByteString -> Parser LB.ByteString 
notEmpty p = Parser $ \s ->
             case unParser p s of
               o@(Right (a, _)) ->
                   if LB.null a
                   then Left (a, ["notEmpty"])
                   else o
               x -> x

-- | Parse some input with the given parser and return that input
-- without copying it.
match :: Parser a -> Parser LB.ByteString
match p = do bs <- getInput
             start <- getConsumed
             p
             end <- getConsumed
             return (LB.take (end - start) bs)

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINE eitherP #-}

peek :: Parser a -> Parser (Maybe a)
peek p = Parser $ \s ->
         case unParser p s of
           Right (m, _) -> Right (Just m, s)
           _ -> Right (Nothing, s)

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \s ->
         case unParser p s of
           Right (m, _) -> Right (m, s)
           Left (e, bs) -> Left (e, bs)

parseAt :: Parser a -> LB.ByteString -> Int64
        -> (LB.ByteString, Either ParseError (a, Int64))
parseAt p bs n = 
    case unParser p (mkState bs n) of
      Left (bs', msg) -> (bs', Left $ showError msg)
      Right (a, S sb lb n') -> (sb +: lb, Right (a, n'))
    where
      showError [""] = "Parser error\n"
      showError [msg] = "Parser error, expected:\n" ++ msg ++ "\n"
      showError [] = "Parser error\n"
      showError msgs = "Parser error, expected one of:\n" ++ unlines msgs

-- | Run a parser.
parse :: Parser a -> LB.ByteString
      -> (LB.ByteString, Either ParseError a)
parse p bs = case parseAt p bs 0 of
               (bs', Right (a, _)) -> (bs', Right a)
               (bs', Left err) -> (bs', Left err)

parseTest :: (Show a) => Parser a -> LB.ByteString -> IO ()
parseTest p s =
    case parse p s of
      (st, Left msg) -> putStrLn $ msg ++ "\nGot:\n" ++ show st
      (_, Right r) -> print r
