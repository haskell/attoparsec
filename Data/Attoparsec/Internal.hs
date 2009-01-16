{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Internal
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
module Data.Attoparsec.Internal
    (
    -- * Parser types
      ParseError
    , Parser

    -- * Running parsers
    , parse
    , parseAt
    , parseTest

    -- * Combinators
    , (<?>)
    , try

    -- * Parsing individual bytes
    , satisfy
    , anyWord8
    , word8
    , notWord8

    -- * Efficient string handling
    , match
    , notEmpty
    , skipWhile
    , string
    , stringTransform
    , takeAll
    , takeCount
    , takeTill
    , takeWhile
    , takeWhile1

    -- * State observation functions
    , endOfInput
    , getConsumed
    , getInput
    , lookAhead
    , setInput

    -- * Utilities
    , endOfLine
    , (+:)
    ) where

import Control.Applicative
    (Alternative(..), Applicative(..), (*>))
import Control.Monad (MonadPlus(..), ap)
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

-- ^ A description of a parsing error.
type ParseError = String

-- State invariants:
-- * If both strict and lazy bytestrings are empty, the entire input
--   is considered to be empty.
data S = S {-# UNPACK #-} !SB.ByteString
           LB.ByteString
           {-# UNPACK #-} !Int64

-- ^ A parser that produces a result of type @a@.
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

mkState :: LB.ByteString -> Int64 -> S
mkState s = case s of
              LB.Empty -> S SB.empty s
              LB.Chunk x xs -> S x xs

-- | Turn our split representation back into a normal lazy ByteString.
(+:) :: SB.ByteString -> LB.ByteString -> LB.ByteString
sb +: lb | SB.null sb = lb
         | otherwise = LB.Chunk sb lb
{-# INLINE (+:) #-}

infix 0 <?>

-- | Name the parser, in case failure occurs.
(<?>) :: Parser a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser a
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

-- | Match a single byte based on the given predicate.
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p =
    Parser $ \s@(S sb lb n) ->
           case SB.uncons sb of
             Just (c, sb') | p c -> Right (c, mkState (sb' +: lb) (n + 1))
                           | otherwise -> Left (sb +: lb, [])
             Nothing -> unParser (nextChunk >> satisfy p) s
{-# INLINE satisfy #-}

-- | Match a literal string exactly.
string :: LB.ByteString -> Parser LB.ByteString
string s = Parser $ \(S sb lb n) ->
           let bs = sb +: lb
               l = LB.length s
               (h,t) = LB.splitAt l bs
           in if s == h
              then Right (s, mkState t (n + l))
              else Left (bs, [])
{-# INLINE string #-}

-- | Match the end of a line.  This may be any of a newline character,
-- a carriage return character, or a carriage return followed by a newline.
endOfLine :: Parser ()
endOfLine = Parser $ \(S sb lb n) ->
            let bs = sb +: lb
            in if SB.null sb
               then Left (bs, ["EOL"])
               else case I.w2c (U.unsafeHead sb) of
                     '\n' -> Right ((), mkState (LB.tail bs) (n + 1))
                     '\r' -> let (h,t) = LB.splitAt 2 bs
                                 rn = L8.pack "\r\n"
                             in if h == rn
                                then Right ((), mkState t (n + 2))
                                else Right ((), mkState (LB.tail bs) (n + 1))
                     _ -> Left (bs, ["EOL"])

-- | Match a literal string, after applying a transformation to both
-- it and the matching text.  Useful for e.g. case insensitive string
-- comparison.
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

-- | Attempt a parse, but do not consume any input if the parse fails.
try :: Parser a -> Parser a
try p = Parser $ \s@(S sb lb _) ->
        case unParser p s of
          Left (_, msgs) -> Left (sb +: lb, msgs)
          ok -> ok

-- | Succeed if we have reached the end of the input string.
endOfInput :: Parser ()
endOfInput = Parser $ \s@(S sb lb _) -> if SB.null sb && LB.null lb
                                        then Right ((), s)
                                        else Left (sb +: lb, ["EOF"])

-- | Return all of the remaining input as a single string.
takeAll :: Parser LB.ByteString
takeAll = Parser $ \(S sb lb n) ->
          let bs = sb +: lb
          in Right (bs, mkState LB.empty (n + LB.length bs))

-- | Return exactly the given number of bytes.  If not enough are
-- available, fail.
takeCount :: Int -> Parser LB.ByteString
takeCount k =
  Parser $ \(S sb lb n) ->
      let bs = sb +: lb
          k' = fromIntegral k
          (h,t) = LB.splitAt k' bs
      in if LB.length h == k'
         then Right (h, mkState t (n + k'))
         else Left (bs, [show k ++ " bytes"])

-- | Consume bytes while the predicate succeeds.
takeWhile :: (Word8 -> Bool) -> Parser LB.ByteString
takeWhile p =
    Parser $ \(S sb lb n) ->
    let (h,t) = LB.span p (sb +: lb)
    in Right (h, mkState t (n + LB.length h))
{-# INLINE takeWhile #-}

-- | Consume bytes while the predicate fails.  If the predicate never
-- succeeds, the entire input string is returned.
takeTill :: (Word8 -> Bool) -> Parser LB.ByteString
takeTill p =
  Parser $ \(S sb lb n) ->
  let (h,t) = LB.break p (sb +: lb)
  in Right (h, mkState t (n + LB.length h))
{-# INLINE takeTill #-}

-- | Consume bytes while the predicate is true.  Fails if the
-- predicate fails on the first byte.
takeWhile1 :: (Word8 -> Bool) -> Parser LB.ByteString
takeWhile1 p =
    Parser $ \(S sb lb n) ->
    case LB.span p (sb +: lb) of
      (h,t) | LB.null h -> Left (t, [])
            | otherwise -> Right (h, mkState t (n + LB.length h))
{-# INLINE takeWhile1 #-}

-- | Test that a parser returned a non-null 'LB.ByteString'.
notEmpty :: Parser LB.ByteString -> Parser LB.ByteString 
notEmpty p = Parser $ \s ->
             case unParser p s of
               o@(Right (a, _)) ->
                   if LB.null a
                   then Left (a, ["notEmpty"])
                   else o
               x -> x

-- | Parse some input with the given parser, and return the input it
-- consumed as a string.
match :: Parser a -> Parser LB.ByteString
match p = do bs <- getInput
             start <- getConsumed
             p
             end <- getConsumed
             return (LB.take (end - start) bs)

-- | Apply a parser without consuming any input.
lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \s ->
         case unParser p s of
           Right (m, _) -> Right (m, s)
           err -> err

-- | Run a parser. The 'Int64' value is used as a base to count the
-- number of bytes consumed.
parseAt :: Parser a             -- ^ parser to run
        -> LB.ByteString        -- ^ input to parse
        -> Int64                -- ^ offset to count input from
        -> (LB.ByteString, Either ParseError (a, Int64))
parseAt p bs n = 
    case unParser p (mkState bs n) of
      Left (bs', msg) -> (bs', Left $ showError msg)
      Right (a, ~(S sb lb n')) -> (sb +: lb, Right (a, n'))
    where
      showError [""] = "Parser error\n"
      showError [msg] = "Parser error, expected:\n" ++ msg ++ "\n"
      showError [] = "Parser error\n"
      showError msgs = "Parser error, expected one of:\n" ++ unlines msgs

-- | Run a parser.
parse :: Parser a               -- ^ parser to run
      -> LB.ByteString          -- ^ input to parse
      -> (LB.ByteString, Either ParseError a)
parse p bs = case parseAt p bs 0 of
               (bs', Right (a, _)) -> (bs', Right a)
               (bs', Left err) -> (bs', Left err)

-- | Try out a parser, and print its result.
parseTest :: (Show a) => Parser a -> LB.ByteString -> IO ()
parseTest p s =
    case parse p s of
      (st, Left msg) -> putStrLn $ msg ++ "\nGot:\n" ++ show st
      (_, Right r) -> print r

#define PARSER Parser
#include "Word8Boilerplate.h"
