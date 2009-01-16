{-# LANGUAGE BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.Incremental
-- Copyright   :  Bryan O'Sullivan 2009
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient, and incremental parser combinators for lazy
-- 'L.ByteString' strings, loosely based on the Parsec library.
--
-- This module is heavily influenced by Adam Langley's incremental
-- parser in his @binary-strict@ package.
-- 
-----------------------------------------------------------------------------
module Data.Attoparsec.Incremental
    (
    -- * Incremental parsing
    -- $incremental

    -- * Parser types
      Parser
    , Result(..)

    -- * Running parsers
    , parse
    , parseWith
    , parseTest

    -- * Combinators
    , (<?>)
    , try

    -- * Parsing individual bytes
    , word8
    , notWord8
    , anyWord8
    , satisfy

    -- * Efficient string handling
    , string
    , skipWhile
    , takeCount
    , takeTill
    , takeWhile

    -- * State observation and manipulation functions
    , endOfInput
    , pushBack
    , yield

    -- * Combinators
    , module Data.Attoparsec.Combinator
    ) where

import Data.Attoparsec.Combinator
import Control.Monad (MonadPlus(..), ap)
import Data.Attoparsec.Internal ((+:))
import Data.Word (Word8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import Prelude hiding (takeWhile)

-- $incremental
-- Incremental parsing makes it possible to supply a parser with only
-- a limited amount of input.  If the parser cannot complete due to
-- lack of data, it will return a 'Partial' result with a continuation
-- to which more input can be supplied, as follows:
--
-- >   case parse myParser someInput of
-- >     Partial k -> k moreInput
--
-- To signal that no more input is available, pass an empty
-- string to this continuation.

data S = S {-# UNPACK #-} !S.ByteString -- first chunk of input
           L.ByteString                 -- rest of input
           [L.ByteString]               -- input acquired during backtracks
           !Bool                        -- have we hit EOF yet?
           {-# UNPACK #-} !Int          -- failure depth

-- | The result of a partial parse.
data Result a = Failed String
                -- ^ The parse failed, with the given error message.
              | Done L.ByteString a
                -- ^ The parse succeeded, producing the given
                -- result. The 'L.ByteString' contains any unconsumed
                -- input.
              | Partial (L.ByteString -> Result a)
                -- ^ The parse ran out of data before finishing. To
                -- resume the parse, pass more data to the given
                -- continuation.

instance (Show a) => Show (Result a) where
  show (Failed err)      = "Failed " ++ show err
  show (Done L.Empty rs) = "Done Empty " ++ show rs
  show (Done rest rs)    = "Done (" ++ show rest ++ ") " ++ show rs
  show (Partial _)       = "Partial"

-- | This is the internal version of the above. This is the type which
-- is actually used by the code, as it has the extra information
-- needed for backtracking. This is converted to a friendly 'Result'
-- type just before giving it to the outside world.
data IResult a = IFailed S String
               | IDone S a
               | IPartial (L.ByteString -> IResult a)

instance Show (IResult a) where
  show (IFailed _ err) = "IFailed " ++ show err
  show (IDone _ _)     = "IDone"
  show (IPartial _)    = "IPartial"

-- | The parser type.
newtype Parser r a = Parser {
      unParser :: S -> (a -> S -> IResult r) -> IResult r
    }

instance Monad (Parser r) where
  return a = Parser $ \s k -> k a s
  m >>= k = Parser $ \s cont -> unParser m s $ \a s' -> unParser (k a) s' cont
  fail err = Parser $ \s -> const $ IFailed s err

zero :: Parser r a
zero = fail ""

-- | I'm not sure if this is a huge bodge or not. It probably is.
--
-- When performing a choice (in @plus@), the failure depth in the
-- current state is incremented. If a failure is generated inside the
-- attempted path, the state carried in the IFailure will have this
-- incremented failure depth. However, we don't want to backtrack
-- after the attempted path has completed. Thus we insert this cut
-- continuation, which decrements the failure count of any failure
-- passing though, thus it would be caught in @plus@ and doesn't
-- trigger a backtrack.
cutContinuation :: (a -> S -> IResult r) -> a -> S -> IResult r
cutContinuation k v s =
  case k v s of
       IFailed (S lb i adds eof failDepth) err -> IFailed (S lb i adds eof (failDepth - 1)) err
       x -> x

appL :: L.ByteString -> L.ByteString -> L.ByteString
appL xs L.Empty = xs
appL L.Empty ys = ys
appL xs ys      = xs `L.append` ys

plus :: Parser r a -> Parser r a -> Parser r a
plus p1 p2 =
  Parser $ \(S sb lb adds eof failDepth) k ->
    let
      filt f@(IFailed (S _ _ adds' eof' failDepth') _)
        | failDepth' == failDepth + 1 =
            let lb' = lb `appL` L.concat (reverse adds')
            in  unParser p2 (S sb lb' (adds' ++ adds) eof' failDepth) k
        | otherwise = f
      filt (IPartial cont) = IPartial (filt . cont)
      filt v@(IDone _ _) = v
    in
      filt $ unParser p1 (S sb lb [] eof (failDepth + 1)) (cutContinuation k)

-- | This is a no-op combinator for compatibility.
try :: Parser r a -> Parser r a
try p = p

instance Functor (Parser r) where
    fmap f m = Parser $ \s cont -> unParser m s (cont . f)

infix 0 <?>

-- | Name the parser, in case failure occurs.
(<?>) :: Parser r a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser r a
{-# INLINE (<?>) #-}
p <?> msg =
  Parser $ \st k ->
    case unParser p st k of
      IFailed st' _ -> IFailed st' msg
      ok -> ok

initState :: L.ByteString -> S
initState (L.Chunk sb lb) = S sb lb [] False 0
initState _               = S S.empty L.empty [] False 0

mkState :: L.ByteString -> [L.ByteString] -> Bool -> Int -> S
mkState bs adds eof failDepth =
    case bs of
      L.Empty -> S S.empty L.empty adds eof failDepth
      L.Chunk sb lb -> S sb lb adds eof failDepth

addX :: L.ByteString -> [L.ByteString] -> [L.ByteString]
addX s adds | L.null s = adds
            | otherwise = s : adds

-- | Resume our caller, handing back a 'Partial' result. This function
-- is probably not useful, but provided for completeness.
yield :: Parser r ()
yield = Parser $ \(S sb lb adds eof failDepth) k ->
  IPartial $ \s -> k () (S sb (lb `appL` s) (addX s adds) eof failDepth)

continue :: (S -> IResult r) -> Parser r a
         -> (a -> S -> IResult r) -> S -> IResult r
continue onEOF p k (S _sb _lb adds eof failDepth) =
    if eof
    then onEOF (S S.empty L.empty adds True failDepth)
    else IPartial $ \s -> let st = contState s adds failDepth
                          in unParser p st k

takeWith :: (L.ByteString -> (L.ByteString, L.ByteString))
         -> Parser r L.ByteString
takeWith splitf =
  Parser $ \st@(S sb lb adds eof failDepth) k ->
  let (left,rest) = splitf (sb +: lb)
  in if L.null rest
     then continue (k left) (takeWith splitf) (k . appL left) st
     else k left (mkState rest adds eof failDepth)
    
-- | Consume bytes while the predicate succeeds.
takeWhile :: (Word8 -> Bool) -> Parser r L.ByteString
takeWhile = takeWith . L.span

-- | Consume bytes while the predicate fails.  If the predicate never
-- succeeds, the entire input string is returned.
takeTill :: (Word8 -> Bool) -> Parser r L.ByteString
takeTill = takeWith . L.break

-- | Return exactly the given number of bytes.  If not enough are
-- available, fail.
takeCount :: Int -> Parser r L.ByteString
takeCount = tc . fromIntegral where
 tc n = Parser $ \st@(S sb lb adds eof failDepth) k ->
        let (h,t) = L.splitAt n (sb +: lb)
            l = L.length h
        in if L.length h == n
           then k h (mkState t adds eof failDepth)
           else continue (`IFailed` "takeCount: EOF")
                         (tc (n - l)) (k . appL h) st

-- | Match a literal string exactly.
string :: L.ByteString -> Parser r L.ByteString
string s =
  Parser $ \st@(S sb lb adds eof failDepth) k ->
    case L.splitAt (L.length s) (sb +: lb) of
      (h,t)
        | h == s -> k s (mkState t adds eof failDepth)
      (h,L.Empty)
        | h `L.isPrefixOf` s ->
            continue (`IFailed` "string: EOF")
                     (string (L.drop (L.length h) s))
                     (k . appL h)
                     st
      _ -> IFailed st "string failed to match"

contState :: L.ByteString -> [L.ByteString] -> Int -> S
contState s adds failDepth
    | L.null s  = S S.empty L.empty [] True failDepth
    | otherwise = mkState s (addX s adds) False failDepth

-- | Match a single byte based on the given predicate.
satisfy :: (Word8 -> Bool) -> Parser r Word8
satisfy p =
  Parser $ \st@(S sb lb adds eof failDepth) k ->
    case S.uncons sb of
      Just (w, sb') | p w -> k w (S sb' lb adds eof failDepth)
                    | otherwise -> IFailed st "failed to match"
      Nothing -> case L.uncons lb of
                   Just (w, lb') | p w -> k w (mkState lb' adds eof failDepth)
                                 | otherwise -> IFailed st "failed to match"
                   Nothing -> continue (`IFailed` "satisfy: EOF")
                                       (satisfy p) k st

-- | Force the given string to appear next in the input stream.
pushBack :: L.ByteString -> Parser r ()
pushBack bs =
    Parser $ \(S sb lb adds eof failDepth) k ->
        k () (mkState (bs `appL` (sb +: lb)) adds eof failDepth)

-- | Succeed if we have reached the end of the input string.
endOfInput :: Parser r ()
endOfInput = Parser $ \st@(S sb lb _adds _eof _failDepth) k ->
             if not (S.null sb) || not (L.null lb)
             then IFailed st "endOfInput: not EOF"
             else continue (k ()) endOfInput k st

toplevelTranslate :: IResult a -> Result a
toplevelTranslate (IFailed _ err) = Failed err
toplevelTranslate (IDone (S sb lb _ _ _) value) = Done (sb +: lb) value
toplevelTranslate (IPartial k) = Partial $ toplevelTranslate . k

terminalContinuation :: a -> S -> IResult a
terminalContinuation v s = IDone s v

-- | Run a parser.
parse :: Parser r r -> L.ByteString -> Result r
parse m input =
  toplevelTranslate $ unParser m (initState input) terminalContinuation

-- | Run a parser, using the given function to resupply it with input.
--
-- Here's an example that shows how to parse data from a socket, using
-- Johan Tibbell's @network-bytestring@ package.
--
-- >  import qualified Data.ByteString.Lazy as L
-- >  import Data.Attoparsec.Incremental (Parser, Result, parseWith)
-- >  import Network.Socket.ByteString.Lazy (recv_)
-- >  import Network.Socket (Socket)
-- >
-- >  netParse :: Parser r r -> Socket -> IO (Result r)
-- >  netParse p sock = parseWith (recv_ sock 65536) p L.empty
parseWith :: Applicative f => f L.ByteString -- ^ resupply parser with input
          -> Parser r r                      -- ^ parser to run
          -> L.ByteString                    -- ^ initial input
          -> f (Result r)
parseWith refill p s =
  case parse p s of
    Partial k -> k <$> refill
    ok        -> pure ok

-- | Try out a parser, and print its result.
parseTest :: (Show r) => Parser r r -> L.ByteString -> IO ()
parseTest p s = print (parse p s)

#define PARSER Parser r
#include "Word8Boilerplate.h"
