{-# LANGUAGE BangPatterns, CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.Attoparsec.Incremental
-- Copyright   :  Bryan O'Sullivan 2009
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for lazy 'LB.ByteString'
-- strings, loosely based on 'Text.ParserCombinators.Parsec'.
--
-- This module is heavily influenced by Adam Langley's incremental
-- parser in his binary-strict package.
-- 
-----------------------------------------------------------------------------
module Data.ParserCombinators.Attoparsec.Incremental
    (
      Parser
    , Result(..)
    , parse
    , parseWith

    , (<?>)
    , takeWhile
    , takeTill
    , takeCount
    , string
    , satisfy
    , pushBack

    , word8
    , notWord8
    , anyWord8

    , many1
    , skipWhile
    , skipMany
    , skipMany1
    , sepBy
    , sepBy1
    , manyTill
    , count

    , yield
    ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Data.ParserCombinators.Attoparsec.Internal ((+:))
import Data.Word (Word8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import Prelude hiding (takeWhile)

data S = S {-# UNPACK #-} !S.ByteString -- ^ first chunk of input
           L.ByteString                 -- ^ rest of input
           [L.ByteString]               -- ^ input acquired during backtracks
           {-# UNPACK #-} !Int          -- ^ failure depth

-- | The result of a partial parse
data Result a = Failed String
                -- ^ the parse failed with the given error message
              | Done L.ByteString a
                -- ^ the parse finished and produced the given list of
                --   results doing so. Any unparsed data is returned.
              | Partial (L.ByteString -> Result a)
                -- ^ the parse ran out of data before finishing, but produced
                --   the given list of results before doing so. To continue the
                --   parse pass more data to the given continuation

instance (Show a) => Show (Result a) where
  show (Failed err) = "Failed " ++ err
  show (Done rest rs) = "Done " ++ show rest ++ " " ++ show rs
  show (Partial _) = "Partial"

-- | This is the internal version of the above. This is the type which is
--   actually used by the code, as it has the extra information needed
--   for backtracking. This is converted to an external friendly @Result@
--   type just before giving it to the outside world.
data IResult a = IFailed S String
               | IDone S a
               | IPartial (L.ByteString -> IResult a)

instance Show (IResult a) where
  show (IFailed _ err) = "IFailed " ++ err
  show (IDone _ _) = "IDone"
  show (IPartial _) = "IPartial"

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
       IFailed (S lb i adds failDepth) err -> IFailed (S lb i adds (failDepth - 1)) err
       x -> x

appL :: L.ByteString -> L.ByteString -> L.ByteString
appL xs L.Empty = xs
appL L.Empty ys = ys
appL xs ys      = xs `L.append` ys

plus :: Parser r a -> Parser r a -> Parser r a
plus p1 p2 =
  Parser $ \(S sb lb adds failDepth) k ->
    let
      filt f@(IFailed (S _ _ adds' failDepth') _)
        | failDepth' == failDepth + 1 =
            let lb' = lb `appL` L.concat (reverse adds')
            in  unParser p2 (S sb lb' (adds' ++ adds) failDepth) k
        | otherwise = f
      filt (IPartial cont) = IPartial (filt . cont)
      filt v@(IDone _ _) = v
    in
      filt $ unParser p1 (S sb lb [] (failDepth + 1)) (cutContinuation k)

instance Functor (Parser r) where
    fmap f m = Parser $ \s cont -> unParser m s (cont . f)

infix 0 <?>

-- | Name the parser.
(<?>) :: Parser r a -> String -> Parser r a
{-# INLINE (<?>) #-}
p <?> msg =
  Parser $ \st k ->
    case unParser p st k of
      IFailed st' _ -> IFailed st' msg
      ok -> ok

initState :: L.ByteString -> S
initState (L.Chunk sb lb) = S sb lb [] 0
initState _               = S S.empty L.empty [] 0

mkState :: L.ByteString -> [L.ByteString] -> Int -> S
mkState bs adds failDepth =
    case bs of
      L.Empty -> S S.empty L.empty adds failDepth
      L.Chunk sb lb -> S sb lb adds failDepth

addX :: L.ByteString -> [L.ByteString] -> [L.ByteString]
addX s adds | L.null s = adds
            | otherwise = s : adds

yield :: Parser r ()
yield = Parser $ \(S sb lb adds failDepth) k ->
  IPartial $ \s -> k () (S sb (lb `appL` s) (addX s adds) failDepth)

takeWith :: (L.ByteString -> (L.ByteString, L.ByteString))
         -> Parser r L.ByteString
takeWith splitf =
  Parser $ \(S sb lb adds failDepth) k ->
  let (left,rest) = splitf (sb +: lb)
  in case rest of
       L.Empty -> IPartial $ \s ->
                  let s' = mkState s (addX s adds) failDepth
                      k' a = k (appL left a)
                  in unParser (takeWith splitf) s' k'
       L.Chunk h t -> k left (S h t adds failDepth)
    
takeWhile :: (Word8 -> Bool) -> Parser r L.ByteString
takeWhile = takeWith . L.span

takeTill :: (Word8 -> Bool) -> Parser r L.ByteString
takeTill = takeWith . L.break

takeCount :: Int -> Parser r L.ByteString
takeCount = tc . fromIntegral where
 tc (!n) = Parser $ \(S sb lb adds failDepth) k ->
           let (h,t) = L.splitAt n (sb +: lb)
               l = L.length h
           in if L.length h == n
              then k h (mkState t adds failDepth)
              else IPartial $ \s ->
                   let st = mkState s (addX s adds) failDepth
                       k' a = k (appL h a)
                   in unParser (tc (n - l)) st k'

string :: L.ByteString -> Parser r L.ByteString
string s =
  Parser $ \st@(S sb lb adds failDepth) k ->
    case L.splitAt (L.length s) (sb +: lb) of
      (h,t)
        | h == s -> k s (mkState t adds failDepth)
      (h,L.Empty)
        | h `L.isPrefixOf` s ->
            IPartial $ \s' ->
            let st'  = mkState s' (addX s' adds) failDepth
                k' a = k (appL h a)
                r'   = L.drop (L.length h) s
            in unParser (string r') st' k'
      _ -> IFailed st "string failed to match"

emptyState = S S.empty L.empty

satisfy :: (Word8 -> Bool) -> Parser r Word8
satisfy p =
  Parser $ \st@(S sb lb adds failDepth) k ->
    case S.uncons sb of
      Just (w, sb') | p w -> k w (S sb' lb adds failDepth)
                    | otherwise -> IFailed st "failed to match"
      Nothing -> case L.uncons lb of
                   Just (w, lb') | p w -> k w (mkState lb' adds failDepth)
                                 | otherwise -> IFailed st "failed to match"
                   Nothing -> IPartial $ \s ->
                              let st' = emptyState adds failDepth
                              in if L.null s
                                 then IFailed st "barf"
                                 else unParser (satisfy p) st' k

pushBack :: L.ByteString -> Parser r ()
pushBack bs =
    Parser $ \(S sb lb adds failDepth) k ->
        k () (mkState (bs `appL` (sb +: lb)) adds failDepth)

toplevelTranslate :: IResult a -> Result a
toplevelTranslate (IFailed _ err) = Failed err
toplevelTranslate (IDone (S sb lb _ _) value) = Done (sb +: lb) value
toplevelTranslate (IPartial k) = Partial $ toplevelTranslate . k

terminalContinuation :: a -> S -> IResult a
terminalContinuation v s = IDone s v

parse :: Parser r r -> L.ByteString -> Result r
parse m input =
  toplevelTranslate $ unParser m (initState input) terminalContinuation

parseWith :: Applicative f => f L.ByteString -> Parser r r -> L.ByteString
          -> f (Result r)
parseWith refill p s =
  case parse p s of
    Partial k -> k <$> refill
    ok        -> pure ok

#define PARSER Parser r
#include "Word8Boilerplate.h"
