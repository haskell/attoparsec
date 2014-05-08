{-# LANGUAGE BangPatterns #-}
-- |
-- Module      :  Data.Attoparsec.Internal
-- Copyright   :  Bryan O'Sullivan 2012
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators, loosely based on the Parsec
-- library.

module Data.Attoparsec.Internal
    (
      compareResults
    , get
    , advance
    , endOfChunk
    , ensure
    , prompt
    , demandInput
    , wantInput
    ) where

import Data.Attoparsec.Internal.Types
import Data.ByteString (ByteString)
import Data.Text (Text)
import Prelude hiding (succ)

-- | Compare two 'IResult' values for equality.
--
-- If both 'IResult's are 'Partial', the result will be 'Nothing', as
-- they are incomplete and hence their equality cannot be known.
-- (This is why there is no 'Eq' instance for 'IResult'.)
compareResults :: (Eq t, Eq r) => IResult t r -> IResult t r -> Maybe Bool
compareResults (Fail t0 ctxs0 msg0) (Fail t1 ctxs1 msg1) =
    Just (t0 == t1 && ctxs0 == ctxs1 && msg0 == msg1)
compareResults (Done t0 r0) (Done t1 r1) =
    Just (t0 == t1 && r0 == r1)
compareResults (Partial _) (Partial _) = Nothing
compareResults _ _ = Just False

get :: Chunk t => Parser t t
get = Parser $ \t pos more _lose succ -> succ t pos more (unsafeChunkDrop pos t)
{-# INLINE get #-}

endOfChunk :: Chunk t => Parser t Bool
endOfChunk = Parser $ \t pos more _lose succ ->
  succ t pos more (pos == chunkLength t)
{-# INLINE endOfChunk #-}

advance :: Int -> Parser t ()
advance n = Parser $ \t pos more _lose succ -> succ t (pos+n) more ()
{-# INLINE advance #-}

ensureSuspended :: Chunk t => Int -> t -> Pos -> More
                -> Failure t r -> Success t t r -> IResult t r
ensureSuspended n t pos more lose succ =
    runParser (demandInput >> go) t pos more lose succ
  where go = Parser $ \t' pos' more' lose' succ' ->
          if chunkLengthAtLeast pos' n t'
          then succ' t' pos' more' (substring pos n t')
          else runParser (demandInput >> go) t' pos' more' lose' succ'
{-# SPECIALIZE ensureSuspended :: Int -> ByteString -> Pos -> More
                               -> Failure ByteString r
                               -> Success ByteString ByteString r
                               -> IResult ByteString r #-}
{-# SPECIALIZE ensureSuspended :: Int -> Text -> Pos -> More
                               -> Failure Text r -> Success Text Text r
                               -> IResult Text r #-}

-- | If at least @n@ elements of input are available, return the
-- current input, otherwise fail.
ensure :: Chunk t => Int -> Parser t t
ensure n = Parser $ \t pos more lose succ ->
    if chunkLengthAtLeast pos n t
    then succ t pos more (substring pos n t)
    -- The uncommon case is kept out-of-line to reduce code size:
    else ensureSuspended n t pos more lose succ
-- Non-recursive so the bounds check can be inlined:
{-# INLINE ensure #-}

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: Chunk t => t -> Pos -> More
       -> (t -> Pos -> More -> IResult t r)
       -> (t -> Pos -> More -> IResult t r)
       -> IResult t r
prompt t pos _more lose succ = Partial $ \s ->
  if nullChunk s
  then lose t pos Complete
  else succ (t <> s) pos Incomplete
{-# SPECIALIZE prompt :: ByteString -> Pos -> More
                      -> (ByteString -> Pos -> More -> IResult ByteString r)
                      -> (ByteString -> Pos -> More -> IResult ByteString r)
                      -> IResult ByteString r #-}
{-# SPECIALIZE prompt :: Text -> Pos -> More
                      -> (Text -> Pos -> More -> IResult Text r)
                      -> (Text -> Pos -> More -> IResult Text r)
                      -> IResult Text r #-}

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Chunk t => Parser t ()
demandInput = Parser $ \t pos more lose succ ->
  case more of
    Complete -> lose t pos more [] "not enough input"
    _ -> let lose' t' pos' more' = lose t' pos' more' [] "not enough input"
             succ' t' pos' more' = succ t' pos' more' ()
         in prompt t pos more lose' succ'
{-# SPECIALIZE demandInput :: Parser ByteString () #-}
{-# SPECIALIZE demandInput :: Parser Text () #-}

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: Chunk t => Parser t Bool
wantInput = Parser $ \t pos more _lose succ ->
  case () of
    _ | chunkLengthAtLeast pos 1 t -> succ t pos more True
      | more == Complete -> succ t pos more False
      | otherwise       -> let lose' t' pos' more' = succ t' pos' more' False
                               succ' t' pos' more' = succ t' pos' more' True
                           in prompt t pos more lose' succ'
{-# SPECIALIZE wantInput :: Parser ByteString Bool #-}
{-# SPECIALIZE wantInput :: Parser Text Bool #-}
