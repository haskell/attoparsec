{-# LANGUAGE CPP, BangPatterns #-}
-- |
-- Module      :  Data.Attoparsec.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2014
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators, loosely based on the Parsec
-- library.

module Data.Attoparsec.Internal
    ( compareResults
    , prompt
    , demandInput
    ) where

#if __GLASGOW_HASKELL__ >= 700
import Data.ByteString (ByteString)
import Data.Text (Text)
#endif
import Data.Attoparsec.Internal.Types
import Prelude hiding (succ)

-- | Compare two 'IResult' values for equality.
--
-- If both 'IResult's are 'Partial', the result will be 'Nothing', as
-- they are incomplete and hence their equality cannot be known.
-- (This is why there is no 'Eq' instance for 'IResult'.)
compareResults :: (Eq i, Eq r) => IResult i r -> IResult i r -> Maybe Bool
compareResults (Fail t0 ctxs0 msg0) (Fail t1 ctxs1 msg1) =
    Just (t0 == t1 && ctxs0 == ctxs1 && msg0 == msg1)
compareResults (Done t0 r0) (Done t1 r1) =
    Just (t0 == t1 && r0 == r1)
compareResults (Partial _) (Partial _) = Nothing
compareResults _ _ = Just False

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: Chunk t
       => State t -> Pos -> More
       -> (State t -> Pos -> More -> IResult t r)
       -> (State t -> Pos -> More -> IResult t r)
       -> IResult t r
prompt t pos _more lose succ = Partial $ \s ->
  if nullChunk s
  then lose t pos Complete
  else succ (pappendChunk t s) pos Incomplete
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE prompt :: State ByteString -> Pos -> More
                      -> (State ByteString -> Pos -> More
                          -> IResult ByteString r)
                      -> (State ByteString -> Pos -> More
                          -> IResult ByteString r)
                      -> IResult ByteString r #-}
{-# SPECIALIZE prompt :: State Text -> Pos -> More
                      -> (State Text -> Pos -> More -> IResult Text r)
                      -> (State Text -> Pos -> More -> IResult Text r)
                      -> IResult Text r #-}
#endif

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Chunk t => Parser t ()
demandInput = Parser $ \t pos more lose succ ->
  case more of
    Complete -> lose t pos more [] "not enough input"
    _ -> let lose' t' pos' more' = lose t' pos' more' [] "not enough input"
             succ' t' pos' more' = succ t' pos' more' ()
         in prompt t pos more lose' succ'
#if __GLASGOW_HASKELL__ >= 700
{-# SPECIALIZE demandInput :: Parser ByteString () #-}
{-# SPECIALIZE demandInput :: Parser Text () #-}
#endif
