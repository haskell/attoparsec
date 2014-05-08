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
    ) where

import Data.Attoparsec.Internal.Types
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
