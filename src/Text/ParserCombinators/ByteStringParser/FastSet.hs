-----------------------------------------------------------------------------
-- |
-- Module      :  Text.ParserCombinators.ByteStringParser.FastSet
-- Copyright   :  Bryan O'Sullivan 2008
-- License     :  BSD3
-- 
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast 8-bit character set membership.
-- 
-----------------------------------------------------------------------------
module Text.ParserCombinators.ByteStringParser.FastSet
    (
      FastSet
    , set
    , member
    ) where

import Data.ByteString.Char8 as SB
import Data.ByteString.Internal as SB
import Data.ByteString.Unsafe as SB

newtype FastSet = FastSet SB.ByteString
    deriving (Eq, Ord, Show)

set :: SB.ByteString -> FastSet
set = FastSet . SB.sort

member :: Char -> FastSet -> Bool
member c (FastSet s) = search 0 (SB.length s - 1)
    where w = SB.c2w c
          search lo hi | hi < lo = False
                       | otherwise =
                           let mid = (lo + hi) `div` 2
                               cur = SB.unsafeIndex s mid
                           in case compare cur w of
                                GT -> search lo (mid - 1)
                                LT -> search (mid + 1) hi
                                _ -> True
