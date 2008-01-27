{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ParserCombinators.ByteStringParser.FastSet
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
module Data.ParserCombinators.ByteStringParser.FastSet
    (
    -- * Data type
      FastSet
    -- * Construction
    , set
    -- * Lookup
    , memberChar
    , memberWord8
    -- * Debugging
    , fromSet
    ) where

import qualified Data.ByteString as B
-- import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Internal as I
import qualified Data.ByteString.Unsafe as U
import Data.Word (Word8)
import Foreign.Storable (peekByteOff, pokeByteOff)

data FastSet = Sorted { fromSet :: {-# UNPACK #-} !B.ByteString }
             | Table  { fromSet :: {-# UNPACK #-} !B.ByteString }
    deriving (Eq, Ord)

instance Show FastSet where
    show (Sorted s) = "FastSet " ++ show s
    show (Table t)  = "FastSet " ++ fromTable t

-- | The lower bound on the size of a lookup table.  We choose this to
-- balance table density against performance.
tableCutoff :: Int
tableCutoff = 32

-- | Create a character set.
set :: B.ByteString -> FastSet
set s | B.length s < tableCutoff = Sorted . B.sort $ s
      | otherwise                = Table . mkTable $ s

-- | Check the table for membership.
memberChar :: Char -> FastSet -> Bool
memberChar c = memberWord8 (I.c2w c)

-- | Check the table for membership.
memberWord8 :: Word8 -> FastSet -> Bool
memberWord8 w (Table t)  = U.unsafeIndex t (fromIntegral w) == entry
memberWord8 w (Sorted s) = search 0 (B.length s - 1)
    where search !lo !hi
              | hi < lo = False
              | otherwise =
                  let mid = (lo + hi) `div` 2
                  in case compare w (U.unsafeIndex s mid) of
                       GT -> search lo (mid - 1)
                       LT -> search (mid + 1) hi
                       _ -> True

-- | The value in a table that indicates that a character is not
-- present.  We avoid NUL to make the table representation printable.
noEntry :: Word8
noEntry = 0x5f

-- | The value in a table that indicates that a character is present.
-- We use a printable character for readability.
entry :: Word8
entry = 0x21

mkTable :: B.ByteString -> B.ByteString
mkTable s = I.unsafeCreate 256 $ \t -> do
            I.memset t noEntry 256
            U.unsafeUseAsCStringLen s $ \(p, l) ->
              let loop n | n == l = return ()
                         | otherwise = do
                    c <- peekByteOff p n :: IO Word8
                    pokeByteOff t (fromIntegral c) entry
                    loop (n + 1)
              in loop 0

-- | Turn the table representation into a string, for debugging.
fromTable :: B.ByteString -> String
fromTable = snd . B.foldr go (0xff, [])
    where go c (!n, cs) | c == noEntry = (n - 1, cs)
                        | otherwise    = (n - 1, I.w2c n:cs)
