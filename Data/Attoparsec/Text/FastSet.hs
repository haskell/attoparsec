-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.FastSet
-- Copyright   :  Felipe Lessa 2010, Bryan O'Sullivan 2007-2014
-- License     :  BSD3
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast set membership tests for 'Char' values.  The set
-- representation is unboxed for efficiency.  We test for
-- membership using a hashtable implemented with Robin Hood
-- collision resolution.
--
-----------------------------------------------------------------------------
module Data.Attoparsec.Text.FastSet
    (
    -- * Data type
      FastSet
    -- * Construction
    , fromList
    , set
    -- * Lookup
    , member
    -- * Handy interface
    , charClass
    ) where

import Data.Bits
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Array.Base as AB
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T

data FastSet = FastSet {keys :: !(A.UArray Int Char), 
                        initialOffsets :: !(A.UArray Int Int), 
                        mask :: {-# UNPACK #-} !Int}
    deriving (Eq, Ord, Show)
    
data Entry = Entry {key :: {-# UNPACK #-} !Char,
                    initialOffset :: {-# UNPACK #-} !Int,
                    offset :: {-# UNPACK #-} !Int} deriving (Show)
                    
probe :: Entry -> Int
probe e = offset e - initialOffset e

resolveCollisions :: [Entry] -> [Entry]
resolveCollisions [] = []
resolveCollisions [x] = [x]
resolveCollisions (a:b:ys)
    | key a == key b = resolveCollisions (a:ys)
    | otherwise = a' : resolveCollisions (b':ys)
    where (a', b')
            | offset a < offset b = (a, b)
            | probe a < probe b = (b{offset=offset a}, a{offset=offset a + 1})
            | otherwise = (a, b{offset=offset a + 1})
                          
pad :: [Entry]-> [Entry]
pad = go 0
    where go _ [] = [empty]
          go k (p:ps) = map (const empty) [k..o - 1] ++ p : go (o + 1) ps
              where o = offset p
          empty = Entry '\0' maxBound 0
    
nextPowerOf2 :: Int -> Int
nextPowerOf2 x = go (x - 1) 1
    where go y 32 = y + 1
          go y k  = go (y .|. (y `shiftR` k)) $ k * 2

fastHash :: Char -> Int
fastHash c = fromEnum

fromList :: String -> FastSet
fromList s = FastSet (arr key) (arr initialOffset) mask'
    where l = length s
          mask' = nextPowerOf2 ((5 * l) `div` 4) - 1
          offsets = map (\c -> fastHash c .&. mask') s
          entries = pad . resolveCollisions $ sortBy (compare `on` initialOffset) $ zipWith (\c o -> Entry c o o) s offsets
          arr :: A.IArray a e => (Entry -> e) -> a Int e
          arr f = AB.listArray (0, length entries - 1) $ map f entries
          
set :: T.Text -> FastSet
set = fromList . T.unpack
                                      
-- | Check the set for membership.
member :: Char -> FastSet -> Bool
member c a = go 0
    where i = fastHash c .&. mask a
          go :: Int -> Bool
          go p
              | p > p' || p' == maxBound = False
              | p' == p && c' == c = True
              | otherwise = go (p + 1)
              where j = i + p
                    p' = j - AB.unsafeAt (initialOffsets a) j
                    c' = AB.unsafeAt (keys a) j
                       
charClass :: String -> FastSet
charClass = fromList . go
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""
