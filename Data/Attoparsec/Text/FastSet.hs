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
import Data.List (sort, sortBy, nub)
import Data.Function (on)
import qualified Data.Array.Base as AB
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T
import Test.QuickCheck hiding ((.&.))

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
resolveCollisions [e] = [e]
resolveCollisions (a:b:entries) = a' : resolveCollisions (b':entries)
    where (a', b')
            | offset a < offset b = (a, b)
            | probe a < probe b = (b{offset=offset a}, a{offset=offset a + 1})
            | otherwise = (a, b{offset=offset a + 1})
                          
pad :: Int -> [Entry]-> [Entry]
pad = go 0
    where go _ m [] = replicate (max 1 m) empty
          go k m (e:entries) = map (const empty) [k..o - 1] ++ e : go (o + 1) (m + o - k - 1) entries
              where o = offset e
          empty = Entry '\0' maxBound 0
    
nextPowerOf2 :: Int -> Int
nextPowerOf2 0 = 1
nextPowerOf2 x = go (x - 1) 1
    where go y 32 = y + 1
          go y k  = go (y .|. (y `shiftR` k)) $ k * 2
          
fastHash :: Char -> Int
fastHash = fromEnum

fromList :: String -> FastSet
fromList s = FastSet (arr key) (arr initialOffset) mask'
    where s' = nub $ sort s
          l = length s'
          mask' = nextPowerOf2 ((5 * l) `div` 4) - 1
          offsets = map (\c -> fastHash c .&. mask') s'
          entries = pad mask' . 
                    resolveCollisions . 
                    sortBy (compare `on` initialOffset) $ 
                    zipWith (\c o -> Entry c o o) s' offsets
          arr :: A.IArray a e => (Entry -> e) -> a Int e
          arr f = AB.listArray (0, length entries - 1) $ map f entries
          
set :: T.Text -> FastSet
set = fromList . T.unpack
                                      
-- | Check the set for membership.
member :: Char -> FastSet -> Bool
member c a = go i
    where i = (fastHash c .&. mask a)
          go j
              | i' > i = False
              | i' == i && c == c' = True
              | otherwise = go (j + 1)
              where i' = AB.unsafeAt (initialOffsets a) j
                    c' = AB.unsafeAt (keys a) j
                       
charClass :: String -> FastSet
charClass = fromList . go
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""
