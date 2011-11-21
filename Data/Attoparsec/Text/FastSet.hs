-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Attoparsec.FastSet
-- Copyright   :  Felipe Lessa 2010, Bryan O'Sullivan 2008
-- License     :  BSD3
--
-- Maintainer  :  felipe.lessa@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast set membership tests for 'Char' values.  The set
-- representation is unboxed for efficiency.  We test for
-- membership using a binary search.
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

import Data.List (sort)
import qualified Data.Array.Base as AB
import qualified Data.Array.Unboxed as A
import qualified Data.Text as T

newtype FastSet = FastSet (A.UArray Int Char)
    deriving (Eq, Ord, Show)

-- | Create a set.
set :: T.Text -> FastSet
set t = mkSet (T.length t) (sort $ T.unpack t)

fromList :: [Char] -> FastSet
fromList cs = mkSet (length cs) (sort cs)

mkSet :: Int -> [Char] -> FastSet
mkSet l = FastSet . A.listArray (0,l-1)

-- | Check the set for membership.
member :: Char -> FastSet -> Bool
member c (FastSet a) = uncurry search (A.bounds a)
    where search lo hi
              | hi < lo = False
              | otherwise =
                  let mid = (lo + hi) `div` 2
                  in case compare c (AB.unsafeAt a mid) of
                       GT -> search (mid + 1) hi
                       LT -> search lo (mid - 1)
                       _ -> True

charClass :: String -> FastSet
charClass = fromList . go
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""
