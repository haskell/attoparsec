-- |
-- Module      :  Data.Attoparsec.Text.FastSet
-- Copyright   :  Bryan O'Sullivan 2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Fast set membership tests for 'Char' values.

module Data.Attoparsec.Text.FastSet
    (
    -- * Data type
      FastSet
    -- * Construction
    , fromList
    -- * Lookup
    , member
    -- * Handy interface
    , charClass
    ) where

import qualified Data.IntSet as I
import Data.Char (ord)

newtype FastSet = FastSet I.IntSet

fromList :: String -> FastSet
fromList = FastSet . I.fromList . map ord

-- | Check the set for membership.
member :: Char -> FastSet -> Bool
member c (FastSet s) = I.member (ord c) s
{-# INLINE member #-}

charClass :: String -> FastSet
charClass = fromList . go
  where go (a:'-':b:xs) = [a..b] ++ go xs
        go (x:xs)       = x : go xs
        go _            = ""
