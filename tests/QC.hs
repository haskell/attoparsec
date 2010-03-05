{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Prelude hiding (takeWhile)
import QCSupport
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (NonEmpty)
import qualified Data.Attoparsec as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- Make sure that structures whose types claim they are non-empty
-- really are.

nonEmptyList l = length (nonEmpty l) > 0
    where types = l :: NonEmpty [Int]
nonEmptyBS l = B.length (nonEmpty l) > 0

-- Naming.

{-
label (NonEmpty s) = case parse (anyWord8 <?> s) B.empty of
                            (_, Left err) -> s `isInfixOf` err
                            _             -> False
-}

-- Basic byte-level combinators.

maybeP p s = case P.parse p s `P.feed` B.empty of
               P.Done _ i -> Just i
               _          -> Nothing

defP p s = P.parse p s `P.feed` B.empty

word8 (NonEmpty s) = maybeP (P.word8 w) s == Just w
    where w = B.head s

anyWord8 (NonEmpty s) = isJust $ maybeP P.anyWord8 s

notWord8 (w, NonEmpty s) = v /= w ==> maybeP (P.notWord8 w) s == Just v
    where v = B.head s

string s = maybeP (P.string s) s == Just s

skipWhile (w,s) =
    let (h,t) = B.span (==w) s
    in case defP (P.skipWhile (==w)) s of
         P.Done t' () -> t == t'
         _            -> False

takeCount (k,s) =
    k >= 0 ==>
    case maybeP (P.take k) s of
      Nothing -> j > B.length s
      Just s' -> j <= B.length s
  where j = fromIntegral k

takeWhile (w,s) =
    let (h,t) = B.span (==w) s
    in case defP (P.takeWhile (==w)) s of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeWhile1 (w, s) =
    let s'    = B.cons w s
        (h,t) = B.span (<=w) s'
    in case defP (P.takeWhile1 (<=w)) s' of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeTill (w, s) =
    let (h,t) = B.break (==w) s
    in case defP (P.takeTill (==w)) s of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeWhile1_empty = maybeP (P.takeWhile1 undefined) B.empty == Nothing

main = defaultMain tests

tests = [
  testGroup "fnord" [
    testProperty "nonEmptyList" nonEmptyList,
    testProperty "nonEmptyBS" nonEmptyBS,
    testProperty "word8" word8,
    testProperty "notWord8" notWord8,
    testProperty "anyWord8" anyWord8,
    testProperty "string" string,
    testProperty "skipWhile" skipWhile,
    testProperty "takeCount" takeCount,
    testProperty "takeWhile" takeWhile,
    testProperty "takeWhile1" takeWhile1,
    testProperty "takeWhile1_empty" takeWhile1_empty,
    testProperty "takeTill" takeTill
    ]
  ]
