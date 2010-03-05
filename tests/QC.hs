module Main (main) where

import Control.Applicative
import Data.Attoparsec as P
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Control.Monad (forM_)
import Debug.Trace
import System.IO
import Test.QuickCheck hiding (NonEmpty)
import qualified Data.ByteString as B
import System.Environment
import QCSupport
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- Make sure that structures whose types claim they are non-empty
-- really are.

prop_nonEmptyList l = length (nonEmpty l) > 0
    where types = l :: NonEmpty [Int]
prop_nonEmptyBS l = B.length (nonEmpty l) > 0

-- Naming.

{-
prop_label (NonEmpty s) = case parse (anyWord8 <?> s) B.empty of
                            (_, Left err) -> s `isInfixOf` err
                            _             -> False
-}

-- Basic byte-level combinators.

maybeP p s = case parse p s `feed` B.empty of
               Done _ i -> Just i
               _        -> Nothing

defP p s = parse p s `feed` B.empty

prop_word8 (NonEmpty s) = maybeP (word8 w) s == Just w
    where w = B.head s

prop_anyWord8 (NonEmpty s) = isJust $ maybeP anyWord8 s

prop_notWord8 (w, NonEmpty s) = v /= w ==> maybeP (notWord8 w) s == Just v
    where v = B.head s

prop_string s = maybeP (string s) s == Just s

prop_skipWhile (w,s) =
    let (h,t) = B.span (==w) s
    in case defP (skipWhile (==w)) s of
         Done t' () -> t == t'
         _          -> False

prop_takeCount (k,s) =
    k >= 0 ==>
    case maybeP (P.take k) s of
      Nothing -> j > B.length s
      Just s' -> j <= B.length s
  where j = fromIntegral k

prop_takeWhile (w,s) =
    let (h,t) = B.span (==w) s
    in case defP (P.takeWhile (==w)) s of
         Done t' h' -> t == t' && h == h'
         _          -> False

prop_takeWhile1 (w, NonEmpty s) =
    let (h,t) = B.span (==w) s
    in case defP (P.takeWhile1 (==w)) s of
         Done t' h' -> t == t' && h == h'
         _          -> False

prop_takeTill (w, s) =
    let (h,t) = B.break (==w) s
    in case defP (P.takeTill (==w)) s of
         Done t' h' -> t == t' && h == h'
         _          -> False

prop_takeWhile1_empty = maybeP (P.takeWhile1 undefined) B.empty == Nothing

main = defaultMain tests

tests = [
  testGroup "fnord" [
    testProperty "nonEmptyList" prop_nonEmptyList,
    testProperty "nonEmptyBS" prop_nonEmptyBS,
    testProperty "word8" prop_word8,
    testProperty "notWord8" prop_notWord8,
    testProperty "anyWord8" prop_anyWord8,
    testProperty "string" prop_string,
    testProperty "skipWhile" prop_skipWhile,
    testProperty "takeCount" prop_takeCount,
    testProperty "takeWhile" prop_takeWhile,
    testProperty "takeWhile1" prop_takeWhile1,
    testProperty "takeWhile1_empty" prop_takeWhile1_empty,
    testProperty "takeTill" prop_takeTill
    ]
  ]
