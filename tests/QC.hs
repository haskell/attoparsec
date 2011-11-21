{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative ((<$>))
import Prelude hiding (takeWhile)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec as P
import qualified Data.ByteString as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

instance Arbitrary S.ByteString where
    arbitrary   = S.pack <$> arbitrary

instance Arbitrary L.ByteString where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((L.fromChunks . map (S.pack . nonEmpty)) <$> arbitrary)
      where nonEmpty (NonEmpty a) = a

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

satisfy w s = maybeP (P.satisfy (<=w)) (B.cons w s) == Just w

word8 w s = maybeP (P.word8 w) (B.cons w s) == Just w

anyWord8 s = maybeP P.anyWord8 s == if B.null s
                                    then Nothing
                                    else Just (B.head s)

notWord8 w (NonEmpty s) = maybeP (P.notWord8 w) bs == if v == w
                                                      then Nothing
                                                      else Just v
    where v = B.head bs
          bs = B.pack s

string s = maybeP (P.string s) s == Just s

skipWhile w s =
    let t = B.dropWhile (<=w) s
    in case defP (P.skipWhile (<=w)) s of
         P.Done t' () -> t == t'
         _            -> False

takeCount (Positive k) s =
    case maybeP (P.take k) s of
      Nothing -> k > B.length s
      Just s' -> k <= B.length s

takeWhile w s =
    let (h,t) = B.span (==w) s
    in case defP (P.takeWhile (==w)) s of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeWhile1 w s =
    let s'    = B.cons w s
        (h,t) = B.span (<=w) s'
    in case defP (P.takeWhile1 (<=w)) s' of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeTill w s =
    let (h,t) = B.break (==w) s
    in case defP (P.takeTill (==w)) s of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeWhile1_empty = maybeP (P.takeWhile1 undefined) B.empty == Nothing

endOfInput s = maybeP P.endOfInput s == if B.null s
                                        then Just ()
                                        else Nothing

main = defaultMain tests

tests = [
  testGroup "fnord" [
    testProperty "satisfy" satisfy,
    testProperty "word8" word8,
    testProperty "notWord8" notWord8,
    testProperty "anyWord8" anyWord8,
    testProperty "string" string,
    testProperty "skipWhile" skipWhile,
    testProperty "takeCount" takeCount,
    testProperty "takeWhile" takeWhile,
    testProperty "takeWhile1" takeWhile1,
    testProperty "takeWhile1_empty" takeWhile1_empty,
    testProperty "takeTill" takeTill,
    testProperty "endOfInput" endOfInput
    ]
  ]
