{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QC.Text (tests) where

import Control.Applicative ((<$>))
import Prelude hiding (takeWhile)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

instance Arbitrary T.Text where
    arbitrary   = T.pack <$> arbitrary

instance Arbitrary L.Text where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((L.fromChunks . map (T.pack . nonEmpty)) <$> arbitrary)
      where nonEmpty (NonEmpty a) = a

-- Naming.

{-
label (NonEmpty s) = case parse (anyWord8 <?> s) B.empty of
                            (_, Left err) -> s `isInfixOf` err
                            _             -> False
-}

-- Basic byte-level combinators.

maybeP p s = case P.parse p s `P.feed` T.empty of
               P.Done _ i -> Just i
               _          -> Nothing

defP p s = P.parse p s `P.feed` T.empty

satisfy w s = maybeP (P.satisfy (<=w)) (T.cons w s) == Just w

char c s = maybeP (P.char c) (T.cons c s) == Just c

anyChar s = maybeP P.anyChar s == if T.null s
                                  then Nothing
                                  else Just (T.head s)

notChar c (NonEmpty s) = maybeP (P.notChar c) bs == if v == c
                                                    then Nothing
                                                    else Just v
    where v = T.head bs
          bs = T.pack s

string s = maybeP (P.string s) s == Just s

skipWhile w s =
    let t = T.dropWhile (<=w) s
    in case defP (P.skipWhile (<=w)) s of
         P.Done t' () -> t == t'
         _            -> False

takeCount (Positive k) s =
    case maybeP (P.take k) s of
      Nothing -> k > T.length s
      Just s' -> k <= T.length s

takeWhile w s =
    let (h,t) = T.span (==w) s
    in case defP (P.takeWhile (==w)) s of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeWhile1 w s =
    let s'    = T.cons w s
        (h,t) = T.span (<=w) s'
    in case defP (P.takeWhile1 (<=w)) s' of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeTill w s =
    let (h,t) = T.break (==w) s
    in case defP (P.takeTill (==w)) s of
         P.Done t' h' -> t == t' && h == h'
         _            -> False

takeWhile1_empty = maybeP (P.takeWhile1 undefined) T.empty == Nothing

endOfInput s = maybeP P.endOfInput s == if T.null s
                                        then Just ()
                                        else Nothing

tests = [
    testProperty "satisfy" satisfy,
    testProperty "char" char,
    testProperty "notChar" notChar,
    testProperty "anyChar" anyChar,
    testProperty "string" string,
    testProperty "skipWhile" skipWhile,
    testProperty "takeCount" takeCount,
    testProperty "takeWhile" takeWhile,
    testProperty "takeWhile1" takeWhile1,
    testProperty "takeWhile1_empty" takeWhile1_empty,
    testProperty "takeTill" takeTill,
    testProperty "endOfInput" endOfInput
  ]
