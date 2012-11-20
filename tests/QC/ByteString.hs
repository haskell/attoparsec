{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module QC.ByteString (tests) where

import Control.Applicative ((<$>), (<*>))
import Prelude hiding (takeWhile)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Lazy as PL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

instance Arbitrary B.ByteString where
    arbitrary   = B.pack <$> arbitrary

instance Arbitrary L.ByteString where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((L.fromChunks . map (B.pack . nonEmpty)) <$> arbitrary)
      where nonEmpty (NonEmpty a) = a

-- Naming.

{-
label (NonEmpty s) = case parse (anyWord8 <?> s) B.empty of
                            (_, Left err) -> s `isInfixOf` err
                            _             -> False
-}

-- Basic byte-level combinators.

maybeP p = PL.maybeResult . PL.parse p

defP p = PL.parse p

satisfy w s = maybeP (P.satisfy (<=w)) (L.cons w s) == Just w

word8 w s = maybeP (P.word8 w) (L.cons w s) == Just w

anyWord8 s
    | L.null s  = p == Nothing
    | otherwise = p == Just (L.head s)
  where p = maybeP P.anyWord8 s

notWord8 w (NonEmpty s) = maybeP (P.notWord8 w) bs == if v == w
                                                      then Nothing
                                                      else Just v
    where v = L.head bs
          bs = L.pack s

peekWord8 s
    | L.null s  = p == Just (Nothing, s)
    | otherwise = p == Just (Just (L.head s), s)
  where p = maybeP ((,) <$> P.peekWord8 <*> P.takeLazyByteString) s

string s t = maybeP (P.string s') (s `L.append` t) == Just s'
  where s' = toStrict s

toStrict = B.concat . L.toChunks

skipWhile w s =
    let t = L.dropWhile (<=w) s
    in case defP (P.skipWhile (<=w)) s of
         PL.Done t' () -> t == t'
         _             -> False

takeCount (Positive k) s =
    case maybeP (P.take k) s of
      Nothing -> fromIntegral k > L.length s
      Just s' -> fromIntegral k <= L.length s

takeWhile w s =
    let (h,t) = L.span (==w) s
    in case defP (P.takeWhile (==w)) s of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeWhile1 w s =
    let s'    = L.cons w s
        (h,t) = L.span (<=w) s'
    in case defP (P.takeWhile1 (<=w)) s' of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeTill w s =
    let (h,t) = L.break (==w) s
    in case defP (P.takeTill (==w)) s of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeWhile1_empty = maybeP (P.takeWhile1 undefined) L.empty == Nothing

endOfInput s = maybeP P.endOfInput s == if L.null s
                                        then Just ()
                                        else Nothing

scan s (Positive k) = maybeP p s == (Just $ toStrict $ L.take k s)
  where p = P.scan k $ \ n _ ->
            if n > 0 then let !n' = n - 1 in Just n' else Nothing

tests = [
    testProperty "satisfy" satisfy,
    testProperty "word8" word8,
    testProperty "notWord8" notWord8,
    testProperty "anyWord8" anyWord8,
    testProperty "peekWord8" peekWord8,
    testProperty "string" string,
    testProperty "skipWhile" skipWhile,
    testProperty "takeCount" takeCount,
    testProperty "takeWhile" takeWhile,
    testProperty "takeWhile1" takeWhile1,
    testProperty "takeWhile1_empty" takeWhile1_empty,
    testProperty "takeTill" takeTill,
    testProperty "endOfInput" endOfInput,
    testProperty "scan" scan
  ]
