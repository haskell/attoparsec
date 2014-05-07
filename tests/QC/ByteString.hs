{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module QC.ByteString (tests) where

import Control.Applicative ((<$>), (<*>))
import Data.Char (chr, ord)
import Data.Int (Int64)
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import qualified Data.Attoparsec.ByteString.Lazy as PL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

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

maybeP :: PL.Parser r -> L.ByteString -> Maybe r
maybeP p = PL.maybeResult . PL.parse p

satisfy :: Word8 -> L.ByteString -> Bool
satisfy w s = maybeP (P.satisfy (<=w)) (L.cons w s) == Just w

satisfyWith :: Char -> L.ByteString -> Bool
satisfyWith c s = maybeP (P.satisfyWith (chr . fromIntegral) (<=c))
                         (L.cons (fromIntegral (ord c)) s) == Just c

word8 :: Word8 -> L.ByteString -> Bool
word8 w s = maybeP (P.word8 w) (L.cons w s) == Just w

skip :: Word8 -> L.ByteString -> Bool
skip w s =
  case (maybeP (P.skip (<w)) s, L.uncons s) of
    (Nothing, mcs) -> maybe True (not . it) mcs
    (Just _,  mcs) -> maybe False it mcs
  where it cs = fst cs < w

anyWord8 :: L.ByteString -> Bool
anyWord8 s
    | L.null s  = p == Nothing
    | otherwise = p == Just (L.head s)
  where p = maybeP P.anyWord8 s

notWord8 :: Word8 -> NonEmptyList Word8 -> Bool
notWord8 w (NonEmpty s) = maybeP (P.notWord8 w) bs == if v == w
                                                      then Nothing
                                                      else Just v
    where v = L.head bs
          bs = L.pack s

peekWord8 :: L.ByteString -> Bool
peekWord8 s
    | L.null s  = p == Just (Nothing, s)
    | otherwise = p == Just (Just (L.head s), s)
  where p = maybeP ((,) <$> P.peekWord8 <*> P.takeLazyByteString) s

peekWord8' :: L.ByteString -> Bool
peekWord8' s = maybeP P.peekWord8' s == (fst <$> L.uncons s)

string :: L.ByteString -> L.ByteString -> Bool
string s t = maybeP (P.string s') (s `L.append` t) == Just s'
  where s' = toStrict s

toStrict :: L.ByteString -> B.ByteString
toStrict = B.concat . L.toChunks

skipWhile :: Word8 -> L.ByteString -> Bool
skipWhile w s =
    let t = L.dropWhile (<=w) s
    in case PL.parse (P.skipWhile (<=w)) s of
         PL.Done t' () -> t == t'
         _             -> False

takeCount :: Positive Int -> L.ByteString -> Bool
takeCount (Positive k) s =
    case maybeP (P.take k) s of
      Nothing -> fromIntegral k > L.length s
      Just _s -> fromIntegral k <= L.length s

takeWhile :: Word8 -> L.ByteString -> Bool
takeWhile w s =
    let (h,t) = L.span (==w) s
    in case PL.parse (P.takeWhile (==w)) s of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

take :: Int -> L.ByteString -> Bool
take n s = maybe (L.length s < fromIntegral n) (== B.take n (toStrict s)) $
           maybeP (P.take n) s

takeByteString :: L.ByteString -> Bool
takeByteString s = maybe False (== toStrict s) . maybeP P.takeByteString $ s

takeLazyByteString :: L.ByteString -> Bool
takeLazyByteString s = maybe False (== s) . maybeP P.takeLazyByteString $ s

takeWhile1 :: Word8 -> L.ByteString -> Bool
takeWhile1 w s =
    let s'    = L.cons w s
        (h,t) = L.span (<=w) s'
    in case PL.parse (P.takeWhile1 (<=w)) s' of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeTill :: Word8 -> L.ByteString -> Bool
takeTill w s =
    let (h,t) = L.break (==w) s
    in case PL.parse (P.takeTill (==w)) s of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeWhile1_empty :: Bool
takeWhile1_empty = maybeP (P.takeWhile1 undefined) L.empty == Nothing

endOfInput :: L.ByteString -> Bool
endOfInput s = maybeP P.endOfInput s == if L.null s
                                        then Just ()
                                        else Nothing

endOfLine :: L.ByteString -> Bool
endOfLine s =
  case (maybeP P8.endOfLine s, L8.uncons s) of
    (Nothing, mcs) -> maybe True (not . eol) mcs
    (Just _,  mcs) -> maybe False eol mcs
  where eol (c,s') = c == '\n' || (c, fst <$> L8.uncons s') == ('\r', Just '\n')

scan :: L.ByteString -> Positive Int64 -> Bool
scan s (Positive k) = maybeP p s == (Just $ toStrict $ L.take k s)
  where p = P.scan k $ \ n _ ->
            if n > 0 then let !n' = n - 1 in Just n' else Nothing

tests = [
      testProperty "anyWord8" anyWord8
    , testProperty "endOfInput" endOfInput
    , testProperty "endOfLine" endOfLine
    , testProperty "notWord8" notWord8
    , testProperty "peekWord8" peekWord8
    , testProperty "peekWord8'" peekWord8'
    , testProperty "satisfy" satisfy
    , testProperty "satisfyWith" satisfyWith
    , testProperty "scan" scan
    , testProperty "skip" skip
    , testProperty "skipWhile" skipWhile
    , testProperty "string" string
    , testProperty "take" take
    , testProperty "takeByteString" takeByteString
    , testProperty "takeCount" takeCount
    , testProperty "takeLazyByteString" takeLazyByteString
    , testProperty "takeTill" takeTill
    , testProperty "takeWhile" takeWhile
    , testProperty "takeWhile1" takeWhile1
    , testProperty "takeWhile1_empty" takeWhile1_empty
    , testProperty "word8" word8
  ]
