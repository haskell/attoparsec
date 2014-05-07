{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-warnings-deprecations #-}
module QC.Text (tests) where

import Control.Applicative ((<$>), (<*>))
import Data.Attoparsec.Text (Parser)
import Prelude hiding (take, takeWhile)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Text.Lazy as PL
import qualified Data.Char as Char
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
label (NonEmpty s) = case parse (anyChar <?> s) T.empty of
                            (_, Left err) -> s `isInfixOf` err
                            _             -> False
-}

-- Basic byte-level combinators.

maybeP :: Parser r -> L.Text -> Maybe r
maybeP p = PL.maybeResult . PL.parse p

satisfy :: Char -> L.Text -> Bool
satisfy w s = maybeP (P.satisfy (<=w)) (L.cons w s) == Just w

char :: Char -> L.Text -> Bool
char w s = maybeP (P.char w) (L.cons w s) == Just w

anyChar :: L.Text -> Bool
anyChar s
    | L.null s  = p == Nothing
    | otherwise = p == Just (L.head s)
  where p = maybeP P.anyChar s

notChar :: Char -> NonEmptyList Char -> Bool
notChar w (NonEmpty s) = maybeP (P.notChar w) bs == if v == w
                                                      then Nothing
                                                      else Just v
    where v = L.head bs
          bs = L.pack s

peekChar :: L.Text -> Bool
peekChar s
    | L.null s  = p == Just (Nothing, s)
    | otherwise = p == Just (Just (L.head s), s)
  where p = maybeP ((,) <$> P.peekChar <*> P.takeLazyText) s

string
  :: L.Text
     -> L.Text -> Bool
string s t = maybeP (P.string s') (s `L.append` t) == Just s'
  where s' = toStrict s

stringCI :: T.Text -> Bool
stringCI s = P.parseOnly (P.stringCI fs) s == Right s
  where fs = T.toCaseFold s

asciiCI :: T.Text -> Gen Bool
asciiCI x =
  (\s i -> P.parseOnly (P.asciiCI s) i == Right i)
    <$> maybeModifyCase x
    <*> maybeModifyCase x
  where
    maybeModifyCase s = elements [s, toLower s, toUpper s]
    toLower = T.map (\c -> if c < Char.chr 127 then Char.toLower c else c)
    toUpper = T.map (\c -> if c < Char.chr 127 then Char.toUpper c else c)

toStrict :: L.Text -> T.Text
toStrict = T.concat . L.toChunks

skipWhile :: Char -> L.Text -> Bool
skipWhile w s =
    let t = L.dropWhile (<=w) s
    in case PL.parse (P.skipWhile (<=w)) s of
         PL.Done t' () -> t == t'
         _             -> False

takeCount :: Positive Int -> L.Text -> Bool
takeCount (Positive k) s =
    case maybeP (P.take k) s of
      Nothing -> fromIntegral k > L.length s
      Just _s -> fromIntegral k <= L.length s

takeWhile :: Char -> L.Text -> Bool
takeWhile w s =
    let (h,t) = L.span (==w) s
    in case PL.parse (P.takeWhile (==w)) s of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeWhile1 :: Char -> L.Text -> Bool
takeWhile1 w s =
    let s'    = L.cons w s
        (h,t) = L.span (<=w) s'
    in case PL.parse (P.takeWhile1 (<=w)) s' of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeTill :: Char -> L.Text -> Bool
takeTill w s =
    let (h,t) = L.break (==w) s
    in case PL.parse (P.takeTill (==w)) s of
         PL.Done t' h' -> t == t' && toStrict h == h'
         _             -> False

takeWhile1_empty :: Bool
takeWhile1_empty = maybeP (P.takeWhile1 undefined) L.empty == Nothing

endOfInput :: L.Text -> Bool
endOfInput s = maybeP P.endOfInput s == if L.null s
                                        then Just ()
                                        else Nothing

tests :: [Test]
tests = [
      testProperty "anyChar" anyChar
    , testProperty "asciiCI" asciiCI
    , testProperty "char" char
    , testProperty "endOfInput" endOfInput
    -- , testProperty "endOfLine" endOfLine
    , testProperty "notChar" notChar
    , testProperty "peekChar" peekChar
    -- , testProperty "peekChar'" peekChar'
    , testProperty "satisfy" satisfy
    -- , testProperty "satisfyWith" satisfyWith
    -- , testProperty "scan" scan
    -- , testProperty "skip" skip
    , testProperty "skipWhile" skipWhile
    , testProperty "string" string
    , testProperty "stringCI" stringCI
    -- , testProperty "take" take
    -- , testProperty "takeText" takeText
    , testProperty "takeCount" takeCount
    -- , testProperty "takeLazyText" takeLazyText
    , testProperty "takeTill" takeTill
    , testProperty "takeWhile" takeWhile
    , testProperty "takeWhile1" takeWhile1
    , testProperty "takeWhile1_empty" takeWhile1_empty
  ]
