{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module QC.Simple (
      tests
    ) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import QC.Rechunked (rechunkBS)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, counterexample, forAll)
import qualified Data.Attoparsec.ByteString.Char8 as A

t_issue75 = expect issue75 "ab" (A.Done "" "b")

issue75 :: A.Parser ByteString
issue75 = "a" >> ("b" <|> "")

expect :: (Show r, Eq r) => A.Parser r -> ByteString -> A.Result r -> Property
expect p input wanted =
  forAll (rechunkBS input) $ \in' ->
    let result = parse p in'
    in counterexample (show result ++ " /= " ++ show wanted) $
       fromMaybe False (A.compareResults result wanted)

parse :: A.Parser r -> [ByteString] -> A.Result r
parse p (x:xs) = foldl' A.feed (A.parse p x) xs
parse p []     = A.parse p ""

tests :: [Test]
tests = [
      testProperty "issue75" t_issue75
  ]
