module QC.Combinator where

import Control.Applicative
import Data.Word (Word8)
import QC.Common (parseBS, toLazyBS)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as C
import qualified Data.ByteString as B

choice :: NonEmptyList (NonEmptyList Word8) -> Gen Property
choice (NonEmpty xs) = do
  let ys = map (B.pack . getNonEmpty) xs
  return . forAll (toLazyBS <$> elements ys) $
    maybe False (`elem` ys) . parseBS (C.choice (map P.string ys))

tests :: [Test]
tests = [
    testProperty "choice" choice
  ]
