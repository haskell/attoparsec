module QC.Text.FastSet where

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.Text.FastSet as FastSet

membershipCorrect :: String -> String -> Property
membershipCorrect members others =
    let fs = FastSet.fromList members
        correct c = (c `FastSet.member` fs) == (c `elem` members)
    in property $ all correct (members ++ others)

tests :: [Test]
tests = [ testProperty "membership is correct" membershipCorrect ]
