{-# LANGUAGE OverloadedStrings #-}
module QC.Combinator where

import Control.Applicative
import Data.Word (Word8)
import QC.Common (Repack, parseBS, repackBS)
import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

choice :: NonEmptyList (NonEmptyList Word8) -> Gen Property
choice (NonEmpty xs) = do
  let ys = map (B.pack . getNonEmpty) xs
  return . forAll (repackBS <$> arbitrary <*> elements ys) $
    maybe False (`elem` ys) . parseBS (C.choice (map P.string ys))

count :: Positive (Small Int) -> Repack -> B.ByteString -> Bool
count (Positive (Small n)) rs s =
    (length <$> parseBS (C.count n (P.string s)) input) == Just n
  where input = repackBS rs (B.concat (replicate (n+1) s))

match :: Int -> NonNegative Int -> NonNegative Int -> Repack -> Bool
match n (NonNegative x) (NonNegative y) rs =
    parseBS (P.match parser) (repackBS rs input) == Just (input, n)
  where parser = P.skipWhile (=='x') *> P.signed P.decimal <*
                 P.skipWhile (=='y')
        input = B.concat [
            B8.replicate x 'x', B8.pack (show n), B8.replicate y 'y'
          ]

-- make sure <||> does not backtrack:
nonBacktrack :: Positive (Small Int) -> Positive (Small Int) -> Repack -> Bool
nonBacktrack (Positive (Small a)) (Positive (Small b)) rs =
    parseBS (P.match parser) (repackBS rs input) == Nothing
  where parser = (P.skipWhile (=='x') <* P.signed P.decimal) C.<||>
                 P.skipWhile (const True) -- never failing parser on rhs
        input = B.concat [
            B8.replicate a 'x', B8.replicate b 'y'
          ]
-- make sure <||> succeeds if the first parser succeeds:
pickFirst :: Positive (Small Int) -> NonNegative Int -> Repack -> Bool
pickFirst (Positive (Small a)) (NonNegative b) rs =
    parseBS (P.match parser) (repackBS rs input) == Just (input,())
  where parser = (P.skipWhile (=='x') C.<||>
                 (const () <$> P.satisfy (const False))) -- always failing parser on rhs
                 <* P.skipWhile (=='y')
        input = B.concat [
            B8.replicate a 'x', B8.replicate b 'y'
          ]
-- make sure <||> takes the second parser if the first fails without consuming input:
pickLast :: NonNegative Int -> NonNegative Int -> Repack -> Bool
pickLast (NonNegative a) (NonNegative b) rs =
    parseBS (P.match parser) (repackBS rs input) == Just (input,True)
  where parser = ( (const False <$> P.string "xyx" -- the entire string is matched at once!
                   )
                  C.<||>
                   (const True <$> P.skipWhile (=='x'))
                 )
                 <* P.skipWhile (=='y')
        input = B.concat [
            B8.replicate a 'x', B8.replicate b 'y'
          ]

tests :: [Test]
tests = [
    testProperty "choice" choice
  , testProperty "count" count
  , testProperty "match" match
  , testProperty "pickLast" pickLast
  , testProperty "pickFirst" pickLast
  , testProperty "nonBacktrack" nonBacktrack
  ]
