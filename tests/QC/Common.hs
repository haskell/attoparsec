{-# OPTIONS_GHC -fno-warn-orphans #-}
module QC.Common
    (
      parseBS
    , parseT
    , toLazyBS
    , toStrictBS
    , Repack
    , repackBS
    , repackBS_
    , repackT
    , repackT_
    ) where

import Control.Applicative ((<$>), (<*>))
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Attoparsec.ByteString.Lazy as BL
import qualified Data.Attoparsec.Text.Lazy as TL

parseBS :: BL.Parser r -> BL.ByteString -> Maybe r
parseBS p = BL.maybeResult . BL.parse p

parseT :: TL.Parser r -> TL.Text -> Maybe r
parseT p = TL.maybeResult . TL.parse p

toStrictBS :: BL.ByteString -> B.ByteString
toStrictBS = B.concat . BL.toChunks

toLazyBS :: B.ByteString -> BL.ByteString
toLazyBS = BL.fromChunks . (:[])

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary

instance Arbitrary BL.ByteString where
    arbitrary = repackBS <$> arbitrary <*> arbitrary

type Repack = NonEmptyList (Positive (Small Int))

repackBS :: Repack -> B.ByteString -> BL.ByteString
repackBS (NonEmpty bs) =
    BL.fromChunks . repackBS_ (map (getSmall . getPositive) bs)

repackBS_ :: [Int] -> B.ByteString -> [B.ByteString]
repackBS_ = go . cycle
  where go (b:bs) s
          | B.null s = []
          | otherwise = let (h,t) = B.splitAt b s
                        in h : go bs t
        go _ _ = error "unpossible"

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary TL.Text where
    arbitrary = repackT <$> arbitrary <*> arbitrary

repackT :: Repack -> T.Text -> TL.Text
repackT (NonEmpty bs) =
    TL.fromChunks . repackT_ (map (getSmall . getPositive) bs)

repackT_ :: [Int] -> T.Text -> [T.Text]
repackT_ = go . cycle
  where go (b:bs) s
          | T.null s = []
          | otherwise = let (h,t) = T.splitAt b s
                        in h : go bs t
        go _ _ = error "unpossible"
