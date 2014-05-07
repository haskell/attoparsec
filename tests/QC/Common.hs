{-# OPTIONS_GHC -fno-warn-orphans #-}
module QC.Common
    (
      parseBS
    , parseT
    , toLazyBS
    , toStrictBS
    ) where

import Control.Applicative ((<$>))
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
    arbitrary   = B.pack <$> arbitrary

instance Arbitrary BL.ByteString where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((BL.fromChunks . map (B.pack . nonEmpty)) <$> arbitrary)
      where nonEmpty (NonEmpty a) = a

instance Arbitrary T.Text where
    arbitrary   = T.pack <$> arbitrary

instance Arbitrary TL.Text where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((TL.fromChunks . map (T.pack . nonEmpty)) <$> arbitrary)
      where nonEmpty (NonEmpty a) = a
