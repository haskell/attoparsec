{-# OPTIONS_GHC -fno-warn-orphans #-}
module QC.Common () where

import Control.Applicative ((<$>))
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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
