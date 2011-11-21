{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module QCSupport
    (
      NonEmpty(..)
    ) where

import Control.Applicative
import Data.Attoparsec
import Data.Word (Word8)
import Test.QuickCheck hiding (NonEmpty)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

newtype NonEmpty a = NonEmpty { nonEmpty :: a }
    deriving (Eq, Ord, Read, Show)

instance Functor NonEmpty where
    fmap f (NonEmpty a) = NonEmpty (f a)

instance Applicative NonEmpty where
    NonEmpty f <*> NonEmpty a = NonEmpty (f a)
    pure a                    = NonEmpty a

instance Arbitrary a => Arbitrary (NonEmpty [a]) where
    arbitrary   = NonEmpty <$> sized (\n -> choose (1,n+1) >>= vector)

instance Arbitrary S.ByteString where
    arbitrary   = S.pack <$> arbitrary

instance Arbitrary (NonEmpty S.ByteString) where
    arbitrary   = fmap S.pack <$> arbitrary

instance Arbitrary L.ByteString where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((L.fromChunks . map nonEmpty) <$> arbitrary)

instance Arbitrary (NonEmpty L.ByteString) where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  (fmap (L.fromChunks . map nonEmpty) <$> arbitrary)
