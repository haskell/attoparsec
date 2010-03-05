{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module QCSupport
    (
      NonEmpty(..)
    ) where

import Control.Applicative
import Data.Attoparsec
import Data.Word (Word8)
import System.Random (RandomGen, Random(..))
import Test.QuickCheck hiding (NonEmpty)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR (a,b) g = case randomR (fromIntegral a :: Integer,
                                        fromIntegral b :: Integer) g of
                            (x,g') -> (fromIntegral x, g')

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

instance Random Word8 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary     = choose (minBound, maxBound)
