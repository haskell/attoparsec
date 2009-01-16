{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module QCSupport
    (
      NonEmpty(..)
    , limCheck
    , maybeP
    ) where

import Control.Applicative ((<$>))
import Data.Attoparsec
import Data.Word (Word8)
-- import Debug.Trace
import System.Random (RandomGen, Random(..))
import Test.QuickCheck
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
    coarbitrary = coarbitrary . nonEmpty

instance Arbitrary S.ByteString where
    arbitrary   = S.pack <$> arbitrary
    coarbitrary = coarbitrary . S.unpack

instance Arbitrary (NonEmpty S.ByteString) where
    arbitrary   = fmap S.pack <$> arbitrary
    coarbitrary = coarbitrary . S.unpack . nonEmpty

instance Arbitrary L.ByteString where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  ((L.fromChunks . map nonEmpty) <$> arbitrary)
    coarbitrary = coarbitrary . L.unpack

instance Arbitrary (NonEmpty L.ByteString) where
    arbitrary   = sized $ \n -> resize (round (sqrt (toEnum n :: Double)))
                  (fmap (L.fromChunks . map nonEmpty) <$> arbitrary)
    coarbitrary = coarbitrary . L.unpack . nonEmpty

instance Random Word8 where
    randomR = integralRandomR
    random  = randomR (minBound,maxBound)

instance Arbitrary Word8 where
    arbitrary     = choose (minBound, maxBound)
    coarbitrary _ = variant 0

instance Arbitrary Char where
    arbitrary     = choose (minBound, maxBound)
    coarbitrary _ = variant 0

maybeP :: Parser a -> L.ByteString -> Maybe a
maybeP p s = case parse p s of
               (_, Left _err) -> Nothing
               (_, Right a)   -> Just a

limCheck :: Testable a => Int -> a -> IO ()
limCheck limit = check defaultConfig {
                   configMaxTest = limit
                 , configEvery = \_ _ -> ""
                 }
