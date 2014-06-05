module Main (main) where

import Data.Word (Word8)
import qualified Data.Attoparsec.ByteString.FastSet as F
import System.Random (Random(..), RandomGen)
import Test.QuickCheck

integralRandomR :: (Integral a, RandomGen g) => (a, a) -> g -> (a, g)
integralRandomR (a,b) g = case randomR (fromIntegral a :: Int,
                                        fromIntegral b :: Int) g
                          of (x,g') -> (fromIntegral x, g')

prop_AllMembers s =
    let set = F.fromList s
    in all (`F.memberWord8` set) s

main = do
  quickCheck prop_AllMembers
