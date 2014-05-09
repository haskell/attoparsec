{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common (chunksOf) where

import Control.DeepSeq (NFData(rnf))
import Text.Parsec (ParseError)

#if !MIN_VERSION_bytestring(0,10,0)
import Data.ByteString.Internal (ByteString(..))

instance NFData ByteString where
    rnf (PS _ _ _) = ()
#endif

instance NFData ParseError where
    rnf = rnf . show

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where go xs = case splitAt k xs of
                  ([],_)  -> []
                  (y, ys) -> y : go ys
