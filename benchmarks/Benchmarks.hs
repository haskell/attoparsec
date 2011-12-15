{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Data.ByteString.Internal (ByteString(..))
import Data.Char
import Text.Parsec.Text ()
import Text.Parsec.Text.Lazy ()
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.Parsec as P

instance NFData ByteString where
    rnf (PS _ _ _) = ()

instance NFData P.ParseError where
    rnf = rnf . show

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where go xs = case splitAt k xs of
                  ([],_)  -> []
                  (y, ys) -> y : go ys

fromLazy :: BL.ByteString -> B.ByteString
fromLazy = B.concat . BL.toChunks

main = do
  let s  = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      !b = BC.pack s
      !bl = BL.fromChunks . map BC.pack . chunksOf 4 $ s
      !t = T.pack s
      !tl = TL.fromChunks . map T.pack . chunksOf 4 $ s
      s1 = take 1024 $ cycle ['a'..'z']
      !b1 = BC.pack s1
      !t1 = T.pack s1
      !bl1 = BL.fromChunks . map BC.pack . chunksOf 4 $ s1
  defaultMain [
     bgroup "many" [
       bgroup "attoparsec" [
         bench "B" $ whnf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
       , bench "BL" $ whnf (ABL.parse (many (AC.satisfy AC.isAlpha_ascii))) bl
       , bench "T" $ whnf (AT.parse (many (AT.satisfy AC.isAlpha_ascii))) t
       , bench "TL" $ whnf (ATL.parse (many (AT.satisfy AC.isAlpha_ascii))) tl
       ]
     , bgroup "parsec" [
         bench "S" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") s
       , bench "B" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") b
       , bench "BL" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") bl
       , bench "T" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") t
       , bench "TL" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") tl
       ]
     ]
   , bgroup "comparison" [
       bgroup "many-vs-takeWhile" [
         bench "many" $ whnf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
       , bench "takeWhile" $ whnf (AB.parse (AC.takeWhile AC.isAlpha_ascii)) b
       ]
     , bgroup "letter-vs-many" [
         bench "letter" $ whnf (AB.parse (many AC.letter_ascii)) b
       , bench "many" $ whnf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
       ]
     ]
   , bgroup "takeWhile" [
       bench "isAlpha" $ whnf (AB.parse (AC.takeWhile isAlpha)) b
     , bench "isAlpha_ascii" $ whnf (AB.parse (AC.takeWhile AC.isAlpha_ascii)) b
     , bench "isAlpha_iso8859_15" $ whnf (AB.parse (AC.takeWhile AC.isAlpha_iso8859_15)) b
     ]
   ]
