{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Criterion.Main (bench, bgroup, defaultMain, nf, whnf)
import Data.Char
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import Control.DeepSeq (NFData(rnf))
import Data.ByteString.Internal (ByteString(..))

instance NFData ByteString where
    rnf (PS _ _ _) = ()

instance NFData P.ParseError where
    rnf = rnf . show

main = do
  let s  = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      !b = BC.pack s
      !t = T.pack s
  defaultMain [
     bgroup "comparison" [
       bgroup "parsec-vs-atto" [
         bench "attoparsec B" $ whnf (AB.parse (many (AC.satisfy AC.isAlpha_ascii))) b
       , bench "attoparsec T" $ whnf (AT.parse (many (AT.satisfy AC.isAlpha_ascii))) t
       , bench "parsec B" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") b
       , bench "parsec S" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") s
       , bench "parsec T" $ whnf (P.parse (many (P.satisfy AC.isAlpha_ascii)) "") t
       ]
     , bgroup "many-vs-takeWhile" [
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
