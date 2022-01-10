{-# LANGUAGE OverloadedStrings #-}

module Warp (benchmarks) where

import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)
import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp.ReadInt (readInt)
import qualified Data.Attoparsec.ByteString.Char8 as B

benchmarks :: Benchmark
benchmarks = bgroup "warp" [
    bgroup "decimal" [
      bench "warp" $ nf (readInt :: ByteString -> Int) "31337"
    , bench "atto" $ nf (B.parse (B.decimal :: B.Parser Int)) "31337"
    ]
  ]
