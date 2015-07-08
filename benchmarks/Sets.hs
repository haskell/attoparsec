module Sets (benchmarks) where

import Criterion
import Data.Char (ord)
import qualified Data.Attoparsec.Text.FastSet as FastSet
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet

smallSet :: String
smallSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

benchmarks :: Benchmark
benchmarks = bgroup "sets" [
    bench "Fast" $ whnf (FastSet.member '*') (FastSet.fromList smallSet)
  , bench "HashSet" $ whnf (HashSet.member '*') (HashSet.fromList smallSet)
  , bench "IntSet" $ whnf (IntSet.member (ord '*'))
                     (IntSet.fromList (map ord smallSet))
  ]
