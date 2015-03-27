{-# LANGUAGE OverloadedStrings #-}

module Genome
    (
      genome
    ) where

import Control.Applicative
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Lazy as L
import Common (rechunkBS)

genome :: Benchmark
genome = bgroup "genome" [
    bench "search/B" $ nf (map (parse searchBS)) (B8.tails geneB)
  , bench "search/BL" $ nf (map (L.parse searchBS)) (L8.tails geneBL)
  ]
  where geneB  = B8.pack gene
        geneBL = rechunkBS 4 geneB

searchBS :: Parser ByteString
searchBS = "caac" *> ("aaca" <|> "aact")

-- Dictyostelium discoideum developmental protein DG1094 (gacT) gene,
-- partial cds. http://www.ncbi.nlm.nih.gov/nuccore/AF081586.1

gene :: String
gene = "atcgatttagaaagatacaaagatagaaccatcaataataaacaagagaagagagcaagt\
       \agagatattaataaagagattgaaagagagattgaaaagaagagattatcaccaagagaa\
       \agattaaatttatttggtctttcttcctcatcttcatcagtgaattcaacattaacaaga\
       \tctacagcaaatattatctctacaatagacggtagtggaggtagtaatcgtaatagtaaa\
       \aattatggtaatggctcatcctcctcctcaaatagaagatatagtaatactattaatcaa\
       \caattacaaatgcaattacaacaacttcaaatccaacaacaacaatatcaacaaactcaa\
       \caatctcaaataccattacaatatcaacaacaacaacagcaacaacaacaacaaaccact\
       \acaactacaactacatcaagtggtagtaatagattctcttcaaatagatataaaccagtt\
       \gatcttacacaatcatcttcaaactttcgttattcacgtgaaatttatgatgatgattat\
       \tattcaaataataatttaatgatgtttggtaatgagcaaccaaatcaaacaccaatttct\
       \gtatcatcttcatctgcattcacacgtcaaagatctcaaagttgctttgaaccagagaat\
       \cttgtattgctacaacaacaatatcaacaatatcaacaacaacaacaacaacaacaacaa\
       \attccattccaagcaaatccacaatatagtaatgctgttattgaacaaaaattggatcaa\
       \attagagataccattaataatttacatagagataaccgagtctctaga"
