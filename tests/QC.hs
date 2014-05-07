{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import qualified QC.Text as Text
import Test.Framework (defaultMain, testGroup)

main = defaultMain tests

tests = [
    testGroup "bs" ByteString.tests
  , testGroup "combinator" Combinator.tests
  , testGroup "text" Text.tests
  ]
