module Main (main) where

import Control.Applicative
import Data.Attoparsec
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Control.Monad
import Debug.Trace
import System.IO
import Test.QuickCheck
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import System.Environment
import QCSupport

-- Make sure that structures whose types claim they are non-empty
-- really are.

prop_nonEmptyList l = length (nonEmpty l) > 0
    where types = l :: NonEmpty [Int]
prop_nonEmptyBS l = S.length (nonEmpty l) > 0
prop_nonEmptyLBS l = L.length (nonEmpty l) > 0

-- Naming.

prop_label (NonEmpty s) = case parse (anyWord8 <?> s) L.empty of
                            (_, Left err) -> s `isInfixOf` err
                            _             -> False

-- Basic byte-level combinators.

prop_word8 (NonEmpty s) = maybeP (word8 w) s == Just w
    where w = L.head s

prop_anyWord8 (NonEmpty s) = isJust $ maybeP anyWord8 s

prop_notWord8 (w, NonEmpty s) = v /= w ==> maybeP (notWord8 w) s == Just v
    where v = L.head s

--p :: Testable a => a -> Int -> IO ()
p prop count = limCheck count prop

main = do
  args <- getArgs
  let n = case args of
            []  -> 100
            [k] -> read k
            _   -> error "too many arguments"
  forM_ tests $ \(name,prop) -> do
      putStr name
      putStr (replicate (40 - length name) ' ')
      hFlush stdout
      prop n

tests = [
  ("nonEmptyList", p prop_nonEmptyList),
  ("nonEmptyBS", p prop_nonEmptyBS),
  ("nonEmptyLBS", p prop_nonEmptyLBS),
  ("word8", p prop_word8),
  ("notWord8", p prop_notWord8),
  ("anyWord8", p prop_anyWord8)
  ]
