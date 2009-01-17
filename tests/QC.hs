module Main (main) where

import Control.Applicative
import Data.Attoparsec as P
import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Control.Monad (forM_)
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

prop_string s = maybeP (string s) s == Just s

prop_skipWhile (w,s) =
    let (h,t) = L.span (==w) s
    in case parse (skipWhile (==w)) s of
         (t',Right ()) -> t == t'
         _ -> False

prop_takeCount (k,s) =
    k >= 0 ==>
    case maybeP (takeCount k) s of
      Nothing -> j > L.length s
      Just s' -> j <= L.length s
  where j = fromIntegral k

prop_takeWhile (w,s) =
    let (h,t) = L.span (==w) s
    in case parse (P.takeWhile (==w)) s of
         (t',Right h') -> t == t' && h == h'
         _ -> False

prop_takeWhile1 (w, NonEmpty s) =
    let (h,t) = L.span (==w) s
    in case parse (P.takeWhile (==w)) s of
         (t',Right h') -> t == t' && h == h'
         _ -> False

prop_takeTill (w, s) =
    let (h,t) = L.break (==w) s
    in case parse (P.takeTill (==w)) s of
         (t',Right h') -> t == t' && h == h'
         _ -> False

prop_takeWhile1_empty = maybeP (P.takeWhile1 undefined) L.empty == Nothing

prop_notEmpty_string s = case maybeP (notEmpty (string s)) s of
                           Nothing -> L.null s
                           Just s' -> not (L.null s) && s == s'

main :: IO ()
main = do
  args <- getArgs
  let n = case args of
            []  -> 500
            [k] -> read k
            _   -> error "too many arguments"
  forM_ tests $ \(name,prop) -> do
      putStr name
      putStr (replicate (40 - length name) ' ')
      hFlush stdout
      prop n

p :: Testable a => a -> Int -> IO ()
p prop count = limCheck count prop

tests :: [(String, Int -> IO ())]
tests = [
  ("nonEmptyList", p prop_nonEmptyList),
  ("nonEmptyBS", p prop_nonEmptyBS),
  ("nonEmptyLBS", p prop_nonEmptyLBS),
  ("word8", p prop_word8),
  ("notWord8", p prop_notWord8),
  ("anyWord8", p prop_anyWord8),
  ("string", p prop_string),
  ("skipWhile", p prop_skipWhile),
  ("takeCount", p prop_takeCount),
  ("takeWhile", p prop_takeWhile),
  ("takeWhile1", p prop_takeWhile1),
  ("takeWhile1_empty", p prop_takeWhile1_empty),
  ("takeTill", p prop_takeTill),
  ("notEmpty_string", p prop_notEmpty_string)
  ]
