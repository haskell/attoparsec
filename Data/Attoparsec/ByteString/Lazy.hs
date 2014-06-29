-- |
-- Module      :  Data.Attoparsec.ByteString.Lazy
-- Copyright   :  Bryan O'Sullivan 2007-2014
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing that can consume lazy
-- 'ByteString' strings, loosely based on the Parsec library.
--
-- This is essentially the same code as in the 'Data.Attoparsec'
-- module, only with a 'parse' function that can consume a lazy
-- 'ByteString' incrementally, and a 'Result' type that does not allow
-- more input to be fed in.  Think of this as suitable for use with a
-- lazily read file, e.g. via 'L.readFile' or 'L.hGetContents'.
--
-- /Note:/ The various parser functions and combinators such as
-- 'string' still expect /strict/ 'B.ByteString' parameters, and
-- return strict 'B.ByteString' results.  Behind the scenes, strict
-- 'B.ByteString' values are still used internally to store parser
-- input and manipulate it efficiently.

module Data.Attoparsec.ByteString.Lazy
    (
      Result(..)
    , module Data.Attoparsec.ByteString
    -- * Running parsers
    , parse
    , parseTest
    -- ** Result conversion
    , maybeResult
    , eitherResult
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.ByteString.Lazy.Internal (ByteString(..), chunk)
import Data.List (intercalate)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.ByteString
    hiding (IResult(..), Result, eitherResult, maybeResult,
            parse, parseWith, parseTest)

-- | The result of a parse.
data Result r = Fail ByteString [String] String
              -- ^ The parse failed.  The 'ByteString' is the input
              -- that had not yet been consumed when the failure
              -- occurred.  The @[@'String'@]@ is a list of contexts
              -- in which the error occurred.  The 'String' is the
              -- message describing the error, if any.
              | Done ByteString r
              -- ^ The parse succeeded.  The 'ByteString' is the
              -- input that had not yet been consumed (if any) when
              -- the parse succeeded.

instance NFData r => NFData (Result r) where
    rnf (Fail bs ctxs msg) = rnfBS bs `seq` rnf ctxs `seq` rnf msg
    rnf (Done bs r)        = rnfBS bs `seq` rnf r
    {-# INLINE rnf #-}

rnfBS :: ByteString -> ()
rnfBS (Chunk _ xs) = rnfBS xs
rnfBS Empty        = ()
{-# INLINE rnfBS #-}

instance Show r => Show (Result r) where
    show (Fail bs stk msg) =
        "Fail " ++ show bs ++ " " ++ show stk ++ " " ++ show msg
    show (Done bs r)       = "Done " ++ show bs ++ " " ++ show r

fmapR :: (a -> b) -> Result a -> Result b
fmapR _ (Fail st stk msg) = Fail st stk msg
fmapR f (Done bs r)       = Done bs (f r)

instance Functor Result where
    fmap = fmapR

-- | Run a parser and return its result.
parse :: A.Parser a -> ByteString -> Result a
parse p s = case s of
              Chunk x xs -> go (A.parse p x) xs
              empty      -> go (A.parse p B.empty) empty
  where
    go (T.Fail x stk msg) ys      = Fail (chunk x ys) stk msg
    go (T.Done x r) ys            = Done (chunk x ys) r
    go (T.Partial k) (Chunk y ys) = go (k y) ys
    go (T.Partial k) empty        = go (k B.empty) empty

-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => A.Parser a -> ByteString -> IO ()
parseTest p s = print (parse p s)

-- | Convert a 'Result' value to a 'Maybe' value.
maybeResult :: Result r -> Maybe r
maybeResult (Done _ r) = Just r
maybeResult _          = Nothing

-- | Convert a 'Result' value to an 'Either' value.
eitherResult :: Result r -> Either String r
eitherResult (Done _ r)     = Right r
eitherResult (Fail input contexts msg) =
    Left . intercalate "\n" $
        contexts ++ [msg, "attoparsec failed on input:", unpack input, ""]
