{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-orphans #-}
module HeadersByteString.Atto
    (
      request
    , response
    ) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Network.HTTP.Types.Version (HttpVersion, http11)
import qualified Data.Attoparsec.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as B

instance NFData HttpVersion where
    rnf !_ = ()

header = do
  name <- B.takeWhile1 (B.inClass "a-zA-Z0-9_-") <* B.char ':' <* B.skipSpace
  body <- bodyLine
  return (name, body)

bodyLine = B.takeTill (\c -> c == '\r' || c == '\n') <* B.endOfLine

requestLine = do
  m <- (B.takeTill B.isSpace <* B.char ' ')
  (p,q) <- B.break (=='?') <$> (B.takeTill B.isSpace <* B.char ' ')
  v <- httpVersion
  return (m,p,q,v)

httpVersion = http11 <$ "HTTP/1.1"

responseLine = (,,) <$>
               (httpVersion <* B.skipSpace) <*>
               (int <* B.skipSpace) <*>
               bodyLine

int :: B.Parser Int
int = B.decimal

request = (,) <$> (requestLine <* B.endOfLine) <*> manyheader

response = (,) <$> responseLine <*> many header

manyheader = do
  c <- B.peekChar'
  if c == '\r' || c == '\n'
    then return []
    else (:) <$> header <*> manyheader
