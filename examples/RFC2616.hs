{-# LANGUAGE OverloadedStrings #-}
module RFC2616 where

import Data.Attoparsec.Incremental.Char8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 ()
import Control.Applicative
import Data.Char
import Control.Monad
import Prelude hiding (takeWhile)
import Data.Time.Clock
import Data.Time.Format
import System.Locale

date = rfc1123Date -- <|> rfc850Date <|> asctimeDate

fallible :: Parser r (Maybe a) -> Parser r a
fallible p = maybe mzero return =<< p

rfc1123Date :: Parser r UTCTime
rfc1123Date =
  fallible (q <$> (manyTill anyChar (string " GMT")))
    <?> "RFC1123 date"
  where q = parseTime defaultTimeLocale "%a, %d %b %Y %T"

time = liftA3 (,,) (d <* c) (d <* c) d <?> "time"
    where d = replicateM 2 (satisfy isDigit)
          c = char ':'

eol = (char '\n' *> pure Nothing) <|> (string "\r\n" *> pure Nothing)

header =
    (,) <$> (takeWhile fieldChar <* char ':' <* skipWhile space)
        <*> ((:) <$> tillEOL <*> many cont)
    where tillEOL = takeTill newline <* eol
          newline c = c == '\r' || c == '\n'
          fieldChar c = c /= ':' && c >= '!' && c <= '~' 
          cont = some (satisfy space) *> tillEOL
          space c = c == ' ' || c == '\t'
