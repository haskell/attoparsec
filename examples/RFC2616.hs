{-# LANGUAGE OverloadedStrings #-}
module RFC2616 where

import Data.ParserCombinators.Attoparsec.Incremental.Char8
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 ()
import Control.Applicative
import Data.Char
import Control.Monad
import Prelude hiding (takeWhile)

date = rfc1123Date -- <|> rfc850Date <|> asctimeDate

oneOf :: Alternative f => [f a] -> f a
oneOf = foldr (<|>) empty

rfc1123Date =
    liftA3 (,,) (wkday <* string ", ") (date <* char ' ') (time <* string " GMT") <?> "RFC1123 date"
  where wkday = oneWord "Mon Tue Wed Thu Fri Sat Sun"
        oneWord = oneOf . map string . L.words
        date = liftA3 (,,) (d2 <* char ' ') (month <* char ' ') d4
        month = oneWord "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"
        d2 = replicateM 2 (satisfy isDigit)
        d4 = replicateM 4 (satisfy isDigit)

time = liftA3 (,,) (d <* c) (d <* c) d <?> "time"
    where d = replicateM 2 (satisfy isDigit)
          c = char ':'

eol = (char '\n' *> pure Nothing) <|> (string "\r\n" *> pure Nothing)

header =
    (,) <$> (takeWhile fieldChar <* char ':' <* skipWhile space)
        <*> ((:) <$> tillEOL <*> many cont)
    where tillEOL = takeTill (\c -> c == '\r' || c == '\n') <* eol
          fieldChar c = c /= ':' && c >= '!' && c <= '~' 
          cont = some (satisfy space) *> tillEOL
          space c = c == ' ' || c == '\t'
