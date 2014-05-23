{-# LANGUAGE OverloadedStrings #-}

module RFC2616
    (
      Header(..)
    , Request(..)
    , Response(..)
    , request
    , response
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import Data.Attoparsec.ByteString.Char8 (char8, endOfLine, isDigit_w8)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy P8.isHorizontalSpace *> skipWhile P8.isHorizontalSpace

data Request = Request {
      requestMethod  :: B.ByteString
    , requestUri     :: B.ByteString
    , requestVersion :: B.ByteString
    } deriving (Eq, Ord, Show)

httpVersion :: Parser B.ByteString
httpVersion = "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

requestLine :: Parser Request
requestLine = Request <$> (P.takeWhile1 isToken <* char8 ' ')
                      <*> (P.takeWhile1 (/=32) <* char8 ' ')
                      <*> (httpVersion <* endOfLine)

data Header = Header {
      headerName  :: B.ByteString
    , headerValue :: [B.ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = Header
  <$> (P.takeWhile isToken <* char8 ':' <* skipWhile P8.isHorizontalSpace)
  <*> ((:) <$> (takeTill P8.isEndOfLine <* endOfLine)
           <*> (many $ skipSpaces *> takeTill P8.isEndOfLine <* endOfLine))

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

data Response = Response {
      responseVersion :: B.ByteString
    , responseCode    :: B.ByteString
    , responseMsg     :: B.ByteString
    } deriving (Eq, Ord, Show)

responseLine :: Parser Response
responseLine = Response <$> (httpVersion <* char8 ' ')
                        <*> (P.takeWhile isDigit_w8 <* char8 ' ')
                        <*> (P.takeTill P8.isEndOfLine <* endOfLine)

response :: Parser (Response, [Header])
response = (,) <$> responseLine <*> many messageHeader <* endOfLine
