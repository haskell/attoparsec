{-# LANGUAGE OverloadedStrings #-}

module RFC2616
    (
      Request(..)
    , Header(..)
    , isToken
    , requestLine
    , messageHeader
    , request
    ) where

import Control.Applicative hiding (many)
import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine, isEndOfLine, isHorizontalSpace)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B

isToken :: Word8 -> Bool
isToken w = notInClass "\0-\31()<>@,;:\\\"/[]?={} \t\128-\255" w

skipHSpaces :: Parser ()
skipHSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

data Request = Request {
      requestMethod   :: !B.ByteString
    , requestUri      :: !B.ByteString
    , requestProtocol :: !B.ByteString
    } deriving (Eq, Ord, Show)

requestLine :: Parser Request
requestLine = do
  method <- P.takeWhile1 isToken <* skipHSpaces
  uri <- P.takeWhile1 (not . isHorizontalSpace) <* skipHSpaces <* string "HTTP/"
  proto <- P.takeWhile1 isDigitOrDot <* endOfLine
  return $! Request method uri proto
 where
  isDigitOrDot w = (w >= 48 && w <= 57) || w == 46

data Header = Header {
      headerName  :: !B.ByteString
    , headerValue :: [B.ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = do
  header <- P.takeWhile1 isToken <* char8 ':' <* skipHSpaces
  body <- takeTill isEndOfLine <* endOfLine
  conts <- many $ skipHSpaces *> takeTill isEndOfLine <* endOfLine
  return $! Header header (body:conts)

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine
