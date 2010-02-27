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
import Data.Attoparsec.Char8 (char8, endOfLine)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as B

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

skipSpaces :: Parser ()
skipSpaces = satisfy spc *> skipWhile spc
    where spc = inClass " \t"

data Request = Request {
      requestMethod   :: !B.ByteString
    , requestUri      :: !B.ByteString
    , requestProtocol :: !B.ByteString
    } deriving (Eq, Ord, Show)

requestLine :: Parser Request
requestLine = do
  method <- P.takeWhile isToken
  skipSpaces
  uri <- P.takeWhile (notInClass " \t")
  skipSpaces >> string "HTTP/"
  proto <- P.takeWhile (inClass "0-9.")
  endOfLine
  return $! Request method uri proto

data Header = Header {
      headerName  :: !B.ByteString
    , headerValue :: [B.ByteString]
    } deriving (Eq, Ord, Show)

messageHeader :: Parser Header
messageHeader = do
  header <- P.takeWhile isToken
  char8 ':' *> skipSpaces
  body <- takeTill (inClass "\r\n")
  endOfLine
  bodies <- many $ satisfy (inClass " \t") *> skipSpaces *>
                   takeTill (inClass "\r\n") <* endOfLine
  return $! Header header (body:bodies)

request :: Parser (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine
