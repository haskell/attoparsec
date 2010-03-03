{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Main (main) where

import qualified Data.ByteString as B
import Control.Applicative
import Control.Monad
import System.IO
import Control.Exception hiding (try)
import System.Environment (getArgs)
import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (many, token, (<|>))

token :: Stream s m Char => ParsecT s u m Char
token = satisfy $ \c -> not (elem c (['\0'..'\31'] ++ "()<>@,;:\\\"/[]?={} \t" ++ ['\128'..'\255']))

isHorizontalSpace c = c == ' ' || c == '\t'

skipHSpaces :: Stream s m Char => ParsecT s u m ()
{-# SPECIALISE skipHSpaces :: Parser () #-}
skipHSpaces = skipMany1 (satisfy isHorizontalSpace)

data Request = Request {
      requestMethod   :: String
    , requestUri      :: String
    , requestProtocol :: String
    } deriving (Eq, Ord, Show)

requestLine :: Stream s m Char => ParsecT s u m Request
{-# SPECIALISE requestLine :: Parser Request #-}
requestLine = do
  method <- many1 token <* skipHSpaces
  uri <- many1 (satisfy (not . isHorizontalSpace)) <* skipHSpaces <* string "HTTP/"
  proto <- many httpVersion <* endOfLine
  return $! Request method uri proto
 where
  httpVersion = satisfy $ \c -> c == '1' || c == '0' || c == '.'

endOfLine :: Stream s m Char => ParsecT s u m ()
{-# SPECIALISE endOfLine :: Parser () #-}
endOfLine = (string "\r\n" *> pure ()) <|> (char '\n' *> pure ())

data Header = Header {
      headerName  :: String
    , headerValue :: [String]
    } deriving (Eq, Ord, Show)

messageHeader :: Stream s m Char => ParsecT s u m Header
{-# SPECIALISE messageHeader :: Parser Header #-}
messageHeader = do
  header <- many1 token <* char ':' <* skipHSpaces
  body <- manyTill anyChar (try endOfLine)
  conts <- many $ skipHSpaces *> manyTill anyChar (try endOfLine)
  return $! Header header (body:conts)

request :: Stream s m Char => ParsecT s u m (Request, [Header])
{-# SPECIALISE request :: Parser (Request, [Header]) #-}
request = (,) <$> requestLine <*> many messageHeader <* endOfLine

main = mapM_ chunky =<< getArgs

listy arg = do
  r <- parseFromFile (many request) arg
  case r of
    Left err -> putStrLn $ arg ++ ": " ++ show err
    Right rs -> print (length rs)

chunky arg = bracket (openFile arg ReadMode) hClose $ \h ->
               loop 0 =<< B.hGetContents h
 where
  loop !n bs
      | B.null bs = print n
      | otherwise = case parse myReq arg bs of
                      Left err -> putStrLn $ arg ++ ": " ++ show err
                      Right (r,bs') -> loop (n+1) bs'
  myReq :: Parser ((Request, [Header]), B.ByteString)
  myReq = liftA2 (,) request getInput
