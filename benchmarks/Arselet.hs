module Main (main) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Attoparsec.Char8 hiding (digit, letter)
import qualified Data.Attoparsec.Char8 as A
import System.Environment

letter :: Parser Char
letter = satisfy $ \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

digit :: Parser Char
digit = satisfy $ \c -> c >= '0' && c <= '9'

letters :: Parser B.ByteString
letters = takeWhile1 $ \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') 

digits :: Parser B.ByteString
digits = takeWhile1 $ \c -> c >= '0' && c <= '9'

chunksOf :: Int -> B.ByteString -> [B.ByteString]
chunksOf n = go
  where go s | B.null s  = []
             | otherwise = let (h,t) = B.splitAt n s
                         in h : go t

parseAll :: Parser a -> [B.ByteString] -> Result a
parseAll p ss = case ss of
                  []     -> go (parse p B.empty) []
                  (c:cs) -> go (parse p c) cs
  where go (Partial k) (c:cs) = go (k c) cs
        go (Partial k) []     = k B.empty
        go r           _      = r

main = do
  args <- getArgs
  forM_ args $ \arg -> do
    chunks <- if False
              then L.toChunks `fmap` L.readFile arg
              else do
                content <- B.readFile arg
                return $ if False
                         then [content]
                         else chunksOf 24 content
    let p1 = many (many1 letter `mplus` many1 digit)
        p2 = many (many1 A.letter `mplus` many1 A.digit)
        p3 = many (letters `mplus` digits)
    print (parseAll p3 chunks)
