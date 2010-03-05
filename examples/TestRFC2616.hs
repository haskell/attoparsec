{-# LANGUAGE BangPatterns #-}
import RFC2616
import Control.Monad (forM_)
import System.IO
import Control.Exception (bracket)
import System.Environment
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec

refill h = B.hGet h (4*1024)

listy file h = do
  r <- parseWith (refill h) (many request) =<< refill h
  case r of
    Fail _ _ msg -> hPutStrLn stderr $ file ++ ": " ++ msg
    Done _ reqs  -> print (length reqs)
  
incrementy file h = go 0 =<< refill h
 where
   go !n is = do
     r <- parseWith (refill h) request is
     case r of
       Fail _ _ msg -> hPutStrLn stderr $ file ++ ": " ++ msg
       Done bs _req
           | B.null bs -> do
              s <- refill h
              if B.null s
                then print (n+1)
                else go (n+1) s
           | otherwise -> go (n+1) bs
  
main = do
  args <- getArgs
  forM_ args $ \arg ->
    bracket (openFile arg ReadMode) hClose $
      -- listy arg
      incrementy arg
