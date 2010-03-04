import Control.Monad
import System.Environment
import qualified Data.Attoparsec.Char8 as A
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.ByteString.Char8 as B
import Control.Applicative

mainA = do
  args <- getArgs
  forM_ args $ \arg -> do
    input <- B.readFile arg
    let p = A.parse (A.many (A.many1 A.letter <|> A.many1 A.digit)) input
    case p `A.feed` B.empty of
      A.Done _ xs -> print (length xs)
      what        -> print what

mainP = do
  args <- getArgs
  forM_ args $ \arg -> do
    input <- readFile arg
    case P.parse (P.many (P.many1 P.letter P.<|> P.many1 P.digit)) "" input of
      Left err -> print err
      Right xs -> print (length xs)

main = mainA
