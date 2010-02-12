import Control.Monad
import System.Environment
import qualified Data.Attoparsec.Incremental.Char8 as A
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative

mainA = do
  args <- getArgs
  forM_ args $ \arg -> do
    input <- B.readFile arg
    print (A.parse (A.many (A.many1 A.letter <|> A.many1 A.digit)) input)

mainP = do
  args <- getArgs
  forM_ args $ \arg -> do
    input <- readFile arg
    print (P.parse (P.many (P.many1 P.letter P.<|> P.many1 P.digit)) "" input)

main = mainP
