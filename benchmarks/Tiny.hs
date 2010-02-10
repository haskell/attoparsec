import qualified Data.Attoparsec.Incremental.Char8 as A
import qualified Text.ParserCombinators.Parsec as P
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative

x = "asnoteubaoe8u9823bnaotebusnt823bsoeut98234nbaoetu29234"
yS = take 570000 $ cycle x
yB = B.pack yS

mainA = print (A.parse (A.many (A.many1 A.letter <|> A.many1 A.digit)) yB)

mainP = print (P.parse (P.many (P.many1 P.letter P.<|> P.many1 P.digit)) "" yS)

main = mainP
