{-# LANGUAGE Rank2Types #-}

module Main (main) where
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Data.Word (Word8)
import Prelude hiding (getChar)

data Result r = Fail [String] String
              | Partial (B.ByteString -> Result r)
              | Done B.ByteString r

-- | The Parser monad is an Exception and State monad.
newtype Parser a = Parser {
      runParser :: forall r. S
                -> Failure   r
                -> Success a r
                -> Result r
    }

type Failure   r = [String] -> String -> Result r
type Success a r = S -> a -> Result r

type S = B.ByteString

instance Show r => Show (Result r) where
    show (Fail stack msg) = "Fail " ++ show stack ++ " " ++ show msg
    show (Partial _) = "Partial _"
    show (Done bs r) = "Done " ++ show bs ++ " " ++ show r

feed :: Result r -> B.ByteString -> Result r
feed f@(Fail _ _) _ = f
feed (Partial k) s = k s
feed (Done bs r) s = Done (B.append bs s) r

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP m g =
    Parser (\s0 kf ks -> runParser m s0 kf (\s a -> runParser (g a) s kf ks))
{-# INLINE bindP #-}

returnP :: a -> Parser a
returnP a = Parser (\s0 _kf ks -> ks s0 a)
{-# INLINE returnP #-}

instance Monad Parser where
    return = returnP
    (>>=)  = bindP
    fail   = failDesc

instance MonadPlus Parser where
    mzero = failDesc "mzero"
    mplus a b = Parser (\s0 kf ks -> runParser a s0 (\_ _ -> runParser b s0 kf ks) ks)

failDesc :: String -> Parser a
failDesc err = Parser (\_s0 kf _ks -> kf [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

ensure :: Int -> Parser ()
ensure n = Parser $ \s0 kf ks ->
           if B.length s0 >= n
           then ks s0 ()
           else Partial $ \s -> if B.null s
                           then kf ["ensure"] "not enough bytes"
                           else runParser (ensure n) (B.append s0 s) kf ks

failK :: Failure a
failK stack msg = Fail stack msg

successK :: Success a a
successK state a = Done state a

get :: Parser B.ByteString
get  = Parser (\s0 _ k -> k s0 s0)

put :: B.ByteString -> Parser ()
put s = Parser (\_ _ k -> k s ())

getBytes :: Int -> Parser B.ByteString
getBytes n = do
    s <- get
    when (n > B.length s) (fail "too few bytes")
    let (consume,rest) = B.splitAt n s
    put rest
    return consume

getWord8 :: Parser Word8
getWord8 = getPtr (sizeOf (undefined :: Word8))

getChar :: Parser Char
getChar = B.w2c `liftM` getWord8

letter :: Parser Char
letter = do
  c <- getChar
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    then return c
    else fail "letter"

digit :: Parser Char
digit = do
  c <- getChar
  if (c >= '0' && c <= '9')
    then return c
    else fail "digit"

getPtr :: Storable a => Int -> Parser a
getPtr n = do
    (fp,o,_) <- B.toForeignPtr `liftM` getBytes n
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

parse :: Parser a -> B.ByteString -> Result a
parse m s = runParser m s failK successK
              
many :: Parser a -> Parser [a]
many v = many_v
    where many_v = some_v `mplus` return []
	  some_v = (:) `liftM` v `ap` many_v

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

x = "asnoteubaoe8u9823bnaotebusnt823bsoeut98234nbaoetu29234"
yS = take 570000 $ cycle x
yB = B.pack yS

main = print (parse (many (many1 letter `mplus` many1 digit)) yB)
