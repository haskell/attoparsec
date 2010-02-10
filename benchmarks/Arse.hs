{-# LANGUAGE Rank2Types #-}

module Main (main) where
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString as S
import Data.ByteString.Internal (inlinePerformIO, toForeignPtr, w2c)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Data.Int (Int64)
import Data.Word (Word8)
import Prelude hiding (getChar)

data Result r = Fail [String] String
              | Partial (ByteString -> Result r)
              | Done ByteString r

-- | The Parser monad is an Exception and State monad.
newtype Parser a = Parser {
      runParser :: forall r. S
                -> Failure   r
                -> Success a r
                -> Result r
    }

type Failure   r = [String] -> String -> Result r
type Success a r = S -> a -> Result r

type S = ByteString

instance Show r => Show (Result r) where
    show (Fail stack msg) = "Fail " ++ show stack ++ " " ++ show msg
    show (Partial _) = "Partial _"
    show (Done bs r) = "Done " ++ show bs ++ " " ++ show r

feed :: Result r -> ByteString -> Result r
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

plus :: Parser a -> Parser a -> Parser a
plus a b = Parser (\s0 kf ks -> runParser a s0 (\_ _ -> runParser b s0 kf ks) ks)
{-# INLINE plus #-}

instance MonadPlus Parser where
    mzero = failDesc "mzero"
    mplus = plus

fmapP :: (a -> b) -> Parser a -> Parser b
fmapP p m = Parser (\s0 f k -> runParser m s0 f (\s a -> k s (p a)))
{-# INLINE fmapP #-}

instance Functor Parser where
    fmap = fmapP

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative Parser where
    pure  = returnP
    (<*>) = apP

instance Alternative Parser where
    empty = failDesc "empty"
    (<|>) = mplus

failDesc :: String -> Parser a
failDesc err = Parser (\_s0 kf _ks -> kf [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

compareLength :: ByteString -> Int64 -> Ordering
compareLength (B.Chunk sb lb) n =
    let l = S.length sb
    in if l > fromIntegral n
       then GT
       else compareLength lb (n - fromIntegral l)
compareLength _ 0 = EQ
compareLength _ _ = LT

ensure :: Int -> Parser ()
ensure n = Parser $ \s0 kf ks ->
           if compareLength s0 (fromIntegral n) /= LT
           then ks s0 ()
           else Partial $ \s -> if B.null s
                           then kf ["ensure"] "not enough bytes"
                           else runParser (ensure n) (B.append s0 s) kf ks

failK :: Failure a
failK stack msg = Fail stack msg

successK :: Success a a
successK state a = Done state a

get :: Parser ByteString
get  = Parser (\s0 _kf ks -> ks s0 s0)

put :: ByteString -> Parser ()
put s = Parser (\_s0 _kf ks -> ks s ())

getBytes :: Int -> Parser ByteString
getBytes n = do
    ensure n
    s <- get
    let (consume,rest) = B.splitAt (fromIntegral n) s
    put rest
    return consume

getWord8 :: Parser Word8
getWord8 = getPtr (fromIntegral (sizeOf (undefined :: Word8)))

getChar :: Parser Char
getChar = w2c `fmapP` getWord8

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
  s <- getBytes n
  let (fp,o,_) = toForeignPtr (S.concat (B.toChunks s))
  return . inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

parse :: Parser a -> ByteString -> Result a
parse m s = runParser m s failK successK
              
manyP :: Parser a -> Parser [a]
manyP v = many_v
    where many_v = some_v <|> pure []
	  some_v = (:) <$> v <*> many_v

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- manyP p
  return (a:as)

x = "asnoteubaoe8u9823bnaotebusnt823bsoeut98234nbaoetu29234"
yS = take 570000 $ cycle x
yB = B.pack yS

main = print (parse (manyP (many1 letter `mplus` many1 digit)) yB)
