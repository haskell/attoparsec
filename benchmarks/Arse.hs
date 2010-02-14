{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Applicative
import System.Environment
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Data.Word (Word8)
import Prelude hiding (getChar)

data Result r = Fail S [String] String
              | Partial (B.ByteString -> Result r)
              | Done S r

-- | The Parser monad is an Exception and State monad.
newtype Parser a = Parser {
      runParser :: forall r. S
                -> Failure   r
                -> Success a r
                -> Result r
    }

type Failure   r = S -> [String] -> String -> Result r
type Success a r = S -> a -> Result r

data InputState = Incomplete | Complete
                  deriving (Eq, Ord, Show)

data S = S {
      input :: !B.ByteString
    , added :: !B.ByteString
    , inputState :: !InputState
    } deriving (Show)

instance Show r => Show (Result r) where
    show (Fail _ stack msg) = "Fail " ++ show stack ++ " " ++ show msg
    show (Partial _) = "Partial _"
    show (Done bs r) = "Done " ++ show bs ++ " " ++ show r

feed :: Result r -> B.ByteString -> Result r
feed f@(Fail _ _ _) _ = f
feed (Partial k) d = k d
feed (Done (S s a c) r) d = Done (S (s +++ d) a c) r

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP m g =
    Parser (\st0 kf ks -> runParser m st0 kf (\s a -> runParser (g a) s kf ks))
{-# INLINE bindP #-}

returnP :: a -> Parser a
returnP a = Parser (\st0 _kf ks -> ks st0 a)
{-# INLINE returnP #-}

instance Monad Parser where
    return = returnP
    (>>=)  = bindP
    fail   = failDesc

plus :: Parser a -> Parser a -> Parser a
plus a b = Parser $ \(S s0 a0 c0) kf ks ->
           let kf' (S _s1 a1 c1) _ _ = runParser b st1 kf ks
                   where st1 = S (s0 +++ a1) (a0 +++ a1) (max c0 c1)
           in  runParser a (S s0 B.empty c0) kf' ks
{-# INLINE plus #-}

instance MonadPlus Parser where
    mzero = failDesc "mzero"
    mplus = plus

fmapP :: (a -> b) -> Parser a -> Parser b
fmapP p m = Parser (\st0 f k -> runParser m st0 f (\s a -> k s (p a)))
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
failDesc err = Parser (\st0 kf _ks -> kf st0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

ensure :: Int -> Parser ()
ensure n = Parser $ \st0@(S s0 a0 c0) kf ks ->
    if B.length s0 >= n
    then ks st0 ()
    else runParser (acquireInput >> ensure n) st0 kf ks

acquireInput :: Parser ()
acquireInput = Parser $ \st0@(S s0 a0 c0) kf ks ->
    if c0 == Complete
    then kf st0 ["acquireInput"] "not enough bytes"
    else Partial $ \s ->
         if B.null s
         then kf (S s0 a0 Complete) ["acquireInput"] "not enough bytes"
         else let st1 = S (s0 +++ s) (a0 +++ s) Incomplete
              in  ks st1 ()

failK :: Failure a
failK st0 stack msg = Fail st0 stack msg

successK :: Success a a
successK state a = Done state a

get :: Parser B.ByteString
get  = Parser (\st0 _kf ks -> ks st0 (input st0))

put :: B.ByteString -> Parser ()
put s = Parser (\(S _s0 a0 c0) _kf ks -> ks (S s a0 c0) ())

getBytes :: Int -> Parser B.ByteString
getBytes n = do
    ensure n
    s <- get
    let (consume,rest) = B.splitAt n s
    put rest
    return consume

getWord8 :: Parser Word8
getWord8 = getPtr (sizeOf (undefined :: Word8))

getChar :: Parser Char
getChar = B.w2c `fmapP` getWord8

(+++) :: B.ByteString -> B.ByteString -> B.ByteString
(+++) = B.append
{-# INLINE (+++) #-}

try :: Parser a -> Parser a
try p = Parser $ \(S s0 a0 c0) kf ks ->
        let kf' (S _s1 a1 c1) = kf (S (s0 +++ a1) (a0 +++ a1) (max c0 c1))
        in  runParser p (S s0 B.empty c0) kf' ks

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  ensure 1
  s <- get
  let c = B.w2c (B.unsafeHead s)
  if p c
    then put (B.unsafeTail s) >> return c
    else fail "satisfy"
{-# INLINE satisfy #-}
  
letter :: Parser Char
letter = satisfy $ \c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

letter1 :: Parser Char
letter1 = try $ do
  c <- getChar
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    then return c
    else fail "letter"

digit :: Parser Char
digit = satisfy $ \c -> c >= '0' && c <= '9'

digit1 :: Parser Char
digit1 = try $ do
  c <- getChar
  if (c >= '0' && c <= '9')
    then return c
    else fail "digit"

getPtr :: Storable a => Int -> Parser a
getPtr n = do
    (fp,o,_) <- B.toForeignPtr `fmapP` getBytes n
    return . B.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)

parse :: Parser a -> B.ByteString -> Result a
parse m s = runParser m (S s B.empty Incomplete) failK successK
              
manyP :: Parser a -> Parser [a]
manyP v = many_v
    where many_v = some_v <|> pure []
	  some_v = (:) <$> v <*> many_v

many_ :: Parser a -> Parser ()
many_ v = many_v
    where many_v = some_v <|> pure ()
	  some_v = v *> many_v

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- manyP p
  return (a:as)

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
              then B8.toChunks `fmap` B8.readFile arg
              else do
                content <- B.readFile arg
                return $ if True
                         then [content]
                         else chunksOf 24 content
    let p = manyP (many1 letter `mplus` many1 digit)
    print (parseAll p chunks)
