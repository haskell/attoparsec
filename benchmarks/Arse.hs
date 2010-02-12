{-# LANGUAGE Rank2Types #-}

module Main where

import Control.Applicative
import Data.Monoid
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Data.Word (Word8)
import Prelude hiding (getChar)
--import Debug.Trace
trace a b = b

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

data AddState = Incomplete | Complete
                deriving (Eq, Ord, Show)

data S = S {
      input :: B.ByteString
    , added :: B.ByteString
    , addState :: AddState
    } deriving (Show)

instance Show r => Show (Result r) where
    show (Fail _ stack msg) = "Fail " ++ show stack ++ " " ++ show msg
    show (Partial _) = "Partial _"
    show (Done bs r) = "Done " ++ show bs ++ " " ++ show r

feed :: Result r -> B.ByteString -> Result r
feed f@(Fail _ _ _) _ = f
feed (Partial k) d = trace "k" k d
feed (Done st0@(S s a c) r) d = Done (S (B.append s d) a c) r

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
plus a b = Parser (\st0@(S s0 a0 c0) kf ks -> runParser a (S s0 B.empty c0) (\st1@(S _s1 a1 c1) _ _ -> trace (show ("+",st0,st1)) runParser b (S s0 (B.append a0 a1) (max c0 c1)) kf ks) ks)
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
failDesc err = Parser (\st0 kf _ks -> trace (show ("fail",err,st0)) kf st0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

ensure :: Int -> Parser ()
ensure n = Parser $ \st0@(S s0 a0 c0) kf ks ->
           if B.length s0 >= n
           then ks st0 ()
           else if c0 == Complete
                then kf st0 ["ensure"] "not enough bytes"
                else trace "p" Partial $ \s -> trace (show ("partial",s)) $ if B.null s
                           then trace "kf" kf (S s0 a0 Complete) ["ensure"] "not enough bytes"
                           else let st1 = S (B.append s0 s) (B.append a0 s) Incomplete
                                in trace (show ("resume",st1)) runParser (ensure n) st1 kf ks

failK :: Failure a
failK st0 stack msg = Fail st0 stack msg

successK :: Success a a
successK state a = Done state a

get :: Parser B.ByteString
get  = Parser (\st0 _kf ks -> ks st0 (input st0))

put :: B.ByteString -> Parser ()
put s = Parser (\st0@(S _s0 a0 c0) _kf ks -> ks (S s a0 c0) ())

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

try :: Parser a -> Parser a
try p = Parser (\st0@(S s0 a0 c0) kf ks -> trace (show ("trying",st0)) runParser p (S s0 B.empty c0) (\(S s1 a1 c1) a b -> let st1 = S s0 (B.append a0 a1) (max c0 c1) in trace (show ("tried",st1)) kf st1 a b) ks)

letter :: Parser Char
letter = try $ do
  c <- getChar
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    then return c
    else fail "letter"

digit :: Parser Char
digit = try $ do
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

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- manyP p
  return (a:as)

x = "asnoteubaoe8u9823bnaotebusnt823bsoeut98234nbaoetu29234"
yS = take 570000 $ cycle x
yB = B.pack yS

main = print (parse (manyP (many1 letter `mplus` many1 digit)) yB `feed` B.empty)
