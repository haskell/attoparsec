{-# LANGUAGE Rank2Types #-}

--module Main (main) where

import Debug.Trace
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

data Feed = Empty
          | Fed !B.ByteString
            deriving (Eq, Ord, Show)

instance Monoid Feed where
    mempty = Empty
    mappend Empty b = b
    mappend a Empty = a
    mappend (Fed a) (Fed b) = Fed (B.append a b)

augment :: B.ByteString -> Feed -> B.ByteString
augment bs Empty = bs
augment bs (Fed s) = B.append bs s

shove :: Feed -> B.ByteString -> Feed
shove Empty bs = Fed bs
shove (Fed s) bs = Fed (B.append s bs)

isEOF :: Feed -> Bool
isEOF (Fed s) = B.null s
isEOF _ = False

data S = S {
      input :: B.ByteString
    , fed :: Feed
    } deriving (Show)

instance Show r => Show (Result r) where
    show (Fail _ stack msg) = "Fail " ++ show stack ++ " " ++ show msg
    show (Partial _) = "Partial _"
    show (Done bs r) = "Done " ++ show bs ++ " " ++ show r

feed :: Result r -> B.ByteString -> Result r
feed f@(Fail _ _ _) _ = f
feed (Partial k) s = trace "k" k s
feed (Done bs r) s = Done (S (B.append (input bs) s) (fed bs)) r

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

merge (S i1 a1) (S i2 a2) = S i1 (mappend a1 a2)

plus :: Parser a -> Parser a -> Parser a
plus a b = Parser (\s0@(S i j) kf ks -> runParser a s0 (\s1@(S x y) _ _ -> trace (show ("+",s0,s1)) runParser b (S (augment i y) (mappend j y)) kf ks) ks)
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
failDesc err = Parser (\s0 kf _ks -> trace (show ("fail",err,s0)) kf s0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

ensure :: Int -> Parser ()
ensure n = Parser $ \s0@(S a b) kf ks ->
           if B.length (input s0) >= n
           then ks s0 ()
           else if isEOF b
                then kf s0 ["ensure"] "not enough bytes"
                else trace "p" Partial $ \s -> trace (show ("partial",s)) $ if B.null s
                           then trace "kf" kf (S a (shove b s)) ["ensure"] "not enough bytes"
                           else let s1 = S (B.append a s) (shove b s)
                                in trace (show ("resume",s1)) runParser (ensure n) s1 kf ks

failK :: Failure a
failK s0 stack msg = Fail s0 stack msg

successK :: Success a a
successK state a = Done state a

get :: Parser B.ByteString
get  = Parser (\s0 _kf ks -> ks s0 (input s0))

put :: B.ByteString -> Parser ()
put s = Parser (\s0 _kf ks -> ks (S s (fed s0)) ())

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
try p = Parser (\s0@(S i j) kf ks -> trace (show ("trying",s0)) runParser p s0 (\(S x y) a b -> let s1 = S (augment i y) (mappend j y) in trace (show ("tried",s1)) kf s1 a b) ks)

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
parse m s = runParser m (S s mempty) failK successK
              
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
