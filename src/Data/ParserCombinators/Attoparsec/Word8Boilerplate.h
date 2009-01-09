-- -*- haskell -*-
-- This file is intended to be #included by other source files.

instance MonadPlus (PARSER) where
    mzero = zero
    mplus = plus

instance Applicative (PARSER) where
    pure = return
    (<*>) = ap

instance Alternative (PARSER) where
    empty = zero
    (<|>) = plus

-- | Skip over characters while the predicate is true.
skipWhile :: (Word8 -> Bool) -> PARSER ()
skipWhile p = takeWhile p *> pure ()
{-# INLINE skipWhile #-}

many1 :: PARSER a -> PARSER [a]
many1 p = liftA2 (:) p (many p)

sepBy :: PARSER a -> PARSER s -> PARSER [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []

sepBy1 :: PARSER a -> PARSER s -> PARSER [a]
sepBy1 p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure [])

manyTill :: PARSER a -> PARSER b -> PARSER [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan

-- | Skip zero or more instances of the parser.
skipMany :: PARSER a -> PARSER ()
skipMany p = scan
    where scan = (p >> scan) <|> return ()

-- | Skip one or more instances of the parser.
skipMany1 :: PARSER a -> PARSER ()
skipMany1 p = p >> skipMany p

-- | Apply the given parser repeatedly, returning every parse result.
count :: Int -> PARSER a -> PARSER [a]
count n p = sequence (replicate n p)
{-# INLINE count #-}

anyWord8 :: PARSER Word8
anyWord8 = satisfy $ const True
{-# INLINE anyWord8 #-}

-- | Satisfy a specific character.
word8 :: Word8 -> PARSER Word8
word8 c = satisfy (== c) <?> show c
{-# INLINE word8 #-}

-- | Satisfy a specific character.
notWord8 :: Word8 -> PARSER Word8
notWord8 c = satisfy (/= c) <?> "not " ++ show c
{-# INLINE notWord8 #-}
