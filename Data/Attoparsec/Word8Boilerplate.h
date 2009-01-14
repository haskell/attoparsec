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
