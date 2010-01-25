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

-- | Match any character in a set.
--
-- > vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- > halfAlphabet = inClass "a-nA-N"
--
-- To add a literal \'-\' to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Word8 -> Bool
inClass s = (`memberWord8` mySet)
    where mySet = charClass s
{-# INLINE inClass #-}

-- | Match any character not in a set.
notInClass :: String -> Word8 -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Skip over bytes while the predicate is true.
skipWhile :: (Word8 -> Bool) -> PARSER ()
skipWhile p = takeWhile p *> pure ()
{-# INLINE skipWhile #-}

-- | Match any byte.
anyWord8 :: PARSER Word8
anyWord8 = satisfy $ const True
{-# INLINE anyWord8 #-}

-- | Match a specific byte.
word8 :: Word8 -> PARSER Word8
word8 c = satisfy (== c) <?> show c
{-# INLINE word8 #-}

-- | Match any byte except the given one.
notWord8 :: Word8 -> PARSER Word8
notWord8 c = satisfy (/= c) <?> "not " ++ show c
{-# INLINE notWord8 #-}
