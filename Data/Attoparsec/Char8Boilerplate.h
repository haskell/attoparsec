-- -*- haskell -*-

-- | Character parser.
satisfy :: (Char -> Bool) -> PARSER Char
satisfy p = w2c <$> I.satisfy (p . w2c)
{-# INLINE satisfy #-}

letter :: PARSER Char
letter = satisfy isLetter <?> "letter"
{-# INLINE letter #-}

digit :: PARSER Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

anyChar :: PARSER Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

space :: PARSER Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Match a specific character.
char :: Char -> PARSER Char
char c = satisfy (== c) <?> [c]
{-# INLINE char #-}

-- | Match any character except the given one.
notChar :: Char -> PARSER Char
notChar c = satisfy (/= c) <?> "not " ++ [c]
{-# INLINE notChar #-}

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
inClass :: String -> Char -> Bool
inClass s = (`memberChar` mySet)
    where mySet = charClass s
{-# INLINE inClass #-}

-- | Match any character not in a set.
notInClass :: String -> Char -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Consume characters while the predicate succeeds.
takeWhile :: (Char -> Bool) -> PARSER LB.ByteString
takeWhile p = I.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

-- | Consume characters while the predicate fails.
takeTill :: (Char -> Bool) -> PARSER LB.ByteString
takeTill p = I.takeTill (p . w2c)
{-# INLINE takeTill #-}

-- | Skip over characters while the predicate succeeds.
skipWhile :: (Char -> Bool) -> PARSER ()
skipWhile p = I.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

-- | Skip over white space.
skipSpace :: PARSER ()
skipSpace = skipWhile isSpace >> return ()
{-# INLINE skipSpace #-}
