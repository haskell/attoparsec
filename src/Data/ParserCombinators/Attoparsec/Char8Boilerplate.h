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

-- | Satisfy a specific character.
char :: Char -> PARSER Char
char c = satisfy (== c) <?> [c]
{-# INLINE char #-}

-- | Satisfy a specific character.
notChar :: Char -> PARSER Char
notChar c = satisfy (/= c) <?> "not " ++ [c]
{-# INLINE notChar #-}

charClass :: String -> FastSet
charClass = set . SB.pack . go
    where go (a:'-':b:xs) = [a..b] ++ go xs
          go (x:xs) = x : go xs
          go _ = ""

inClass :: String -> Char -> Bool
inClass s = (`memberChar` myset)
    where myset = charClass s
{-# INLINE inClass #-}

notInClass :: String -> Char -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Consume characters while the predicate is true.
takeWhile :: (Char -> Bool) -> PARSER LB.ByteString
takeWhile p = I.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

takeTill :: (Char -> Bool) -> PARSER LB.ByteString
takeTill p = I.takeTill (p . w2c)
{-# INLINE takeTill #-}

-- | Skip over characters while the predicate is true.
skipWhile :: (Char -> Bool) -> PARSER ()
skipWhile p = I.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

-- | Skip over white space.
skipSpace :: PARSER ()
skipSpace = takeWhile isSpace >> return ()
{-# INLINE skipSpace #-}
