-- -----------------------------------------------------------------------------

-- |
-- Module      :  Text.IPv6Addr
-- Copyright   :  Copyright © Michel Boucey 2011-2015
-- License     :  BSD-Style
-- Maintainer  :  michel.boucey@gmail.com
--
-- Dealing with IPv6 address text representations, canonization and manipulations.
--

-- -----------------------------------------------------------------------------

module QC.IPv6.Types where

import Data.Text (Text, unpack)

newtype IPv6Addr = IPv6Addr Text

instance Show IPv6Addr where
    show (IPv6Addr addr) = unpack addr

data IPv6AddrToken
    = SixteenBit Text  -- ^ A four hexadecimal digits group representing a 16-Bit chunk
    | AllZeros           -- ^ An all zeros 16-Bit chunk
    | Colon              -- ^ A separator between 16-Bit chunks
    | DoubleColon        -- ^ A double-colon stands for a unique compression of many consecutive 16-Bit chunks
    | IPv4Addr Text    -- ^ An embedded IPv4 address as representation of the last 32-Bit
    deriving (Eq,Show)
