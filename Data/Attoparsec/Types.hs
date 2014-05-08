-- |
-- Module      :  Data.Attoparsec.Types
-- Copyright   :  Bryan O'Sullivan 2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for strings, loosely based on
-- the Parsec library.

module Data.Attoparsec.Types
    (
      Parser
    , IResult(..)
    ) where

import Data.Attoparsec.Internal.Types (Parser(..), IResult(..))
