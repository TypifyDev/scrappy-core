{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Types
Description : Core types for the scrappy HTML scraping library
Copyright   : (c) Galen Sprout, 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com
-}
module Scrappy.Types (
    -- * Core types

    -- | @since 0.1.0.0
    Html,
    -- | @since 0.1.0.0
    ScrapeFail (..),

    -- * Parser utilities

    -- | @since 0.1.0.0
    mapMaybe,
) where

import qualified Data.Text as T
import Text.Parsec (ParsecT, parserZero)
import Prelude (Maybe (..), Show, pure)

{- | Upgrade a parser result via a partial function, failing with 'parserZero' on 'Nothing'.

@since 0.1.0.0
-}
mapMaybe :: (a -> Maybe b) -> ParsecT s u m a -> ParsecT s u m b
mapMaybe f ma = do
    x <- ma
    case f x of
        Just a -> pure a
        Nothing -> parserZero

{- | Failure modes for scraping operations.

@since 0.1.0.0
-}
data ScrapeFail = Eof | NonMatch deriving (Show)

{- | Raw HTML content as 'Text'.

@since 0.1.0.0
-}
type Html = T.Text
