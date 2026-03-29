{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Elem
Description : Re-export module for HTML element parsing, combining Types, ElemHeadParse, SimpleElemParser, and ChainHTML
Copyright   : (c) Galen Sprout 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com
-}
module Scrappy.Elem (
    -- | @since 0.1.0.0
    module Scrappy.Elem.Types,
    -- | @since 0.1.0.0
    module Scrappy.Elem.ElemHeadParse,
    -- | @since 0.1.0.0
    module Scrappy.Elem.SimpleElemParser,
    -- | @since 0.1.0.0
    module Scrappy.Elem.ChainHTML,
) where

import Scrappy.Elem.ChainHTML
import Scrappy.Elem.ElemHeadParse
import Scrappy.Elem.SimpleElemParser hiding (manyTill_)
import Scrappy.Elem.Types
