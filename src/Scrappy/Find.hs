{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Find
Description : Search and stream-editing combinators for Parsec parsers
Copyright   : (c) Galen Sprout, 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com

Provides combinators for finding patterns separated by arbitrary noise
in a parse stream, sequential matching of multiple patterns, and
stream-editing (search-and-replace) via Parsec parsers.
-}
module Scrappy.Find (
    -- * Naive finders

    -- | @since 0.1.0.0
    findNaive,
    -- | @since 0.1.0.0
    findNaiveIO,

    -- * Core finders

    -- | @since 0.1.0.0
    findIO,
    -- | @since 0.1.0.0
    find,

    -- * Sequential finders

    -- | @since 0.1.0.0
    findSequential,
    -- | @since 0.1.0.0
    findSequential2,
    -- | @since 0.1.0.0
    findSequential3,
    -- | @since 0.1.0.0
    findUntilMatch,

    -- * Stream editing

    -- | @since 0.1.0.0
    streamEdit,
    -- | @since 0.1.0.0
    findEdit,
    -- | @since 0.1.0.0
    editFirst,
    -- | @since 0.1.0.0
    StreamEditCase (..),

    -- * HTML helpers

    -- | @since 0.1.0.0
    findSomeHTMLNaive,
    -- | @since 0.1.0.0
    findSomeHTML,

    -- * Unimplemented placeholders

    -- | @since 0.1.0.0
    findAllBetween,
    -- | @since 0.1.0.0
    buildSequentialElemsParser,

    -- * Low-level building blocks

    -- | @since 0.1.0.0
    baseParser,
    -- | @since 0.1.0.0
    givesNothing,
    -- | @since 0.1.0.0
    endStream,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Text.Parsec (
    ParseError,
    Parsec,
    ParsecT,
    Stream,
    anyChar,
    eof,
    many,
    parse,
    parserZero,
    try,
    (<|>),
 )
import Prelude (Char, Either (..), Maybe (..), Show, String, fmap, length, mempty, print, return, sequenceA, undefined, ($), (.), (<$), (<$>), (<>), (==), (>>))

import Scrappy.Types (ScrapeFail (Eof, NonMatch))

{- | Convert a parser into one that returns 'Nothing' on zero matches
or @'Just' results@ when at least one match is found. Collects all
matches from the stream, discarding non-matching noise.

@since 0.1.0.0
-}
findNaive :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m (Maybe [a])
findNaive p = (justify . (fromRight mempty) . sequenceA) <$> (find p)
  where
    justify x = if length x == 0 then Nothing else Just x

{- | Like 'findNaive' but prints each intermediate result via 'liftIO' for
debugging purposes.

@since 0.1.0.0
-}
findNaiveIO :: (MonadIO m, Stream s m Char, Show a) => ParsecT s u m a -> ParsecT s u m (Maybe [a])
findNaiveIO p = (justify . (fromRight mempty) . sequenceA) <$> (findIO p)
  where
    justify x = if length x == 0 then Nothing else Just x

{- | Like 'find' but prints each intermediate parse result to stdout.
Useful for debugging parsers interactively.

@since 0.1.0.0
-}
findIO :: (MonadIO m, Stream s m Char, Show a) => ParsecT s u m a -> ParsecT s u m [Either ScrapeFail a]
findIO parser = do
    x <- (try (baseParser parser)) <|> givesNothing <|> endStream
    liftIO $ print x
    case x of
        Right _ -> fmap (x :) (find parser)
        Left Eof -> return []
        Left NonMatch -> find parser

{- | Find all occurrences of a parser in a stream, returning results
tagged with 'Right' for matches. Non-matching characters are silently
skipped, and the search terminates at end-of-input.

@since 0.1.0.0
-}
find :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [Either ScrapeFail a]
find parser = do
    x <- (try (baseParser parser)) <|> givesNothing <|> endStream
    case x of
        Right _ -> fmap (x :) (find parser)
        Left Eof -> return []
        Left NonMatch -> find parser

{- | Find a sequence of parsers in order. Currently unimplemented.

@since 0.1.0.0
-}
findSequential :: (Stream s m Char) => [ParsecT s u m a] -> ParsecT s u m [Either ScrapeFail a]
findSequential _parsers = undefined

{- | Run two parsers sequentially, skipping noise between them.
Each parser consumes input up to and including its first match.

@since 0.1.0.0
-}
findSequential2 :: (Stream s m Char) => (ParsecT s u m a, ParsecT s u m b) -> ParsecT s u m (a, b)
findSequential2 (a, b) = do
    a' <- findUntilMatch a
    b' <- findUntilMatch b
    return (a', b')

{- | Run three parsers sequentially, skipping noise between them.
Each parser consumes input up to and including its first match.

@since 0.1.0.0
-}
findSequential3 :: (Stream s m Char) => (ParsecT s u m a, ParsecT s u m b, ParsecT s u m c) -> ParsecT s u m (a, b, c)
findSequential3 (a, b, c) = do
    a' <- findUntilMatch a
    b' <- findUntilMatch b
    c' <- findUntilMatch c
    return (a', b', c')

{- | Skip noise characters until the given parser matches, then return the
matched value. Fails with 'parserZero' if the end of input is reached
without a match.

@since 0.1.0.0
-}
findUntilMatch :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m a
findUntilMatch parser = do
    x <- (try (baseParser parser)) <|> givesNothing
    case x of
        Right a -> return a
        Left NonMatch -> findUntilMatch parser
        Left Eof -> parserZero

{- | Should never throw Left or I did it wrong
Uses Text.Builder for O(n) instead of O(n^2) performance

@since 0.1.0.0
-}
streamEdit :: ParsecT T.Text () Identity a -> (a -> T.Text) -> T.Text -> T.Text
streamEdit p f src =
    TL.toStrict $
        TB.toLazyText $
            fromRight mempty $
                parse (try $ findEditBuilder f p) "" src

-- | Build result using Text.Builder for O(1) appends
findEditBuilder :: (Stream T.Text m Char) => (a -> T.Text) -> ParsecT T.Text u m a -> ParsecT T.Text u m TB.Builder
findEditBuilder f parser = go
  where
    go = do
        let endOfStream = try eof >> return Nothing
        x <- (Just . Left . f <$> try parser) <|> (Just . Right <$> anyChar) <|> endOfStream
        case x of
            Just (Left txt) -> (TB.fromText txt <>) <$> go
            Just (Right chr) -> (TB.singleton chr <>) <$> go
            Nothing -> return mempty

{- | Legacy findEdit for backwards compatibility (now uses Builder internally)

@since 0.1.0.0
-}
findEdit :: (Stream T.Text m Char) => (a -> T.Text) -> ParsecT T.Text u m a -> ParsecT T.Text u m T.Text
findEdit f parser = fmap (TL.toStrict . TB.toLazyText) (findEditBuilder f parser)

{- | Edit only the first match, leave rest unchanged

@since 0.1.0.0
-}
editFirst :: (Stream T.Text m Char) => (a -> T.Text) -> ParsecT T.Text u m a -> ParsecT T.Text u m T.Text
editFirst f parser = fmap (TL.toStrict . TB.toLazyText) $ go
  where
    go = do
        let endOfStream = try eof >> return Nothing
        x <- (Just . Left . f <$> try parser) <|> (Just . Right <$> anyChar) <|> endOfStream
        case x of
            Just (Left txt) -> do
                rest <- many anyChar
                return $ TB.fromText txt <> TB.fromString rest
            Just (Right chr) -> (TB.singleton chr <>) <$> go
            Nothing -> return mempty

{- | Internal type for classifying each step of a stream-edit pass.
'Edit' holds the transformed match, 'Carry' holds a non-matching
character, and 'EOF' signals end of input.

@since 0.1.0.0
-}
data StreamEditCase
    = EOF
    | Carry Char
    | Edit String

{- | Lift a parser into a 'Right'-returning parser. This is the
"base" building block: if the parser succeeds the result is
wrapped in 'Right'.

@since 0.1.0.0
-}
baseParser :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m (Either ScrapeFail a)
baseParser parser = fmap Right parser

{- | Consume a single character and return @'Left' 'NonMatch'@. Used
as an alternative branch to skip noise characters one at a time.

@since 0.1.0.0
-}
givesNothing :: (Stream s m Char) => ParsecT s u m (Either ScrapeFail a)
givesNothing = Left NonMatch <$ anyChar

{- | Attempt to match end-of-input and return @'Left' 'Eof'@ on
success. Uses 'try' so that it does not consume input on failure.

@since 0.1.0.0
-}
endStream :: (Stream s m t, Show t) => ParsecT s u m (Either ScrapeFail a)
endStream = try (eof) >> (return $ Left Eof)

{- | Parse all matches from HTML source, returning 'Nothing' when no
matches are found. A convenience wrapper around 'findNaive' and
'parse'.

@since 0.1.0.0
-}
findSomeHTMLNaive :: (Stream s Identity Char) => Parsec s () a -> s -> (Maybe [a])
findSomeHTMLNaive parser text =
    let parser' = findNaive parser
     in case parse parser' "from html:add-in URL soon" text of
            Left _ -> Nothing
            Right maybe_A -> maybe_A

{- | Parse all matches from HTML source, preserving the 'ParseError'
on failure. Returns @'Right' ('Maybe' [a])@ on successful parse.

@since 0.1.0.0
-}
findSomeHTML :: (Stream s Identity Char) => Parsec s () a -> s -> Either ParseError (Maybe [a])
findSomeHTML parser text =
    let parser' = findNaive parser
     in parse parser' "from html at this url: <unimplemented - derp>" text

{- | Find all patterns between two delimiters. Currently unimplemented.

@since 0.1.0.0
-}
findAllBetween :: a
findAllBetween = undefined

{- | Build a parser that collects sequential element matches.
Currently unimplemented.

@since 0.1.0.0
-}
buildSequentialElemsParser :: ParsecT s u m [a]
buildSequentialElemsParser = undefined
