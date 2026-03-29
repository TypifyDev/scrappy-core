{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Elem.ChainHTML
Description : Sequencing operators and container-based parsing for HTML element chains
Copyright   : (c) Galen Sprout 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com

Provides combinators for sequencing HTML parsers, parsing content within
container elements (shells), and flexible repetition over HTML structures.
-}
module Scrappy.Elem.ChainHTML (
    -- | @since 0.1.0.0
    nl,
    -- | @since 0.1.0.0
    manyHtml,
    -- | @since 0.1.0.0
    someHtml,
    -- | @since 0.1.0.0
    manyTillHtml_,
    -- | @since 0.1.0.0
    htmlTag,
    -- | @since 0.1.0.0
    manyTill_,
    -- | @since 0.1.0.0
    clean,
    -- | @since 0.1.0.0
    mustContain,
    -- | @since 0.1.0.0
    Shell,
    -- | @since 0.1.0.0
    contains'',
    -- | @since 0.1.0.0
    parseInShell,
    -- | @since 0.1.0.0
    contains,
    -- | @since 0.1.0.0
    containsMany,
    -- | @since 0.1.0.0
    contains',
    -- | @since 0.1.0.0
    containsFirst,
    -- | @since 0.1.0.0
    sequenceHtml,
    -- | @since 0.1.0.0
    sequenceHtml_,
    -- | @since 0.1.0.0
    (</>>),
    -- | @since 0.1.0.0
    (</>>=),
) where

import Prelude (
    Char,
    Either (Left, Right),
    Int,
    Maybe (Just, Nothing),
    String,
    length,
    pure,
    return,
    show,
    undefined,
    ($),
    (<$>),
    (<*),
    (>),
    (>>),
 )

import Control.Applicative (liftA2, some)
import Data.Functor.Identity (Identity)
import Data.Maybe (listToMaybe)
import Scrappy.Elem.ElemHeadParse (parseOpeningTag)
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Elem.Types (Elem, Elem', ElemHead, ShowHTML, innerText', matches')
import Scrappy.Find (findNaive)
import Text.Parsec (
    ParsecT,
    Stream,
    char,
    many,
    optional,
    parse,
    parserFail,
    parserZero,
    string,
    (<|>),
 )

{- | Skip optional newlines and spaces between HTML elements.

@since 0.1.0.0
-}
nl :: (Stream s m Char) => ParsecT s u m ()
nl = optional (many $ (char '\n' <|> char ' '))

{- | Parse zero or more occurrences of a parser, consuming whitespace between them.

@since 0.1.0.0
-}
manyHtml :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
manyHtml p = many $ p <* nl

{- | Parse one or more occurrences of a parser, consuming whitespace between them.

@since 0.1.0.0
-}
someHtml :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m [a]
someHtml p = some $ p <* nl

{- | Parse many elements with whitespace until an end parser succeeds.

@since 0.1.0.0
-}
manyTillHtml_ :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTillHtml_ p end = manyTill_ (p <* nl) end

{- | Parse an HTML opening tag.

@since 0.1.0.0
-}
htmlTag :: (Stream s m Char) => ParsecT s u m ElemHead
htmlTag = parseOpeningTag (Just ["html"]) []

{- | Parse many elements until an end parser succeeds, returning both the
collected elements and the end result.

@since 0.1.0.0
-}
manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go

{- | Clean a string by removing unwanted characters. Currently undefined.

@since 0.1.0.0
-}
clean :: String -> String
clean = undefined

{- | Require that an element's inner text contains at least @count@ matches
of the given pattern.

@since 0.1.0.0
-}
mustContain :: ParsecT s u m (Elem' a) -> Int -> ParsecT s u m b -> ParsecT s u m (Elem' a)
mustContain e count _pat = do
    out <- e
    case parse (findNaive $ string "Search") "" (innerText' out) of
        Right (Just xs) -> if count > length xs then parserZero else return out
        _ -> parserZero

{- | An elem head configures the bracketing so this is all we need for crafting.

@since 0.1.0.0
-}
type Shell = (Elem, [(String, Maybe String)])

{- | Parse content inside a shell element, returning all matches.

@since 0.1.0.0
-}
contains'' ::
    (Stream s m Char, ShowHTML a) =>
    Shell ->
    ParsecT s u m a ->
    ParsecT s u m [a]
contains'' (e, as) p = matches' <$> elemParser (Just [e]) (Just p) as

{- | Alias for 'contains'.

@since 0.1.0.0
-}
parseInShell :: ParsecT s u m (Elem' a) -> ParsecT String () Identity b -> ParsecT s u m b
parseInShell = contains

{- | Parse content inside a shell element. This will be fully removed in the future.
99% of the time this is gonna be desired to pair with findNaive.

@since 0.1.0.0
-}
{-# DEPRECATED contains "this should have been called parseInShell from the start, you probably want contains' or containsFirst" #-}
contains :: ParsecT s u m (Elem' a) -> ParsecT String () Identity b -> ParsecT s u m b
contains shell b = do
    x <- shell

    let
        ridNL p = (many (char ' ' <|> char '\n')) >> p

    case parse (ridNL b) "" (innerText' x) of
        Right match -> return match
        Left err -> parserFail (show err)

{- | Find multiple matches anywhere inside the passed elem.

This function is also quite extensible because when used with @scrape@
this combo will return a list of list of elems where the hierarchy of HTML
has been preserved but a great deal of information has been filtered out.
An example use case would be knowing that you want @\<p\>@ tags from a set
of very specific shells, this could allow you to analyze what and how many
came from each shell.

This also naturally extends to running this same scraper on multiple pages
which would allow you to recover ample details on the number of match_A in
shell_S on Page_P.

@since 0.1.0.0
-}
containsMany
    , contains' ::
        (ShowHTML a) =>
        ParsecT s u m (Elem' a) ->
        ParsecT String () Identity b ->
        ParsecT s u m [b]
containsMany = contains'

{- | Find multiple matches anywhere inside the passed elem.

@since 0.1.0.0
-}
contains' shell b = do
    x <- shell
    case parse (findNaive b) "" (innerText' x) of
        Right (Just matches) -> pure matches
        Left err -> parserFail (show err)
        Right Nothing -> parserFail "no matches in this container"

{- | Find the first match inside the passed elem.

@since 0.1.0.0
-}
containsFirst ::
    (ShowHTML a) =>
    ParsecT s u m (Elem' a) ->
    ParsecT String () Identity b ->
    ParsecT s u m b
containsFirst shell b = do
    result <- contains' shell b
    case listToMaybe result of
        Just x -> return x
        Nothing -> parserFail "no matches in containsFirst"

{- | Sequence two HTML parsers with whitespace between, keeping both results.

@since 0.1.0.0
-}
sequenceHtml :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
sequenceHtml p1 p2 = do
    x <- p1
    _ <- many (char ' ' <|> char '\n' <|> char '\t')
    y <- p2
    return (x, y)

{- | Sequence two HTML parsers with whitespace between, discarding the first result.

@since 0.1.0.0
-}
sequenceHtml_ :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b
sequenceHtml_ p1 p2 = do
    _ <- p1
    _ <- many (char ' ' <|> char '\n' <|> char '\t')
    p2

{- | Infix operator for 'sequenceHtml_'. Sequences two HTML parsers, discarding
the first result.

@since 0.1.0.0
-}
(</>>) :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b
(</>>) = sequenceHtml_

{- | Infix operator for 'sequenceHtml'. Sequences two HTML parsers, keeping
both results as a tuple.

@since 0.1.0.0
-}
(</>>=) :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
(</>>=) = sequenceHtml
