{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Elem.SimpleElemParser
Description : Simple HTML element parsers for flat element matching and inner text extraction
Copyright   : (c) Galen Sprout, 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com
-}
module Scrappy.Elem.SimpleElemParser (
    -- * Combinator utilities

    -- | @since 0.1.0.0
    eitherP,
    -- | @since 0.1.0.0
    manyTill_,

    -- * Element parsers

    -- | @since 0.1.0.0
    el,
    -- | @since 0.1.0.0
    elemParser,
    -- | @since 0.1.0.0
    elemParserWhere,

    -- * Clickable link parsers

    -- | @since 0.1.0.0
    clickableHref,
    -- | @since 0.1.0.0
    clickableHref',

    -- * Same-element matching

    -- | @since 0.1.0.0
    sameElTag,
    -- | @since 0.1.0.0
    matchesInSameElTag,

    -- * Self-closing element support

    -- | @since 0.1.0.0
    selfClosing,
    -- | @since 0.1.0.0
    elSelfC,
    -- | @since 0.1.0.0
    elSelfClosing,

    -- * Internal element parsing

    -- | @since 0.1.0.0
    elemWithBody,
    -- | @since 0.1.0.0
    elemParserInternal,
    -- | @since 0.1.0.0
    innerElemParser,

    -- * Styling tag support

    -- | @since 0.1.0.0
    stylingTags,
    -- | @since 0.1.0.0
    stylingElem,

    -- * Deprecated

    -- | @since 0.1.0.0
    parseInnerHTMLAndEndTag,
    -- | @since 0.1.0.0
    elemParserOld,
) where

import Prelude (
    Bool (..),
    Char,
    Either (..),
    Maybe (..),
    String,
    elem,
    fmap,
    foldr,
    fst,
    length,
    mconcat,
    mempty,
    pure,
    return,
    snd,
    ($),
    (.),
    (<),
    (<$>),
    (<>),
    (>>),
 )

import Scrappy.Elem.Types hiding (f)

import Scrappy.Elem.ElemHeadParse (parseOpeningTag, parseOpeningTagWhere)
import Scrappy.Links (LastUrl)

import Scrappy.Types (mapMaybe)

import Control.Applicative (Alternative, liftA2, some, (<|>))
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Text.Parsec (
    ParsecT,
    Stream,
    anyChar,
    anyToken,
    char,
    manyTill,
    optional,
    parserFail,
    parserZero,
    string,
    try,
 )

{- | Attempt the first parser; if it fails, attempt the second. Wraps results in 'Either'.

@since 0.1.0.0
-}
eitherP :: (Alternative m) => m a -> m b -> m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)

{- | Like 'manyTill' but also returns the end result alongside the accumulated list.

@since 0.1.0.0
-}
manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go

{- | Simplest interface to building element patterns.

@since 0.1.0.0
-}
el :: (Stream s m Char) => Elem -> [(String, String)] -> ParsecT s u m (Elem' String)
el element attrss = elemParser (Just (element : [])) Nothing ((fmap . fmap) Just attrss)

{- | Generic interface for building HTML element patterns where we do not differentiate
based on what is inside. For control of allowable inner HTML patterns, see ChainHTML
and\/or TreeElemParser.

@since 0.1.0.0
-}
elemParser ::
    (ShowHTML a, Stream s m Char) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (Elem' a)
elemParser elemList innerSpec attrSpec = do
    (elem', attrs') <- parseOpeningTag elemList attrSpec
    case elem' `Prelude.elem` selfClosing of
        True -> do
            _ <- try (string ">") <|> string "/>"
            case innerSpec of
                Nothing -> return $ Elem' elem' attrs' mempty mempty
                Just _ -> parserZero
        False -> do
            (asString, matchList) <-
                fmap (foldr foldFuncTup mempty) $
                    (try (string "/>") >> return [])
                        <|> (try $ innerElemParser elem' innerSpec)
                        <|> (selfClosingTextful innerSpec)
            return $ Elem' elem' attrs' matchList asString

{- | Generic interface for building HTML element patterns with attribute predicates.
For control of allowable inner HTML patterns, see ChainHTML and\/or TreeElemParser.

@since 0.1.0.0
-}
elemParserWhere ::
    (ShowHTML a, Stream s m Char) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    String ->
    (String -> Bool) ->
    ParsecT s u m (Elem' a)
elemParserWhere elemList innerSpec attr predicate = do
    (elem', attrs') <- parseOpeningTagWhere elemList attr predicate
    case elem' `Prelude.elem` selfClosing of
        True -> do
            _ <- try (string ">") <|> string "/>"
            case innerSpec of
                Nothing -> return $ Elem' elem' attrs' mempty mempty
                Just _ -> parserZero
        False -> do
            (asString, matchList) <-
                fmap (foldr foldFuncTup mempty) $
                    (try (string "/>") >> return [])
                        <|> (try $ innerElemParser elem' innerSpec)
                        <|> (selfClosingTextful innerSpec)
            return $ Elem' elem' attrs' matchList asString

{- | Parse a clickable anchor element, extracting its href attribute as a resolved URL.

@since 0.1.0.0
-}
clickableHref :: (Stream s m Char) => Bool -> LastUrl -> ParsecT s u m Clickable
clickableHref booly cUrl = do
    elA <- parseOpeningTag Nothing [("href", Nothing)]
    href <- mapMaybe (getHrefAttrs booly cUrl) (pure $ snd elA)
    return $ Clickable elA href

{- | Parse a clickable anchor element with a custom inner content parser.

@since 0.1.0.0
-}
clickableHref' ::
    (Stream s m Char, ShowHTML a) =>
    ParsecT s u m a ->
    Bool ->
    LastUrl ->
    ParsecT s u m Clickable
clickableHref' innerPat booly cUrl = do
    e <- elemParser Nothing (Just $ innerPat) [("href", Nothing)]
    href <- mapMaybe (getHrefEl booly cUrl) (pure e)
    return $ Clickable (elTag e, attrs e) href

{- | Parse an element with the same tag, optionally matching inner content.

@since 0.1.0.0
-}
sameElTag :: (ShowHTML a, Stream s m Char) => Elem -> Maybe (ParsecT s u m a) -> ParsecT s u m (Elem' a)
sameElTag tagName parser = elemParser (Just [tagName]) parser []

{- | Parse an element with the same tag and return only its inner matches.

@since 0.1.0.0
-}
matchesInSameElTag :: (ShowHTML a, Stream s m Char) => Elem -> Maybe (ParsecT s u m a) -> ParsecT s u m [a]
matchesInSameElTag tagName parser = do
    parsedEl <- elemParser (Just [tagName]) parser []
    return $ matches' parsedEl

{- | List of HTML self-closing (void) element tag names.

@since 0.1.0.0
-}
selfClosing :: [String]
selfClosing = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

{- | Parse a self-closing element, returning an 'Elem'' with empty inner content.

@since 0.1.0.0
-}
elSelfC :: (Stream s m Char) => Maybe [Elem] -> [(String, Maybe String)] -> ParsecT s u m (Elem' a)
elSelfC elemOpts attrsSubset = do
    (tag, parsedAttrs) <- parseOpeningTag elemOpts attrsSubset
    return $ Elem' tag parsedAttrs mempty mempty

{- | Parse a self-closing element, failing if an inner content parser is specified.

@since 0.1.0.0
-}
elSelfClosing :: (Stream s m Char) => Maybe [Elem] -> Maybe (ParsecT s u m a) -> [(String, Maybe String)] -> ParsecT s u m (Elem' a)
elSelfClosing elemOpts innerSpec attrsSubset = do
    (tag, parsedAttrs) <- parseOpeningTag elemOpts attrsSubset
    case innerSpec of
        Just _ -> parserZero
        Nothing -> return $ Elem' tag parsedAttrs mempty mempty

{- | Parse an element with a body, requiring at least one inner match when an inner parser is given.

@since 0.1.0.0
-}
elemWithBody ::
    (ShowHTML a, Stream s m Char) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (Elem' a)
elemWithBody elemList innerSpec attrSpec = do
    e <- elemParserInternal elemList innerSpec attrSpec
    when (length (matches' e) < (case innerSpec of Nothing -> 0; _ -> 1)) (parserFail "not enough matches")
    return e

{- | Internal element parser that handles self-closing fallback and inner content parsing.

@since 0.1.0.0
-}
elemParserInternal ::
    (ShowHTML a, Stream s m Char) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (Elem' a)
elemParserInternal elemList innerSpec attrSpec = do
    (elem', attrs') <- parseOpeningTag elemList attrSpec
    (asString, matchList) <-
        fmap (foldr foldFuncTup mempty) $
            (try (string "/>") >> return [])
                <|> (try $ innerElemParser elem' innerSpec)
                <|> (selfClosingTextful innerSpec)
    return $ Elem' elem' attrs' matchList asString

{- | Parse inner HTML content between an opening and closing tag, collecting matches,
nested same-elements, styling inlines, and raw characters.

@since 0.1.0.0
-}
innerElemParser ::
    (ShowHTML a, Stream s m Char) =>
    String ->
    Maybe (ParsecT s u m a) ->
    ParsecT s u m [HTMLMatcher Elem' a]
innerElemParser eTag innerSpec =
    char '>'
        >> manyTill
            ( try (Match <$> (fromMaybe parserZero innerSpec))
                <|> (try (IText <$> stylingElem))
                <|> try (Element <$> sameElTag eTag innerSpec)
                <|> ((IText . (: [])) <$> anyChar)
            )
            (endTag eTag)

{- | HTML tags that style inline text without changing document structure
(e.g. @\<b\>@, @\<em\>@, @\<strong\>@).

@since 0.1.0.0
-}
stylingTags :: [String]
stylingTags = ["abbr", "b", "big", "acronym", "dfn", "em", "font", "i", "mark", "q", "small", "strong"]

{- | Parse a styling element and return its inner text content.

@since 0.1.0.0
-}
stylingElem :: (Stream s m Char) => ParsecT s u m String
stylingElem = do
    (e, _) <- parseOpeningTag (Just stylingTags) []
    _ <- char '>'
    fmap fst $ manyTill_ anyChar (endTag e)

{- | Parse inner HTML and the closing tag for a given element, returning matches and full inner text.

@since 0.1.0.0
-}
{-# DEPRECATED parseInnerHTMLAndEndTag "use new elem parser directly" #-}
parseInnerHTMLAndEndTag ::
    (Stream s m Char) =>
    Elem ->
    Maybe (ParsecT s u m String) ->
    ParsecT s u m (InnerTextResult String)
parseInnerHTMLAndEndTag tagName innerPattern = do
    let getPattern :: (Stream s m Char) => Maybe (ParsecT s u m String) -> ParsecT s u m String
        getPattern x = case x of
            Just pat -> pat
            Nothing -> string ""

        parseElTag :: (Stream s m Char) => ParsecT s u m String
        parseElTag = do
            parsedEl <- elemParserOld (Just [tagName]) Nothing []
            return $ showH parsedEl

        p :: (Stream s m Char) => ParsecT s u m String
        p = do
            c <- anyToken
            return (c : [])

        baseParser :: (Stream s m Char) => Maybe (ParsecT s u m String) -> ParsecT s u m String -> ParsecT s u m (InnerTextResult String)
        baseParser innerPat endParse = do
            _ <- char '>'
            (pre, patternFound) <- manyTill_ (try parseElTag <|> p) (getPattern innerPat)
            (post, _) <- manyTill_ (try parseElTag <|> p) endParse

            return $
                InnerTextResult
                    { _matchesITR = [patternFound]
                    , _fullInner = mconcat pre <> patternFound <> mconcat post
                    }

        anyTagInner :: (Stream s m Char) => Maybe (ParsecT s u m String) -> ParsecT s u m (InnerTextResult String)
        anyTagInner innerP =
            baseParser
                innerP
                ( try
                    ( char '<'
                        >> optional (char '/')
                        >> some anyChar
                        >> (string " " <|> string ">")
                    )
                )

        normal :: (Stream s m Char) => Maybe (ParsecT s u m String) -> ParsecT s u m (InnerTextResult String)
        normal innerP = baseParser innerP (try (string ("</" <> tagName <> ">")))

    x <- eitherP (try (string "/>")) (normal innerPattern <|> anyTagInner innerPattern)
    case x of
        Left _ ->
            case innerPattern of
                Just _ -> parserZero
                Nothing ->
                    pure InnerTextResult{_matchesITR = [], _fullInner = ""}
        Right b -> return b

{- | Original element parser that delegates to 'parseInnerHTMLAndEndTag'.

@since 0.1.0.0
-}
{-# DEPRECATED elemParserOld "use elemParser" #-}
elemParserOld ::
    (Stream s m Char) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m String) ->
    [(String, Maybe String)] ->
    ParsecT s u m (Elem' String)
elemParserOld elemList innerSpec attrSpec = do
    (elem', attrs') <- parseOpeningTag elemList attrSpec
    inner <- parseInnerHTMLAndEndTag elem' innerSpec
    return $ Elem' elem' attrs' (_matchesITR inner) (_fullInner inner)
