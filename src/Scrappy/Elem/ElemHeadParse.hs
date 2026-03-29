{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Elem.ElemHeadParse
Description : Parsec parsers for HTML opening tags, attributes, and element heads
Copyright   : (c) Galen Sprout 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com
-}
module Scrappy.Elem.ElemHeadParse (
    -- | @since 0.1.0.0
    href,
    -- | @since 0.1.0.0
    href',
    -- | @since 0.1.0.0
    parseAttrSafe,
    -- | @since 0.1.0.0
    hrefParser,
    -- | @since 0.1.0.0
    parseOpeningTagF,
    -- | @since 0.1.0.0
    parseOpeningTagWhere,
    -- | @since 0.1.0.0
    attrValue,
    -- | @since 0.1.0.0
    attrValuesExist,
    -- | @since 0.1.0.0
    attrValueExists,
    -- | @since 0.1.0.0
    attrName,
    -- | @since 0.1.0.0
    attrParser,
    -- | @since 0.1.0.0
    attrsParser,
    -- | @since 0.1.0.0
    isAttrsMatch',
    -- | @since 0.1.0.0
    isAttrsMatch,
    -- | @since 0.1.0.0
    attrsFit,
    -- | @since 0.1.0.0
    attrsMatch',
    -- | @since 0.1.0.0
    attrsMatch,
    -- | @since 0.1.0.0
    attrsParserDesc,
    -- | @since 0.1.0.0
    parseOpeningTagDesc,
    -- | @since 0.1.0.0
    digitEq,
    -- | @since 0.1.0.0
    saveDigitEq,
    -- | @since 0.1.0.0
    svDigEq,
    -- | @since 0.1.0.0
    digitEqFree,
    -- | @since 0.1.0.0
    unfit,
    -- | @since 0.1.0.0
    mkAttrsDesc,
    -- | @since 0.1.0.0
    parseOpeningTag,
    -- | @since 0.1.0.0
    mkElemtagParser,
    -- | @since 0.1.0.0
    buildElemsOpts,
) where

import Prelude (
    Bool (..),
    Char,
    Either (..),
    Maybe (..),
    String,
    elem,
    fmap,
    fst,
    length,
    otherwise,
    return,
    show,
    snd,
    undefined,
    ($),
    (&&),
    (-),
    (.),
    (/=),
    (<>),
    (==),
    (>>),
    (||),
 )

import Scrappy.Elem.Types (Attrs, AttrsError (IncorrectAttrs), Elem, ElemHead, getHrefAttrs)
import Scrappy.Links (CurrentUrl, LastUrl, Link)

import Control.Applicative (some)
import qualified Data.Map as Map (Map, fromList, lookup, toList)
import Data.Maybe (fromMaybe)
import Text.Parsec (
    ParsecT,
    Stream,
    alphaNum,
    between,
    char,
    many,
    noneOf,
    option,
    optional,
    parserFail,
    parserZero,
    space,
    string,
    try,
    (<|>),
 )

import Scrappy.Types (mapMaybe)

{- | Parse an href link from an anchor tag. Uses 'mapMaybe' to extract the href
attribute after parsing the full opening tag.

@since 0.1.0.0
-}
href :: (Stream s m Char) => Bool -> LastUrl -> ParsecT s u m Link
href booly cUrl = ((getHrefAttrs booly cUrl) . snd) `mapMaybe` (parseOpeningTag (Just ["a"]) [])

{- | Parse an href link with an optional current URL context.

@since 0.1.0.0
-}
href' :: (Stream s m Char) => Maybe CurrentUrl -> ParsecT s u m Link
href' = undefined

{- | Safely parse an attribute value by first parsing the entire opening tag,
then looking up the requested attribute name. Designed for use in
@findSomeHtml@.

@since 0.1.0.0
-}
parseAttrSafe :: (Stream s m Char) => String -> ParsecT s u m String
parseAttrSafe attrNm = do
    tag <- parseOpeningTag Nothing [(attrNm, Nothing)]
    case (Map.lookup attrNm . snd) tag of
        Nothing -> parserZero
        Just a -> return a

{- | Parse an href attribute value from any opening tag that contains one.
Parses the full opening tag first to avoid false positives.

@since 0.1.0.0
-}
hrefParser :: (Stream s m Char) => ParsecT s u m String
hrefParser = do
    tag <- parseOpeningTag Nothing [("href", Nothing)]
    case (Map.lookup "href" . snd) tag of
        Nothing -> parserZero
        Just a -> return a

{- | Parse an opening tag that contains a specific attribute matching a predicate.
Returns the full 'ElemHead' if the predicate succeeds.

@since 0.1.0.0
-}
parseOpeningTagF :: (Stream s m Char) => String -> (String -> Bool) -> ParsecT s u m ElemHead
parseOpeningTagF attrib predicate = do
    (e, as) <- parseOpeningTag Nothing [(attrib, Nothing)]
    case Map.lookup attrib as of
        Nothing -> parserZero
        Just a -> if predicate a then return (e, as) else parserFail "couldnt find match parseOpeningTagF"

{- | Parse an opening tag from a set of allowed elements where a specific
attribute matches a predicate. Generalization of 'parseOpeningTagF' with
element filtering.

@since 0.1.0.0
-}
parseOpeningTagWhere ::
    (Stream s m Char) =>
    Maybe [Elem] ->
    String ->
    (String -> Bool) ->
    ParsecT s u m ElemHead
parseOpeningTagWhere es attrib predicate = do
    (e, as) <- parseOpeningTag es [(attrib, Nothing)]
    case Map.lookup attrib as of
        Nothing -> parserZero
        Just a -> if predicate a then return (e, as) else parserFail "couldnt find match parseOpeningTagF"

{- | Parse an HTML attribute value enclosed in double or single quotes.

@since 0.1.0.0
-}
attrValue :: (Stream s m Char) => ParsecT s u m [Char]
attrValue =
    (between (char '"') (char '"') (many (noneOf ['"'])))
        <|> (between (char '\'') (char '\'') (many (noneOf ['\''])))

{- | Check whether all desired attributes exist in a list of parsed attribute pairs.

@since 0.1.0.0
-}
attrValuesExist :: [(String, String)] -> [(String, Maybe String)] -> Bool
attrValuesExist _ [] = True
attrValuesExist attrsOut (nextAttr : attrsIn)
    | attrValueExists attrsOut nextAttr = True && (attrValuesExist attrsOut attrsIn)
    | otherwise = False

{- | Check whether a single desired attribute exists in a list of parsed
attribute pairs. Matches by name only when the desired value is 'Nothing',
or by both name and value when a value is specified.

@since 0.1.0.0
-}
attrValueExists :: [(String, String)] -> (String, Maybe String) -> Bool
attrValueExists [] _ = False
attrValueExists (attrF : attrsOut) nextAttr
    | fst attrF == fst nextAttr && snd nextAttr == Nothing = True
    | fst attrF == fst nextAttr && snd nextAttr == (Just (snd attrF)) = True
    | otherwise = attrValueExists attrsOut nextAttr

{- | Parse an HTML attribute name (alphanumeric characters, hyphens, underscores).

@since 0.1.0.0
-}
attrName :: (Stream s m Char) => ParsecT s u m String
attrName = some (alphaNum <|> char '-' <|> char '_')

{- | Parse a single HTML attribute as a key-value pair. Handles optional values
(attributes without @=value@).

@since 0.1.0.0
-}
attrParser :: (Stream s m Char) => ParsecT s u m (String, String)
attrParser = do
    _ <- many (space <|> char '\n' <|> char '\t')
    attrName' <- attrName
    content <- option "" (char '=' >> attrValue)
    return (attrName', content)

{- | Parse all attributes from an opening tag and validate that the desired
attribute subset is present. Returns 'Left' 'IncorrectAttrs' on mismatch.

@since 0.1.0.0
-}
attrsParser ::
    (Stream s m Char) =>
    [(String, Maybe String)] ->
    ParsecT s u m (Either AttrsError (Map.Map String String))
attrsParser attrs = do
    attrPairs <- many $ try attrParser
    let
        attrPairsMap = Map.fromList attrPairs
    case isAttrsMatch attrPairsMap attrs of
        True -> return $ Right attrPairsMap
        False -> return $ Left IncorrectAttrs

{- | Check whether parsed attributes match a list of desired attributes.
Variant that uses guard-style pattern matching.

@since 0.1.0.0
-}
isAttrsMatch' :: Map.Map String String -> [(String, Maybe String)] -> Bool
isAttrsMatch' _ [] = True
isAttrsMatch' mapAttr ((name, maybeVal) : desired)
    | Map.lookup name mapAttr == Nothing = False
    | (Map.lookup name mapAttr == maybeVal) || (maybeVal == Nothing) = isAttrsMatch mapAttr desired
    | otherwise = False

{- | Check whether parsed attributes match a list of desired attributes.
Matches by name only when the desired value is 'Nothing', or by both
name and value otherwise.

@since 0.1.0.0
-}
isAttrsMatch :: Map.Map String String -> [(String, Maybe String)] -> Bool
isAttrsMatch _ [] = True
isAttrsMatch mapAttr ((name, maybeVal) : desired) = case maybeVal of
    Just val ->
        case Map.lookup name mapAttr of
            Just valFromKey -> if val /= valFromKey then False else isAttrsMatch mapAttr desired
            Nothing -> False
    Nothing ->
        case Map.lookup name mapAttr of
            Just _ {- we only care about the name -} -> isAttrsMatch mapAttr desired
            Nothing -> False

{- | Check whether parsed attributes satisfy a list of predicate tests.
Each entry is an attribute name paired with a predicate on its value.

@since 0.1.0.0
-}
attrsFit :: Map.Map String String -> [(String, (String -> Bool))] -> Bool
attrsFit _ [] = True
attrsFit mapppy ((name, test) : rest) =
    (fromMaybe False $ fmap test $ Map.lookup name mapppy) && attrsFit mapppy rest

{- | Check whether all attributes from one map match those in another map.
Wrapper around 'attrsMatch' that converts the first map to a list.

@since 0.1.0.0
-}
attrsMatch' :: Map.Map String String -> Map.Map String String -> Bool
attrsMatch' a b = attrsMatch (Map.toList a) b

{- | Check whether all key-value pairs match entries in a map. For attributes
named @title@, @alt@, or @href@, only the presence of the key is checked.
For other attributes, 'digitEqFree' is used for fuzzy numeric matching.

@since 0.1.0.0
-}
attrsMatch :: [(String, String)] -> Map.Map String String -> Bool
attrsMatch [] _ = True
attrsMatch ((k, v) : kvs) mappy = case Map.lookup k mappy of
    Just val ->
        if elem k ["title", "alt", "href"]
            then True && attrsMatch kvs mappy
            else digitEqFree v val && attrsMatch kvs mappy
    Nothing -> False

{- | Parse attributes from an opening tag using descriptive matching.
On mismatch, fails with a message showing which attributes did not fit.

@since 0.1.0.0
-}
attrsParserDesc ::
    (Stream s m Char) =>
    [(String, String)] ->
    ParsecT s u m (Map.Map String String)
attrsParserDesc attrs = do
    attrPairs <- many attrParser
    let
        attrPairsMap = Map.fromList attrPairs
    if attrsMatch attrs attrPairsMap
        then return attrPairsMap
        else parserFail $ "incorrect attrs:" <> (show $ unfit attrs attrPairsMap)

{- | Parse a full opening tag with descriptive attribute matching.
Combines element tag parsing with 'attrsParserDesc'.

@since 0.1.0.0
-}
parseOpeningTagDesc :: (Stream s m Char) => Maybe [Elem] -> [(String, String)] -> ParsecT s u m (Elem, Attrs)
parseOpeningTagDesc elemOpts attrList = do
    _ <- char '<'
    el <- mkElemtagParser elemOpts
    parsedAttrs <- attrsParserDesc attrList
    return (el, parsedAttrs)

{- | Fuzzy digit equality. Allows certain degrees of freedom: digits in the
same position are considered equal, and strings may differ by one digit
in length (delegating to 'saveDigitEq').

@since 0.1.0.0
-}
digitEq :: String -> String -> Bool
digitEq [] [] = True
digitEq [] (_ : _) = False
digitEq (_ : _) [] = False
digitEq (charA : xs) (charB : ys) =
    if charA == charB
        then True && digitEq xs ys
        else
            if elem charA ['0' .. '9'] && elem charB ['0' .. '9']
                then digitEq xs ys
                else
                    saveDigitEq (charA : xs) (charB : ys)

{- | Handle the case where two strings differ in length by exactly one digit.
Used by 'digitEq' to allow a single extra digit.

@since 0.1.0.0
-}
saveDigitEq :: String -> String -> Bool
saveDigitEq [] _ = False
saveDigitEq _ [] = False
saveDigitEq as@(a : _) bs@(b : _) =
    if elem (length as - length bs) [1, -1]
        then
            if elem a ['0' .. '9'] || elem b ['0' .. '9']
                then svDigEq as bs
                else False
        else False

{- | Core implementation for 'saveDigitEq'. Attempts to skip one extra digit
in either string and continue with 'digitEq'.

@since 0.1.0.0
-}
svDigEq :: String -> String -> Bool
svDigEq [] _ = False
svDigEq _ [] = False
svDigEq (charA : as) (charB : bs) = case (as, bs) of
    (a : restA, _) | a == charB -> digitEq restA bs
    (_, b : restB) | b == charA -> digitEq as restB
    _ -> saveDigitEq (charA : as) bs || saveDigitEq as (charB : bs)

{- | Digit-free equality. Ignores all numeric digits in both strings and
compares only the non-digit characters.

@since 0.1.0.0
-}
digitEqFree :: [Char] -> [Char] -> Bool
digitEqFree [] [] = True
digitEqFree (a : rest) [] = if elem a ['0' .. '9'] then digitEqFree rest [] else False
digitEqFree [] (b : rest) = if elem b ['0' .. '9'] then digitEqFree [] rest else False
digitEqFree (a : restA) (b : restB) =
    if elem a ['0' .. '9']
        then digitEqFree restA (b : restB)
        else
            if elem b ['0' .. '9']
                then digitEqFree (a : restA) restB
                else
                    if a == b
                        then digitEqFree restA restB
                        else False

{- | Find attributes from the desired list that do not match the parsed
attribute map. Returns a list of @(description, reason)@ pairs explaining
each mismatch.

@since 0.1.0.0
-}
unfit :: [(String, String)] -> Map.Map String String -> [(String, String)]
unfit [] _ = []
unfit ((n, v) : ns) mappy = case Map.lookup n mappy of
    Nothing -> (n, "no attr") : unfit ns mappy
    Just val ->
        if elem n ["href", "alt", "title"]
            then unfit ns mappy
            else
                if digitEq v val
                    then unfit ns mappy
                    else (n <> ":" <> "(" <> val <> "|" <> v <> ")", "failed test") : unfit ns mappy

{- | Convert a list of @(name, value)@ attribute pairs into predicate-based
attribute descriptors using 'digitEqFree' for fuzzy matching.

@since 0.1.0.0
-}
mkAttrsDesc :: [(String, String)] -> [(String, (String -> Bool))]
mkAttrsDesc atrs = (fmap . fmap) digitEqFree atrs

{- | Parse an HTML opening tag, optionally restricting to specific element
names and requiring a subset of attributes. Returns the element name
and all parsed attributes on success.

@since 0.1.0.0
-}
parseOpeningTag :: (Stream s m Char) => Maybe [Elem] -> [(String, Maybe String)] -> ParsecT s u m (Elem, Attrs)
parseOpeningTag elemOpts attrsSubset = do
    _ <- char '<'
    el <- mkElemtagParser elemOpts
    parsedAttrs <- attrsParser attrsSubset
    _ <- optional (many space)
    case parsedAttrs of
        Left IncorrectAttrs -> parserZero
        Right whateva -> return (el, whateva)

{- | Build an element tag parser from an optional list of allowed element names.
When 'Nothing', accepts any alphanumeric tag name. When @Just elems@, only
accepts tags matching one of the given names.

@since 0.1.0.0
-}
mkElemtagParser :: (Stream s m Char) => Maybe [Elem] -> ParsecT s u m String
mkElemtagParser x = case x of
    Nothing -> some (alphaNum <|> char '-')
    Just elemsOpts -> buildElemsOpts elemsOpts

{- | Build a parser that accepts any of the given element names using
backtracking ('try') and alternation.

@since 0.1.0.0
-}
buildElemsOpts :: (Stream s m Char) => [Elem] -> ParsecT s u m String
buildElemsOpts [] = parserZero
buildElemsOpts (x : elemsAllow) = try (string x) <|> (buildElemsOpts elemsAllow)
