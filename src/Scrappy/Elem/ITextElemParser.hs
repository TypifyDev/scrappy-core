{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Elem.ITextElemParser
Description : NLP-oriented text parsers for extracting structured written content from HTML
Copyright   : (c) Galen Sprout 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com

Provides parsers that bridge NLP concepts (sentences, paragraphs, written words)
with HTML element parsing, enabling extraction of structured written content
from raw HTML streams.
-}
module Scrappy.Elem.ITextElemParser (
    -- * Result types

    -- | @since 0.1.0.0
    ResearchResult,
    -- | @since 0.1.0.0
    Paragraph (..),
    -- | @since 0.1.0.0
    Sentence (..),
    -- | @since 0.1.0.0
    WrittenWord (..),
    -- | @since 0.1.0.0
    Html,
    -- | @since 0.1.0.0
    AccumITextElem (..),

    -- * Type classes

    -- | @since 0.1.0.0
    Zero (..),
    -- | @since 0.1.0.0
    Singleton (..),
    -- | @since 0.1.0.0
    Multiple (..),
    -- | @since 0.1.0.0
    Existential (..),

    -- * Tree parsers

    -- | @since 0.1.0.0
    emptyTree,
    -- | @since 0.1.0.0
    preface,
    -- | @since 0.1.0.0
    emptyTreeGroup,
    -- | @since 0.1.0.0
    elemAny,

    -- * Written-language parsers

    -- | @since 0.1.0.0
    punctuation,
    -- | @since 0.1.0.0
    writtenWord,
    -- | @since 0.1.0.0
    wordSeparator,
    -- | @since 0.1.0.0
    comma,
    -- | @since 0.1.0.0
    colon,
    -- | @since 0.1.0.0
    semiColon,
    -- | @since 0.1.0.0
    word',
    -- | @since 0.1.0.0
    capitalizedWord,
    -- | @since 0.1.0.0
    number,
    -- | @since 0.1.0.0
    sentence,
    -- | @since 0.1.0.0
    sentenceWhere,
    -- | @since 0.1.0.0
    sentenceTail,

    -- * Text utilities

    -- | @since 0.1.0.0
    styleTags,
    -- | @since 0.1.0.0
    negParseOpeningTag,
    -- | @since 0.1.0.0
    textChunk,
    -- | @since 0.1.0.0
    openOrCloseTag,
    -- | @since 0.1.0.0
    anyEndTag,
    -- | @since 0.1.0.0
    anyThingbut,
    -- | @since 0.1.0.0
    textChunkIf,
    -- | @since 0.1.0.0
    plainText,
    -- | @since 0.1.0.0
    styleElem,
    -- | @since 0.1.0.0
    removeStyleTags,
    -- | @since 0.1.0.0
    catEithers,
    -- | @since 0.1.0.0
    divideUp,
    -- | @since 0.1.0.0
    onlyPlainText,
    -- | @since 0.1.0.0
    textOnlyFoldr,
) where

import Prelude (
    Bool (..),
    Char,
    Either (..),
    Maybe (..),
    Monoid (..),
    Semigroup (..),
    Show (..),
    String,
    const,
    either,
    elem,
    fmap,
    foldr,
    id,
    not,
    null,
    pure,
    return,
    reverse,
    snd,
    ($),
    (.),
    (<$>),
    (<*),
    (<*>),
    (>>),
    (>>=),
 )

import Scrappy.Elem.ElemHeadParse (attrsParser, buildElemsOpts, parseOpeningTag)
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Elem.TreeElemParser (treeElemParser)
import Scrappy.Elem.Types (
    Attrs,
    Elem,
    Elem' (..),
    HTMLMatcher (..),
    ShowHTML (..),
    TreeHTML (..),
    attrs,
    coerceAttrs,
    elTag,
    endTag,
    innerText',
    noPat,
    selfClosingTextful,
    _innerTree',
 )

import Control.Applicative.Combinators (eitherP, some)
import Control.Monad (void, when)
import Data.Either (fromRight, isRight)
import Data.List (intercalate)
import Text.Parsec (
    ParsecT,
    Stream,
    alphaNum,
    anyChar,
    char,
    digit,
    letter,
    many,
    manyTill,
    oneOf,
    option,
    optional,
    parse,
    parserZero,
    space,
    string,
    try,
    (<|>),
 )

import Scrappy.Elem.ChainHTML (manyTill_, nl)

{- | Parse an HTML tree element that has no inner subtrees, optionally
constraining the element tag and attributes.

@since 0.1.0.0
-}
emptyTree ::
    (ShowHTML a, Stream s m Char) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (TreeHTML a)
emptyTree elemOpts match attrsSpec = do
    e <- treeElemParser elemOpts match attrsSpec
    when (not $ null $ _innerTree' e) $ parserZero
    pure e

{- | Skip content until the given parser matches, returning only the
matched result.

@since 0.1.0.0
-}
preface ::
    (Stream s m Char) =>
    ParsecT s u m pre ->
    ParsecT s u m a ->
    ParsecT s u m a
preface _pre p = p >> (fmap snd $ manyTill_ anyChar p)

{- | Values that can be consumed at zero cardinality.

@since 0.1.0.0
-}
class Zero a where
    consumeZero :: a -> b -> b

{- | Values that can be consumed as a singleton.

@since 0.1.0.0
-}
class Singleton a where
    consumeSingleton :: a -> b

{- | Values that require two or more elements to be consumed.

@since 0.1.0.0
-}
class Multiple a where
    consumeMultiple :: a -> b

{- | Values that support consumption at any cardinality.

@since 0.1.0.0
-}
class (Zero a, Singleton a, Multiple a) => Existential a where
    consumeExists :: a -> b

{- | Parse a group of consecutive empty-tree elements that share the same
tag and attributes as the first match. The result list is always non-empty.

@since 0.1.0.0
-}
emptyTreeGroup ::
    (ShowHTML a, Stream s m Char) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m [TreeHTML a]
emptyTreeGroup elemOpts match attrsSubset = do
    e <- emptyTree elemOpts match attrsSubset
    let
        same = emptyTree (Just [elTag e]) match (coerceAttrs $ attrs e)
    (:) <$> pure e <*> (many $ same <* nl)

{- | Parse any element with no constraints on tag, attributes, or inner content.

@since 0.1.0.0
-}
elemAny :: (Stream s m Char) => ParsecT s u m (Elem' String)
elemAny = elemParser Nothing noPat []

{- | Just for testing.

@since 0.1.0.0
-}
type ResearchResult = String

{- | A paragraph consisting of a sequence of sentences.

@since 0.1.0.0
-}
data Paragraph = Paragraph {unParagraph :: [Sentence]}

{- | A sentence consisting of a sequence of written words.

@since 0.1.0.0
-}
data Sentence = Sentence {unSentence :: [WrittenWord]}

{- | A single written word token.

@since 0.1.0.0
-}
data WrittenWord = WW {unWord :: String}

-- | @since 0.1.0.0
instance Show Paragraph where
    show (Paragraph sentences) = intercalate " " $ show <$> sentences

-- | @since 0.1.0.0
instance Show Sentence where
    show (Sentence ws) = (intercalate " " $ show <$> ws) <> "."

-- | @since 0.1.0.0
instance Show WrittenWord where
    show (WW s) = s

-- | @since 0.1.0.0
instance Semigroup WrittenWord where
    (WW w1) <> (WW w2) = WW $ w1 <> " " <> w2

{- | Technically this should not exist ever.

@since 0.1.0.0
-}
instance Monoid WrittenWord where
    mempty = WW ""

-- | @since 0.1.0.0
instance Semigroup Sentence where
    (Sentence s1) <> (Sentence s2) = Sentence $ s1 <> s2

-- | @since 0.1.0.0
instance Monoid Sentence where
    mempty = Sentence []

-- | @since 0.1.0.0
instance Semigroup Paragraph where
    (Paragraph p) <> (Paragraph p2) = Paragraph $ p <> p2

-- | @since 0.1.0.0
instance Monoid Paragraph where
    mempty = Paragraph []

-- | @since 0.1.0.0
instance ShowHTML Paragraph where
    showH (Paragraph s) = mconcat $ fmap showH s

-- | @since 0.1.0.0
instance ShowHTML Sentence where
    showH (Sentence ws) = intercalate "" (fmap unWord ws) <> "."

{- | Parse a single punctuation character.

@since 0.1.0.0
-}
punctuation :: (Stream s m Char) => ParsecT s u m Char
punctuation = oneOf [';', ':', '(', ')', '\"', '\'', '-', ',']

{- | Parse a written word: one or more alphanumeric or punctuation characters,
optionally followed by a space.

@since 0.1.0.0
-}
writtenWord :: (Stream s m Char) => ParsecT s u m WrittenWord
writtenWord = WW <$> (some $ alphaNum <|> punctuation) <* optional (char ' ')

{- | Parse a word separator: a space, comma, colon, or semicolon.

@since 0.1.0.0
-}
wordSeparator :: (Stream s m Char) => ParsecT s u m String
wordSeparator = ((: []) <$> space) <|> comma <|> colon <|> semiColon

{- | Parse a comma, optionally followed by a space.

@since 0.1.0.0
-}
comma :: (Stream s m Char) => ParsecT s u m String
comma = do
    c <- char ','
    s <- option "" $ (: []) <$> space
    pure $ c : s

{- | Parse a colon, optionally followed by a space.

@since 0.1.0.0
-}
colon :: (Stream s m Char) => ParsecT s u m String
colon = do
    c <- char ':'
    s <- option "" $ (: []) <$> space
    pure $ c : s

{- | Parse a semicolon, optionally followed by a space.

@since 0.1.0.0
-}
semiColon :: (Stream s m Char) => ParsecT s u m String
semiColon = do
    c <- char ';'
    s <- option "" $ (: []) <$> space
    pure $ c : s

{- | Parse a word token: one or more letters, apostrophes, or hyphens.
Handles the special case where the word starts with @\'a\'@.

@since 0.1.0.0
-}
word' :: (Stream s m Char) => ParsecT s u m String
word' = a_ <|> else'
  where
    a_ = do
        head_ <- char 'a'
        tail_ <- many $ letter <|> (char '\'') <|> (char '-')
        pure $ head_ : tail_

    else' = some (letter <|> (char '\'') <|> (char '-'))

{- | Parse a capitalized word: starts with an uppercase letter followed by
additional letters, apostrophes, or hyphens.

@since 0.1.0.0
-}
capitalizedWord :: (Stream s m Char) => ParsecT s u m String
capitalizedWord = try ia <|> else'
  where
    ia = do
        head_ <- oneOf ['I', 'A']
        tail_ <- many $ letter <|> (char '\'') <|> (char '-')
        pure $ head_ : tail_
    else' = do
        head_ <- oneOf $ ['B' .. 'H'] <> ['J' .. 'Z']
        tail_ <- some (letter <|> (char '\'') <|> (char '-'))
        pure $ head_ : tail_

{- | Parse a number: one or more digits, optionally followed by a decimal
point and more digits.

@since 0.1.0.0
-}
number :: (Stream s m Char) => ParsecT s u m String
number = do
    whole <- some digit
    dec <- option "" $ do
        (:) <$> char '.' <*> some digit
    pure $ whole <> dec

{- | Parse a sentence with no additional filtering.

@since 0.1.0.0
-}
sentence :: (Stream s m Char) => ParsecT s u m Sentence
sentence = sentenceWhere (const True)

{- | Parse a sentence, succeeding only when the collected words satisfy
the given predicate.

@since 0.1.0.0
-}
sentenceWhere :: (Stream s m Char) => ([WrittenWord] -> Bool) -> ParsecT s u m Sentence
sentenceWhere testFn = do
    tokens <-
        eitherP capitalizedWord number >>= \case
            Left word -> do
                eitherP wordSeparator (char '.') >>= \case
                    Right _period -> pure [WW word]
                    Left separator -> (WW (word <> separator) :) <$> sentenceTail False
            Right num -> do
                eitherP wordSeparator (char '.') >>= \case
                    Right _period -> pure [WW num]
                    Left separator -> (WW (num <> separator) :) <$> sentenceTail True

    toSentence testFn tokens
  where
    toSentence :: (Stream s m Char) => ([WrittenWord] -> Bool) -> [WrittenWord] -> ParsecT s u m Sentence
    toSentence fn ws = case fn ws of
        False -> parserZero
        True -> pure $ Sentence ws

{- | Parse the tail of a sentence (everything after the first word).
The boolean tracks whether the previous token was a number.

@since 0.1.0.0
-}
sentenceTail :: (Stream s m Char) => Bool -> ParsecT s u m [WrittenWord]
sentenceTail previousWasNumber = do
    token <- case previousWasNumber of
        True -> Left <$> word'
        False -> eitherP word' number
    eitherP wordSeparator (char '.') >>= \case
        Left separator -> do
            tokens <- sentenceTail $ isRight token
            pure $ WW (either id id token <> separator) : tokens
        Right _period -> pure $ [WW $ either id id token]

{- | List of HTML tags considered \"style\" tags (bold, italic, etc.).

@since 0.1.0.0
-}
styleTags :: [String]
styleTags = ["b", "strong", "i", "em", "mark", "small", "ins", "sub", "sup"]

{- | Parse an opening tag that does NOT match any of the given element names.

@since 0.1.0.0
-}
negParseOpeningTag :: (Stream s m Char) => [Elem] -> ParsecT s u m (Elem, Attrs)
negParseOpeningTag elemOpts = do
    _ <- char '<'
    tag <- some alphaNum
    when (tag `Prelude.elem` elemOpts) parserZero
    parsedAttrs <- attrsParser []
    pure $ (tag, fromRight mempty parsedAttrs)

{- | Parse a chunk of text, consuming plain text until a non-style
opening or closing tag is encountered.

@since 0.1.0.0
-}
textChunk :: (Stream s m Char) => ParsecT s u m String
textChunk = fmap mconcat $ manyTill plainText (try $ openOrCloseTag)

{- | Match any element open or closing tag that is not a style tag.

@since 0.1.0.0
-}
openOrCloseTag :: (Stream s m Char) => ParsecT s u m ()
openOrCloseTag =
    void $ (try $ negParseOpeningTag styleTags >> char '>') <|> anyEndTag

{- | Parse any closing tag that is not a style tag.

@since 0.1.0.0
-}
anyEndTag :: (Stream s m Char) => ParsecT s u m Char
anyEndTag = do
    string "</" >> anyThingbut styleTags >> char '>'

{- | Parse one or more alphanumeric characters, failing if the result
matches any of the given strings.

@since 0.1.0.0
-}
anyThingbut :: (Stream s m Char) => [String] -> ParsecT s u m String
anyThingbut es = do
    txt <- some alphaNum
    when (txt `Prelude.elem` es) $ parserZero
    pure txt

{- | Parse a text chunk, succeeding only if the result passes the given predicate.

@since 0.1.0.0
-}
textChunkIf :: (Stream s m Char) => (String -> Bool) -> ParsecT s u m String
textChunkIf f = do
    x <- textChunk
    when (not $ f x) $ parserZero
    pure x

{- | Parse plain text, transparently consuming style elements and
extracting their inner text.

@since 0.1.0.0
-}
plainText :: (Stream s m Char) => ParsecT s u m String
plainText = do
    unit <- innerText' <$> styleElem <|> (fmap (: []) anyChar)
    pure unit

{- | Parse a style element (bold, italic, etc.).

@since 0.1.0.0
-}
styleElem :: (Stream s m Char) => ParsecT s u m (Elem' String)
styleElem = elemParser (Just $ styleTags) noPat []

{- | Type alias for raw HTML strings.

@since 0.1.0.0
-}
type Html = String

{- | Strip all style tags from HTML, keeping their inner text content.

@since 0.1.0.0
-}
removeStyleTags :: Html -> Html
removeStyleTags html = (mconcat . catEithers) $ fromRight [] $ parse (divideUp expr) "" html
  where
    expr =
        (fmap show $ parseOpeningTag (Just styleTags) [])
            <|> (string "</" >> buildElemsOpts styleTags >> string ">")

{- | Extract only the 'Right' values from a list of 'Either's.

@since 0.1.0.0
-}
catEithers :: [Either e a] -> [a]
catEithers [] = []
catEithers (x : xs) = case x of
    Right a -> a : catEithers xs
    Left _ -> catEithers xs

{- | Partition a stream into matched ('Right') and unmatched ('Left')
segments using the given parser.

@since 0.1.0.0
-}
divideUp :: (Stream s m Char) => ParsecT s u m String -> ParsecT s u m [Either String String]
divideUp parser = many ((Right <$> parser) <|> ((Left . (: [])) <$> anyChar))

{- | Parse an HTML document and extract only its plain text content,
stripping all tags.

@since 0.1.0.0
-}
onlyPlainText :: (Stream s m Char) => ParsecT s u m String
onlyPlainText = fmap (\(ACT strings) -> mconcat $ reverse strings) specialElemParser
  where
    specialElemParser :: (Stream s m Char) => ParsecT s u m (AccumITextElem String)
    specialElemParser = do
        (elem', _) <- parseOpeningTag (Just ["html"]) []
        (localText, inTex) <-
            fmap (foldr textOnlyFoldr mempty) $
                (try (string "/>") >> return [])
                    <|> (try $ innerElemParser' elem')
                    <|> (selfClosingTextful Nothing)
        return $ ACT (localText : inTex)
      where
        innerElemParser' eTag =
            char '>'
                >> manyTill
                    ( Element <$> (try specialElemParser)
                        <|> ((IText . (: [])) <$> anyChar)
                    )
                    (endTag eTag)

{- | Accumulator for collecting inner text from parsed elements.

@since 0.1.0.0
-}
data AccumITextElem a = ACT [String]

{- | Fold function that separates inner text from element text during
accumulation.

@since 0.1.0.0
-}
textOnlyFoldr :: HTMLMatcher AccumITextElem String -> (String, [String]) -> (String, [String])
textOnlyFoldr htmlM (itextAccum, fromElemAccum) = case htmlM of
    IText str ->
        (itextAccum <> str, fromElemAccum)
    Element (ACT strList) ->
        (itextAccum, fromElemAccum <> strList)
    Match mat ->
        (itextAccum <> mat, fromElemAccum)
