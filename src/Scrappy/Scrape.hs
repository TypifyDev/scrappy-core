{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Scrape
Description : High-level scraping interface combining parsers with HTML input
Copyright   : (c) Galen Sprout 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com
-}
module Scrappy.Scrape (
    -- * Core types

    -- | @since 0.1.0.0
    ScraperT,
    -- | @since 0.1.0.0
    Prefix,
    -- | @since 0.1.0.0
    Prefix',

    -- * Running scrapers

    -- | @since 0.1.0.0
    scrape,
    -- | @since 0.1.0.0
    scrapeIO,
    -- | @since 0.1.0.0
    scrapeFirst,
    -- | @since 0.1.0.0
    scrapeFirst',
    -- | @since 0.1.0.0
    scrapeBracketed,
    -- | @since 0.1.0.0
    scrapePrefixed,
    -- | @since 0.1.0.0
    runScraperOnHtml,
    -- | @since 0.1.0.0
    runScraperOnHtmlIO,
    -- | @since 0.1.0.0
    runScraperOnHtml1,
    -- | @since 0.1.0.0
    runScraperInBody,
    -- | @since 0.1.0.0
    runScraperOnBody,

    -- * Scraper combinators

    -- | @since 0.1.0.0
    scrapeLinked,
    -- | @since 0.1.0.0
    filterFromTextP,
    -- | @since 0.1.0.0
    filterPattern,
    -- | @since 0.1.0.0
    skipToInBody,
    -- | @since 0.1.0.0
    skipToBody,
    -- | @since 0.1.0.0
    findCount,

    -- * Utility functions

    -- | @since 0.1.0.0
    exists,
    -- | @since 0.1.0.0
    coerceMaybeParser,
    -- | @since 0.1.0.0
    hoistMaybe,
    -- | @since 0.1.0.0
    getFirstSafe,
    -- | @since 0.1.0.0
    getFirstFitSafe,
    -- | @since 0.1.0.0
    findFit,
) where

import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Either (either, fromRight)
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Text.Parsec (
    ParsecT,
    Stream,
    anyChar,
    char,
    many,
    manyTill,
    parse,
    parserZero,
    runParserT,
    string,
    try,
 )
import Text.Parsec.Text ()
import Prelude (
    Bool (..),
    Char,
    Int,
    Maybe (..),
    Show,
    String,
    const,
    fmap,
    id,
    length,
    maybe,
    mconcat,
    pure,
    return,
    undefined,
    ($),
    (.),
    (<$>),
    (<*>),
    (>>),
    (>>=),
 )

import Scrappy.Elem.ChainHTML ((</>>))
import Scrappy.Elem.ElemHeadParse (parseOpeningTag)
import Scrappy.Elem.SimpleElemParser (el)
import Scrappy.Find (findNaive, findNaiveIO)
import Scrappy.Types (Html)

{- | A scraper over 'Html' input with no user state, using 'Identity' as the base monad.

@since 0.1.0.0
-}
type ScraperT a = ParsecT Html () Identity a

{- | Generate a scraping expression where when found, it will generate and link in a data structure
to a relevant next pattern. For instance, an element of interest being found then switches the
scraper expression to be a reference to itself.

@since 0.1.0.0
-}
scrapeLinked :: ParsecT s u m a -> ParsecT s u m [String]
scrapeLinked = undefined

{- | Drop all occurrences of a given abstract pattern from text, returning the remaining text.

@since 0.1.0.0
-}
filterFromTextP :: ScraperT a -> ScraperT T.Text
filterFromTextP p = (many (try p)) >> (fmap T.pack $ (:) <$> anyChar <*> (T.unpack <$> filterFromTextP p))

{- | Filter a pattern from a string. This will never fail since it falls through to consuming
remaining characters.

@since 0.1.0.0
-}
filterPattern :: T.Text -> ScraperT a -> T.Text
filterPattern txt p = either undefined id $ parse (filterFromTextP p) "" txt

{- | Coerce a 'Maybe' value into a 'ScraperT' parser, failing with 'parserZero' on 'Nothing'.

@since 0.1.0.0
-}
coerceMaybeParser :: Maybe a -> ScraperT a
coerceMaybeParser = \case
    Just a -> return a
    Nothing -> parserZero

{- | Lift a 'Maybe' value into 'MaybeT'.

@since 0.1.0.0
-}
hoistMaybe :: (Applicative m) => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure

{- | Check whether a scraping pattern matches anywhere in the given HTML.

@since 0.1.0.0
-}
exists :: ScraperT a -> Html -> Bool
exists p html = maybe False (const True) $ runScraperOnHtml p html

{- | Run a scraper on HTML and return all matches. Alias for 'runScraperOnHtml'.

@since 0.1.0.0
-}
scrape :: ScraperT a -> Html -> Maybe [a]
scrape = runScraperOnHtml

{- | Run a scraper on HTML and return only the first match.

@since 0.1.0.0
-}
scrapeFirst' :: ScraperT a -> Html -> Maybe a
scrapeFirst' f h = case scrape f h of
    Just (x : _) -> return x
    _ -> Nothing

{- | Safely extract the first element from a 'Maybe' list.

@since 0.1.0.0
-}
getFirstSafe :: Maybe [a] -> Maybe a
getFirstSafe (Just (x : _)) = Just x
getFirstSafe _ = Nothing

{- | Safely extract the first element from a 'Maybe' list that satisfies a predicate.

@since 0.1.0.0
-}
getFirstFitSafe :: (a -> Bool) -> Maybe [a] -> Maybe a
getFirstFitSafe f (Just (x : xs)) = findFit f (x : xs)
getFirstFitSafe _ _ = Nothing

{- | Find the first element in a list that satisfies a predicate.

@since 0.1.0.0
-}
findFit :: (a -> Bool) -> [a] -> Maybe a
findFit _ [] = Nothing
findFit cond (x : xs) = if cond x then Just x else findFit cond xs

{- | Find all occurrences of a given parsing\/scraping pattern in HTML.

@since 0.1.0.0
-}
runScraperOnHtml :: ParsecT T.Text () Identity a -> T.Text -> Maybe [a]
runScraperOnHtml p html = fromRight Nothing $ parse (findNaive $ p) "" html

{- | Like 'runScraperOnHtml' but runs in a 'MonadIO' context.

@since 0.1.0.0
-}
runScraperOnHtmlIO :: (MonadIO m, Show a, Stream T.Text m Char) => ParsecT T.Text () m a -> T.Text -> m (Maybe [a])
runScraperOnHtmlIO p html = do
    x <- runParserT (findNaiveIO $ p) () "" html
    pure $ fromRight Nothing x

{- | Run a scraper in a 'MonadIO' context. Alias for 'runScraperOnHtmlIO'.

@since 0.1.0.0
-}
scrapeIO :: (MonadIO m, Show a, Stream T.Text m Char) => ParsecT T.Text () m a -> T.Text -> m (Maybe [a])
scrapeIO = runScraperOnHtmlIO

{- | Run a scraper only within the @\<body\>@ element, skipping the @\<head\>@ via 'skipToInBody'.

@since 0.1.0.0
-}
runScraperInBody :: ParsecT T.Text () Identity a -> T.Text -> Maybe [a]
runScraperInBody prsr html = fromRight Nothing $ parse (skipToInBody >> findNaive prsr) "" html

{- | Skip parser input past the @\<html\>@ opening tag, the @\<head\>@ element, and the
@\<body\>@ opening tag.

@since 0.1.0.0
-}
skipToInBody :: (Stream s m Char) => ParsecT s u m ()
skipToInBody =
    manyTill anyChar (parseOpeningTag (Just ["html"]) [] >> char '>')
        </>> el "head" []
        </>> parseOpeningTag (Just ["body"]) []
        >> char '>'
        >> return ()

{- | Run a scraper after skipping to the body section via 'skipToBody'.

@since 0.1.0.0
-}
runScraperOnBody :: ParsecT T.Text () Identity a -> T.Text -> Maybe [a]
runScraperOnBody prsr html = fromRight Nothing $ parse (skipToBody >> findNaive prsr) "" html

{- | Skip parser input past the @\<html\>@ opening tag and the @\<head\>@ element.

@since 0.1.0.0
-}
skipToBody :: (Stream s m Char) => ParsecT s u m ()
skipToBody = manyTill anyChar (parseOpeningTag (Just ["html"]) [] >> char '>') </>> el "head" [] >> return ()

{- | Run a scraper and return only the first match.

@since 0.1.0.0
-}
runScraperOnHtml1 :: ParsecT T.Text () Identity a -> T.Text -> Maybe a
runScraperOnHtml1 p = (>>= listToMaybe) . runScraperOnHtml p

{- | Run a 'findNaive' search and return the first match, if any.

@since 0.1.0.0
-}
scrapeFirst :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m (Maybe a)
scrapeFirst p = do
    result <- findNaive p
    case result of
        Just (y : _) -> return $ Just y
        Just [] -> return Nothing
        Nothing -> return Nothing

{- | Count the number of matches for a pattern in the input stream.

@since 0.1.0.0
-}
findCount :: (Stream s m Char) => ParsecT s u m a -> ParsecT s u m Int
findCount p = do
    x <- findNaive p
    return $ length (fromMaybe [] x)

{- | A prefix string for 'scrapeBracketed'.

@since 0.1.0.0
-}
type Prefix' = T.Text

{- | Scrape content between two occurrences of a prefix delimiter.

@since 0.1.0.0
-}
{-# DEPRECATED scrapeBracketed "experimental, first attempt" #-}
scrapeBracketed :: Prefix' -> ScraperT a -> Html -> Maybe [a]
scrapeBracketed pre scraper html = mconcat <$> scrape (string (T.unpack pre) >> manyTill scraper (string (T.unpack pre))) html

{- | A prefix string for 'scrapePrefixed'.

@since 0.1.0.0
-}
type Prefix = T.Text

{- | Scrape a pattern that is expected to appear after a given prefix string.

@since 0.1.0.0
-}
scrapePrefixed :: Prefix -> ScraperT a -> Html -> Maybe [a]
scrapePrefixed pre scraper html = scrape (string (T.unpack pre) >> scraper) html
