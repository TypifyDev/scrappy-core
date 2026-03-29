{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Links
Description : URL handling, link validation, and site-tree navigation for web scraping
Copyright   : (c) Galen Sprout, 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com

The recursive nature of scraping centers on the URL data structure.
DOM -> Link >>= request --> DOM -> Link ...
-}
module Scrappy.Links (
    -- * Core types

    -- | @since 0.1.0.0
    Link (..),
    -- | @since 0.1.0.0
    PageNumber,
    -- | @since 0.1.0.0
    BaseUrl,
    -- | @since 0.1.0.0
    Url,
    -- | @since 0.1.0.0
    HrefURI,
    -- | @since 0.1.0.0
    CurrentUrl,
    -- | @since 0.1.0.0
    DOI,
    -- | @since 0.1.0.0
    Src,
    -- | @since 0.1.0.0
    RelativeUrl,
    -- | @since 0.1.0.0
    LastUrl,
    -- | @since 0.1.0.0
    Href,
    -- | @since 0.1.0.0
    HostName,
    -- | @since 0.1.0.0
    Namespace,
    -- | @since 0.1.0.0
    Option,
    -- | @since 0.1.0.0
    SiteTree,
    -- | @since 0.1.0.0
    GeneratedLink,

    -- * Type classes

    -- | @since 0.1.0.0
    IsLink (..),

    -- * Data types

    -- | @since 0.1.0.0
    ReferenceSys (..),
    -- | @since 0.1.0.0
    QParams (..),
    -- | @since 0.1.0.0
    DOMLink (..),

    -- * URL operations

    -- | @since 0.1.0.0
    fixRelativeUrl,
    -- | @since 0.1.0.0
    fixURL,
    -- | @since 0.1.0.0
    deriveBaseUrl,
    -- | @since 0.1.0.0
    sameAuthority,
    -- | @since 0.1.0.0
    getHostName,
    -- | @since 0.1.0.0
    getFileName,
    -- | @since 0.1.0.0
    getLastPath,

    -- * Link parsing and validation

    -- | @since 0.1.0.0
    parseLink,
    -- | @since 0.1.0.0
    doiParser,
    -- | @since 0.1.0.0
    fixSameSiteURL,

    -- * URL filtering

    -- | @since 0.1.0.0
    maybeUsefulNewUrl,
    -- | @since 0.1.0.0
    maybeUsefulUrl,
    -- | @since 0.1.0.0
    maybeNewUrl,
    -- | @since 0.1.0.0
    urlIsNew,
    -- | @since 0.1.0.0
    usefulNewUrls,
    -- | @since 0.1.0.0
    usefulUrls,
    -- | @since 0.1.0.0
    numberOfQueryParamsIsZero,

    -- * Stateful operations

    -- | @since 0.1.0.0
    getHtmlStateful,
) where

import qualified Control.Lens as Lens
import Control.Monad (join)
import qualified Data.Aeson.TH as AesonTH
import Data.Char (toLower)
import Data.Functor.Classes (eq1)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.List.NonEmpty as NE (last)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Network.URI as NURI
import Text.Parsec (ParsecT)
import qualified Text.URI as URI
import qualified Text.URI.Lens as UL
import Prelude (Bool (..), Either (..), Eq, Int, Maybe (..), Ord, Read, Show, String, drop, elem, flip, fmap, fst, last, length, not, otherwise, return, snd, undefined, ($), (&&), (.), (/=), (<$>), (<*>), (<>), (=<<), (==), (>>=))

{- | Page number for pagination.

@since 0.1.0.0
-}
type PageNumber = Int

{- | Base URL for relative URL resolution.

@since 0.1.0.0
-}
type BaseUrl = Link

{- | A raw URL string.

@since 0.1.0.0
-}
type Url = String

{- | A raw href URI string.

@since 0.1.0.0
-}
type HrefURI = String

{- | The current page URL during scraping.

@since 0.1.0.0
-}
type CurrentUrl = Url

{- | A Digital Object Identifier string.

@since 0.1.0.0
-}
type DOI = String

{- | A source URL.

@since 0.1.0.0
-}
type Src = Url

{- | A relative URL string.

@since 0.1.0.0
-}
type RelativeUrl = Url

{- | Fix a relative URL against a base URL to produce an absolute URL.

@since 0.1.0.0
-}
fixRelativeUrl :: BaseUrl -> Url -> Url
fixRelativeUrl (Link bUrl) url
    | url == "" = bUrl
    | url == "/" = bUrl
    | isInfixOf bUrl url = url
    | last bUrl == '/' && (isPrefixOf "/" url) = bUrl <> (drop 1 url)
    | last bUrl == '/' && (not $ isPrefixOf "/" url) = bUrl <> url
    | last bUrl /= '/' && (isPrefixOf "/" url) = bUrl <> url
    | last bUrl /= '/' && (not $ isPrefixOf "/" url) = bUrl <> "/" <> url
    | otherwise = bUrl <> url

{- | Placeholder for stateful HTML retrieval.

@since 0.1.0.0
-}
getHtmlStateful :: Url -> String
getHtmlStateful = undefined

{- | The most recently visited URL.

@since 0.1.0.0
-}
type LastUrl = Link

{- | A raw href attribute value.

@since 0.1.0.0
-}
type Href = String

{- | Fix a same-site URL relative to the last visited URL.

@since 0.1.0.0
-}
fixSameSiteURL :: LastUrl -> Href -> Maybe Url
fixSameSiteURL _lastUrl _href = undefined

{- | Resolve an href to a full URL using the previous page URL.

If the href has a scheme, return it as-is. Otherwise resolve relative to the base.

@since 0.1.0.0
-}
fixURL :: LastUrl -> Href -> Url
fixURL previous href =
    let
        base = if isPrefixOf "/" href then fromJust $ deriveBaseUrl previous else previous
        hrefURI = URI.mkURI . T.pack $ href
     in
        case join $ URI.uriScheme <$> hrefURI of
            Just _ -> href
            Nothing -> fixRelativeUrl base href

{- | Derive the base URL (scheme + host) from a 'Link'.

@since 0.1.0.0
-}
deriveBaseUrl :: Link -> Maybe BaseUrl
deriveBaseUrl (Link url) = mkBaseUrl =<< (URI.mkURI . T.pack $ url)

{- | Extract scheme and host from a URI to form a base URL.

@since 0.1.0.0
-}
mkBaseUrl :: URI.URI -> Maybe Link
mkBaseUrl uri = do
    scheme <- fmap URI.unRText $ uri Lens.^. UL.uriScheme
    host <- case uri Lens.^. UL.uriAuthority of
        Right author -> Just $ URI.unRText $ author Lens.^. UL.authHost
        Left _ -> Nothing
    Just . Link $ (T.unpack scheme) <> ("://") <> (T.unpack host)

{- | Type class for types that can be rendered as a URL string.

@since 0.1.0.0
-}
class IsLink a where
    {- | Render the link as a raw URL string.

    @since 0.1.0.0
    -}
    renderLink :: a -> Url

{- | Extract the filename from a link's URL path.

@since 0.1.0.0
-}
getFileName :: Link -> Maybe String
getFileName = getLastPath

{- | Parser for Digital Object Identifiers.

@since 0.1.0.0
-}
doiParser :: ParsecT s u m DOI
doiParser = undefined

{- | A reference system containing lists of references and sources.

@since 0.1.0.0
-}
data ReferenceSys = RefSys [String] [String]

{- | A generated link string.

@since 0.1.0.0
-}
type GeneratedLink = String

{- | A namespace identifier (e.g. form field name).

@since 0.1.0.0
-}
type Namespace = T.Text

{- | An option value within a namespace.

@since 0.1.0.0
-}
type Option = T.Text

{- | Query parameters, either as namespace-options or simple key-value pairs.

@since 0.1.0.0
-}
data QParams = Opt (Map.Map Namespace [Option]) | SimpleKV (T.Text, T.Text)

{- | A site tree mapping URLs to their visited status.

@since 0.1.0.0
-}
type SiteTree = [(Bool, T.Text)]

{- | A DOM-level link representation before resolution.

@since 0.1.0.0
-}
data DOMLink
    = Href' Href
    | Src Url
    | PlainLink Url

{- | A validated, resolved URL.

@since 0.1.0.0
-}
newtype Link = Link Url deriving (Eq, Show, Read, Ord)

{- | Parse and validate a link from a raw URL string.

Handles relative URLs, scheme detection, and same-site filtering.

@since 0.1.0.0
-}
parseLink :: Bool -> Link -> Url -> Maybe Link
parseLink onlySameSite lastLink newLink =
    case hasNoURIScheme newLink of
        True -> Just . Link $ fixRelativeUrl (fromJust $ deriveBaseUrl lastLink) newLink
        False -> case isHTTP newLink of
            False -> Nothing
            True -> case onlySameSite of
                False -> Just . Link $ newLink
                True -> case sameAuthority newLink lastLink of
                    False -> Nothing
                    True -> Just . Link $ newLink
  where
    hasNoURIScheme url = (join $ fmap URI.uriScheme $ URI.mkURI . T.pack $ url) == Nothing
    isHTTP url = elem (fromMaybe "" (fmap NURI.uriScheme $ NURI.parseURI url)) ["https:", "http:"]

{- | Check whether two URLs share the same authority (domain).

@since 0.1.0.0
-}
sameAuthority :: Url -> Link -> Bool
sameAuthority href (Link linky) =
    let
        getMainAuthority = last . T.splitOn "." . T.pack
        getRegName l = fmap (getMainAuthority . NURI.uriRegName) $ NURI.uriAuthority =<< NURI.parseURI l
     in
        case (==) <$> (getRegName href) <*> (getRegName linky) of
            Nothing -> False
            Just b -> b

{- | A hostname string.

@since 0.1.0.0
-}
type HostName = String

{- | Extract the hostname from a 'Link'.

@since 0.1.0.0
-}
getHostName :: Link -> Maybe HostName
getHostName (Link url) = do
    uri <- URI.mkURI $ T.pack url
    case fmap (T.unpack . URI.unRText . (Lens.^. UL.authHost)) (uri Lens.^. UL.uriAuthority) of
        Right hn -> Just hn
        _ -> Nothing

-- | @since 0.1.0.0
instance IsLink Link where
    renderLink (Link url) = url

{- | Filter for URLs that are new (not yet visited) and potentially useful.

@since 0.1.0.0
-}
maybeUsefulNewUrl :: Link -> [(Link, a)] -> Link -> Maybe Link
maybeUsefulNewUrl baseUrl tree url = maybeUsefulUrl baseUrl url >>= maybeNewUrl tree

{- | Check whether a URL has not been visited in the site tree.

@since 0.1.0.0
-}
urlIsNew :: [(a, Url)] -> HrefURI -> Bool
urlIsNew [] _uri = True
urlIsNew (branch : tree) uri
    | eq1 (fmap URI.uriPath (mkURI' (uri))) (fmap URI.uriPath (mkURI' (snd branch))) = False
    | otherwise = urlIsNew tree uri
  where
    mkURI' :: String -> Maybe URI.URI
    mkURI' url = URI.mkURI (T.pack url)

{- | Check if a link has not been visited, returning 'Nothing' if already seen.

@since 0.1.0.0
-}
maybeNewUrl :: [(Link, a)] -> Link -> Maybe Link
maybeNewUrl [] uri = Just uri
maybeNewUrl (branch : tree) uri =
    if eq1 (fmap URI.uriPath (mkURI' (renderLink uri))) (fmap URI.uriPath (mkURI' . renderLink . fst $ branch))
        then Nothing
        else maybeNewUrl tree uri
  where
    mkURI' :: String -> Maybe URI.URI
    mkURI' url = URI.mkURI (T.pack url)

{- | Filter for same-site URLs without JavaScript refs, query strings, or disallowed file types.

@since 0.1.0.0
-}
maybeUsefulUrl :: Link -> Link -> Maybe Link
maybeUsefulUrl (Link baseUrl) url = do
    _ <- noJSorShit url
    _ <- numberOfQueryParamsIsZero url
    _ <- if isInfixOf baseUrl (renderLink url) then return url else Nothing
    allowableEndings url
  where
    noJSorShit :: Link -> Maybe Link
    noJSorShit link =
        if (not $ elem True (urlContains link ["javascript", "about", "help", "#"]))
            then Just url
            else Nothing

    urlContains :: Link -> [String] -> [Bool]
    urlContains (Link lnk) icases = fmap ((flip isInfixOf) (fmap toLower lnk)) icases

    allowableEndings lnk =
        let lastPath = fromMaybe "" $ getLastPath lnk
         in if (elem '.' lastPath)
                then allowableFile lastPath lnk
                else Just lnk

    allowableFile endPath lnk =
        if elem True $ fmap (\x -> isSuffixOf x (fmap toLower endPath)) allowed
            then Just lnk
            else Nothing
      where
        allowed = [".aspx", ".html", ".pdf", ".php"]

{- | Extract the last path segment from a link's URL.

@since 0.1.0.0
-}
getLastPath :: Link -> Maybe String
getLastPath (Link url) = do
    x <- URI.mkURI $ T.pack url
    x' <- URI.uriPath x
    Just . T.unpack . URI.unRText . NE.last . snd $ x'

{- | Filter a list of links for useful new URLs.

@since 0.1.0.0
-}
usefulNewUrls :: Link -> [(Link, a)] -> [Link] -> [Maybe Link]
usefulNewUrls _ _ [] = []
usefulNewUrls baseUrl tree (link : links) = (maybeUsefulNewUrl baseUrl tree link) : usefulNewUrls baseUrl tree links

{- | Filter a list of links for useful URLs.

@since 0.1.0.0
-}
usefulUrls :: Link -> [Link] -> [Maybe Link]
usefulUrls _ [] = []
usefulUrls baseUrl (link : links) = maybeUsefulUrl baseUrl link : usefulUrls baseUrl links

{- | Check that a URL has zero query parameters.

@since 0.1.0.0
-}
numberOfQueryParamsIsZero :: Link -> Maybe String
numberOfQueryParamsIsZero (Link uri) = do
    x <- URI.mkURI (T.pack uri)
    if length (URI.uriQuery x) == 0
        then Just uri
        else Nothing

AesonTH.deriveJSON AesonTH.defaultOptions ''Link
