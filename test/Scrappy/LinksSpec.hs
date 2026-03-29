module Scrappy.LinksSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Scrappy.Links
    ( DOMLink (..)
    , IsLink (..)
    , Link (..)
    , QParams (..)
    , ReferenceSys (..)
    , deriveBaseUrl
    , doiParser
    , fixRelativeUrl
    , fixSameSiteURL
    , fixURL
    , getFileName
    , getHostName
    , getHtmlStateful
    , getLastPath
    , maybeNewUrl
    , maybeUsefulNewUrl
    , maybeUsefulUrl
    , numberOfQueryParamsIsZero
    , parseLink
    , sameAuthority
    , urlIsNew
    , usefulNewUrls
    , usefulUrls
    )
import Test.Hspec
import Test.QuickCheck
import TestUtils (shouldBeUndefined)

spec :: Spec
spec = do
    ---------------------------------------------------------------------------
    -- fixRelativeUrl
    ---------------------------------------------------------------------------
    describe "fixRelativeUrl" $ do
        it "returns base URL when URL is empty" $ do
            fixRelativeUrl (Link "https://example.com") ""
                `shouldBe` "https://example.com"

        it "returns base URL when URL is /" $ do
            fixRelativeUrl (Link "https://example.com") "/"
                `shouldBe` "https://example.com"

        it "returns URL unchanged when it already contains the base URL" $ do
            fixRelativeUrl (Link "https://example.com") "https://example.com/page"
                `shouldBe` "https://example.com/page"

        it "handles trailing slash on base + leading slash on relative" $ do
            fixRelativeUrl (Link "https://example.com/") "/page"
                `shouldBe` "https://example.com/page"

        it "handles trailing slash on base + no leading slash on relative" $ do
            fixRelativeUrl (Link "https://example.com/") "page"
                `shouldBe` "https://example.com/page"

        it "handles no trailing slash on base + leading slash on relative" $ do
            fixRelativeUrl (Link "https://example.com") "/page"
                `shouldBe` "https://example.com/page"

        it "handles no trailing slash on base + no leading slash on relative" $ do
            fixRelativeUrl (Link "https://example.com") "page"
                `shouldBe` "https://example.com/page"

        it "preserves deeper relative paths" $ do
            fixRelativeUrl (Link "https://example.com") "/a/b/c"
                `shouldBe` "https://example.com/a/b/c"

        it "is idempotent for already-fixed URLs" $ property $
            \path ->
                let base = Link "https://example.com"
                    sanitized = "/" ++ filter (/= '/') path
                    fixed = fixRelativeUrl base sanitized
                    fixedAgain = fixRelativeUrl base fixed
                 in fixed == fixedAgain

    ---------------------------------------------------------------------------
    -- fixURL
    ---------------------------------------------------------------------------
    describe "fixURL" $ do
        it "returns href as-is when it has a scheme" $ do
            fixURL (Link "https://example.com") "https://other.com/page"
                `shouldBe` "https://other.com/page"

        it "resolves relative href without scheme against base" $ do
            fixURL (Link "https://example.com/dir/") "page"
                `shouldBe` "https://example.com/dir/page"

        it "resolves absolute-path href against derived base" $ do
            fixURL (Link "https://example.com/dir/page") "/other"
                `shouldBe` "https://example.com/other"

        it "returns http href as-is" $ do
            fixURL (Link "https://example.com") "http://insecure.com/path"
                `shouldBe` "http://insecure.com/path"

    ---------------------------------------------------------------------------
    -- deriveBaseUrl
    ---------------------------------------------------------------------------
    describe "deriveBaseUrl" $ do
        it "extracts scheme and host from full URL" $ do
            deriveBaseUrl (Link "https://example.com/path/to/page")
                `shouldBe` Just (Link "https://example.com")

        it "works with http scheme" $ do
            deriveBaseUrl (Link "http://example.com/path")
                `shouldBe` Just (Link "http://example.com")

        it "handles URL with port (drops port, keeps host)" $ do
            -- modern-uri may or may not include port; we just check it parses
            deriveBaseUrl (Link "https://example.com:8080/path")
                `shouldBe` Just (Link "https://example.com")

        it "returns Nothing for invalid URLs" $ do
            deriveBaseUrl (Link "not-a-url") `shouldBe` Nothing

        it "returns Nothing for empty string" $ do
            deriveBaseUrl (Link "") `shouldBe` Nothing

        it "handles URL with subdomain" $ do
            deriveBaseUrl (Link "https://www.example.com/page")
                `shouldBe` Just (Link "https://www.example.com")

    ---------------------------------------------------------------------------
    -- sameAuthority
    ---------------------------------------------------------------------------
    describe "sameAuthority" $ do
        it "returns True for same domain" $ do
            sameAuthority "https://example.com/page" (Link "https://example.com/other")
                `shouldBe` True

        it "returns True for subdomains sharing the same TLD" $ do
            sameAuthority "https://sub.example.com" (Link "https://other.example.com")
                `shouldBe` True

        it "returns False for different TLDs" $ do
            sameAuthority "https://example.com" (Link "https://different.org")
                `shouldBe` False

        it "returns False when one URL is invalid" $ do
            sameAuthority "not-a-url" (Link "https://example.com")
                `shouldBe` False

    ---------------------------------------------------------------------------
    -- getHostName
    ---------------------------------------------------------------------------
    describe "getHostName" $ do
        it "extracts hostname from a valid URL" $ do
            getHostName (Link "https://example.com/path")
                `shouldBe` Just "example.com"

        it "extracts hostname with subdomain" $ do
            getHostName (Link "https://www.example.com/path")
                `shouldBe` Just "www.example.com"

        it "returns Nothing for invalid URL" $ do
            getHostName (Link "not-a-url") `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- getLastPath / getFileName
    ---------------------------------------------------------------------------
    describe "getLastPath" $ do
        it "extracts the last path segment" $ do
            getLastPath (Link "https://example.com/path/file.html")
                `shouldBe` Just "file.html"

        it "extracts single path segment" $ do
            getLastPath (Link "https://example.com/page")
                `shouldBe` Just "page"

        it "returns Nothing for URL without path" $ do
            getLastPath (Link "https://example.com")
                `shouldBe` Nothing

    describe "getFileName" $ do
        it "delegates to getLastPath" $ do
            getFileName (Link "https://example.com/dir/index.html")
                `shouldBe` getLastPath (Link "https://example.com/dir/index.html")

    ---------------------------------------------------------------------------
    -- parseLink
    ---------------------------------------------------------------------------
    describe "parseLink" $ do
        it "returns Just Link for any valid HTTP URL when onlySameSite is False" $ do
            parseLink False (Link "https://example.com") "https://other.com"
                `shouldBe` Just (Link "https://other.com")

        it "returns Nothing for different-TLD URL when onlySameSite is True" $ do
            parseLink True (Link "https://example.com/page") "https://other.org/page"
                `shouldBe` Nothing

        it "returns Just for same-TLD URL when onlySameSite is True" $ do
            parseLink True (Link "https://example.com/page1") "https://example.com/page2"
                `shouldBe` Just (Link "https://example.com/page2")

        it "resolves relative URLs against derived base" $ do
            case parseLink False (Link "https://example.com/dir/") "/other" of
                Just (Link url) -> url `shouldBe` "https://example.com/other"
                Nothing -> expectationFailure "Expected Just Link"

        it "returns Nothing for non-HTTP scheme (ftp)" $ do
            parseLink False (Link "https://example.com") "ftp://files.example.com"
                `shouldBe` Nothing

        it "returns Nothing for javascript: hrefs (recognized as non-HTTP scheme)" $ do
            -- javascript: is parsed as a URI scheme by modern-uri, then rejected by isHTTP
            let result = parseLink False (Link "https://example.com/") "javascript:void(0)"
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- numberOfQueryParamsIsZero
    ---------------------------------------------------------------------------
    describe "numberOfQueryParamsIsZero" $ do
        it "returns Just for URL without query params" $ do
            numberOfQueryParamsIsZero (Link "https://example.com")
                `shouldBe` Just "https://example.com"

        it "returns Nothing for URL with query params" $ do
            numberOfQueryParamsIsZero (Link "https://example.com?q=1")
                `shouldBe` Nothing

        it "returns Just for URL with path but no params" $ do
            numberOfQueryParamsIsZero (Link "https://example.com/page")
                `shouldBe` Just "https://example.com/page"

        it "returns Nothing for URL with multiple query params" $ do
            numberOfQueryParamsIsZero (Link "https://example.com?a=1&b=2")
                `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- urlIsNew
    ---------------------------------------------------------------------------
    describe "urlIsNew" $ do
        it "returns True for empty tree" $ do
            urlIsNew ([] :: [((), String)]) "https://example.com"
                `shouldBe` True

        it "returns False when URL is already in tree" $ do
            urlIsNew [((), "https://example.com")] "https://example.com"
                `shouldBe` False

        it "returns True when URL path differs" $ do
            urlIsNew [((), "https://example.com/page1")] "https://example.com/page2"
                `shouldBe` True

    ---------------------------------------------------------------------------
    -- maybeNewUrl
    ---------------------------------------------------------------------------
    describe "maybeNewUrl" $ do
        it "returns Just for empty tree" $ do
            maybeNewUrl ([] :: [(Link, ())]) (Link "https://example.com")
                `shouldBe` Just (Link "https://example.com")

        it "returns Nothing when URL is already in tree" $ do
            maybeNewUrl [(Link "https://example.com", ())] (Link "https://example.com")
                `shouldBe` Nothing

        it "returns Just when URL path differs from all tree entries" $ do
            maybeNewUrl [(Link "https://example.com/page1", ())] (Link "https://example.com/page2")
                `shouldBe` Just (Link "https://example.com/page2")

    ---------------------------------------------------------------------------
    -- maybeUsefulUrl
    ---------------------------------------------------------------------------
    describe "maybeUsefulUrl" $ do
        it "allows a same-site URL with an allowable extension" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://example.com/doc.html")
                `shouldBe` Just (Link "https://example.com/doc.html")

        it "allows a same-site URL with .pdf extension" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://example.com/doc.pdf")
                `shouldBe` Just (Link "https://example.com/doc.pdf")

        it "filters out URLs containing 'javascript'" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://example.com/javascript:void(0)")
                `shouldBe` Nothing

        it "filters out URLs containing '#'" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://example.com/#section")
                `shouldBe` Nothing

        it "filters out URLs with query parameters" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://example.com/page?query=1")
                `shouldBe` Nothing

        it "filters out URLs not containing base URL" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://other.com/page")
                `shouldBe` Nothing

        it "filters out disallowed file extensions" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://example.com/file.zip")
                `shouldBe` Nothing

        it "allows same-site path with no file extension" $ do
            maybeUsefulUrl (Link "https://example.com") (Link "https://example.com/products")
                `shouldBe` Just (Link "https://example.com/products")

    ---------------------------------------------------------------------------
    -- maybeUsefulNewUrl
    ---------------------------------------------------------------------------
    describe "maybeUsefulNewUrl" $ do
        it "returns Just for useful new URL" $ do
            maybeUsefulNewUrl
                (Link "https://example.com")
                []
                (Link "https://example.com/page")
                `shouldBe` Just (Link "https://example.com/page")

        it "returns Nothing when URL is already visited" $ do
            maybeUsefulNewUrl
                (Link "https://example.com")
                [(Link "https://example.com/page", ())]
                (Link "https://example.com/page")
                `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- usefulUrls
    ---------------------------------------------------------------------------
    describe "usefulUrls" $ do
        it "returns empty list for empty input" $ do
            usefulUrls (Link "https://example.com") [] `shouldBe` []

        it "filters a list of links for useful ones" $ do
            let base = Link "https://example.com"
                links =
                    [ Link "https://example.com/good"
                    , Link "https://other.com/bad"
                    , Link "https://example.com/doc.html"
                    ]
                results = usefulUrls base links
            results
                `shouldBe` [ Just (Link "https://example.com/good")
                           , Nothing
                           , Just (Link "https://example.com/doc.html")
                           ]

    ---------------------------------------------------------------------------
    -- usefulNewUrls
    ---------------------------------------------------------------------------
    describe "usefulNewUrls" $ do
        it "returns empty list for empty input" $ do
            usefulNewUrls (Link "https://example.com") [] [] `shouldBe` []

        it "filters for useful and new URLs" $ do
            let base = Link "https://example.com"
                tree = [(Link "https://example.com/visited", ())]
                links =
                    [ Link "https://example.com/visited"
                    , Link "https://example.com/new"
                    ]
                results = usefulNewUrls base tree links
            results `shouldBe` [Nothing, Just (Link "https://example.com/new")]

    ---------------------------------------------------------------------------
    -- IsLink instance
    ---------------------------------------------------------------------------
    describe "IsLink" $ do
        it "renderLink extracts the URL string from a Link" $ do
            renderLink (Link "https://example.com") `shouldBe` "https://example.com"

        it "renderLink preserves the full URL" $ do
            renderLink (Link "https://example.com/path?q=1#frag")
                `shouldBe` "https://example.com/path?q=1#frag"

    ---------------------------------------------------------------------------
    -- Data type constructors
    ---------------------------------------------------------------------------
    describe "ReferenceSys" $ do
        it "can be constructed with RefSys" $ do
            let (RefSys refs srcs) = RefSys ["ref1", "ref2"] ["src1"]
            refs `shouldBe` ["ref1", "ref2"]
            srcs `shouldBe` ["src1"]

    describe "QParams" $ do
        it "can be constructed as Opt with a map" $ do
            let params = Opt (Map.singleton (T.pack "ns") [T.pack "opt1", T.pack "opt2"])
            case params of
                Opt m -> Map.size m `shouldBe` 1
                SimpleKV _ -> expectationFailure "Expected Opt"

        it "can be constructed as SimpleKV" $ do
            let params = SimpleKV (T.pack "key", T.pack "value")
            case params of
                SimpleKV (k, v) -> do
                    k `shouldBe` T.pack "key"
                    v `shouldBe` T.pack "value"
                Opt _ -> expectationFailure "Expected SimpleKV"

    describe "DOMLink" $ do
        it "can be constructed as Href'" $ do
            let dl = Href' "/page"
            case dl of
                Href' h -> h `shouldBe` "/page"
                _ -> expectationFailure "Expected Href'"

        it "can be constructed as Src" $ do
            let dl = Scrappy.Links.Src "https://img.com/pic.png"
            case dl of
                Scrappy.Links.Src s -> s `shouldBe` "https://img.com/pic.png"
                _ -> expectationFailure "Expected Src"

        it "can be constructed as PlainLink" $ do
            let dl = PlainLink "https://example.com"
            case dl of
                PlainLink u -> u `shouldBe` "https://example.com"
                _ -> expectationFailure "Expected PlainLink"

    ---------------------------------------------------------------------------
    -- Undefined functions
    ---------------------------------------------------------------------------
    describe "getHtmlStateful" $ do
        it "is undefined" $ do
            shouldBeUndefined (getHtmlStateful "https://example.com")

    describe "fixSameSiteURL" $ do
        it "is undefined" $ do
            shouldBeUndefined (fixSameSiteURL (Link "https://example.com") "/page")

    describe "doiParser" $ do
        it "is undefined" $ do
            shouldBeUndefined doiParser
