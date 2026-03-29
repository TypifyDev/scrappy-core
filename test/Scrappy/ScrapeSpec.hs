{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Scrappy.ScrapeSpec (spec) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Text as T
import Test.Hspec
import Text.Parsec (
    ParsecT,
    char,
    parse,
    string,
 )

import Scrappy.Scrape (
    Prefix,
    Prefix',
    ScraperT,
    coerceMaybeParser,
    exists,
    filterFromTextP,
    filterPattern,
    findCount,
    findFit,
    getFirstFitSafe,
    getFirstSafe,
    hoistMaybe,
    runScraperInBody,
    runScraperOnBody,
    runScraperOnHtml,
    runScraperOnHtml1,
    runScraperOnHtmlIO,
    scrape,
    scrapeBracketed,
    scrapeFirst,
    scrapeFirst',
    scrapeIO,
    scrapeLinked,
    scrapePrefixed,
    skipToBody,
    skipToInBody,
 )
import TestUtils (shouldBeUndefined)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run an Identity-based parser on Text input, returning the result or
-- failing the test on parse error.
shouldParseText :: (Show a) => ParsecT T.Text () Identity a -> T.Text -> IO a
shouldParseText p input = case parse p "" input of
    Right a -> pure a
    Left err -> do
        expectationFailure ("Parse failed: " ++ show err)
        error "unreachable"

-- | Assert that an Identity-based Text parser fails on given input.
shouldFailOnText :: (Show a) => ParsecT T.Text () Identity a -> T.Text -> Expectation
shouldFailOnText p input = case parse p "" input of
    Right v -> expectationFailure ("Expected parse failure but got: " ++ show v)
    Left _ -> pure ()

-- | Minimal full HTML document for testing body-related functions.
fullHtmlDoc :: T.Text
fullHtmlDoc =
    "<html><head><title>Test</title></head><body><p>hello</p><p>world</p></body></html>"

-- | Full HTML document with no match content in body.
fullHtmlDocNoMatch :: T.Text
fullHtmlDocNoMatch =
    "<html><head><title>Test</title></head><body><div>nothing here</div></body></html>"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    ---------------------------------------------------------------------------
    -- Type aliases (compilation check)
    ---------------------------------------------------------------------------
    describe "Type aliases" $ do
        it "ScraperT is a type alias for ParsecT Html () Identity" $ do
            -- Compilation check: we can use a ScraperT value as a ParsecT
            let _p :: ScraperT String
                _p = string "test"
            True `shouldBe` True

        it "Prefix is a type alias for Text" $ do
            let _p :: Prefix
                _p = "hello"
            True `shouldBe` True

        it "Prefix' is a type alias for Text" $ do
            let _p :: Prefix'
                _p = "world"
            True `shouldBe` True

    ---------------------------------------------------------------------------
    -- scrapeLinked (undefined)
    ---------------------------------------------------------------------------
    describe "scrapeLinked" $ do
        it "is undefined" $ do
            shouldBeUndefined (scrapeLinked (string "x") :: ParsecT T.Text () Identity [String])

    ---------------------------------------------------------------------------
    -- runScraperOnHtml
    ---------------------------------------------------------------------------
    describe "runScraperOnHtml" $ do
        it "finds multiple matches in HTML text" $ do
            let result = runScraperOnHtml (string "abc") "xxxabcxxxabcxxx"
            case result of
                Just xs -> length xs `shouldBe` (2 :: Int)
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when no matches exist" $ do
            let result = runScraperOnHtml (string "abc") "xxxxxx"
            result `shouldBe` Nothing

        it "returns Nothing on empty input" $ do
            let result = runScraperOnHtml (string "abc") ""
            result `shouldBe` Nothing

        it "finds match at the start" $ do
            let result = runScraperOnHtml (string "abc") "abcdef"
            case result of
                Just xs -> do
                    length xs `shouldBe` (1 :: Int)
                    case xs of
                        (x : _) -> x `shouldBe` "abc"
                        [] -> expectationFailure "Expected non-empty list"
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "finds match at the end" $ do
            let result = runScraperOnHtml (string "abc") "defabc"
            case result of
                Just xs -> length xs `shouldBe` (1 :: Int)
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "finds single-character matches" $ do
            let result = runScraperOnHtml (char 'x') "axbxcx"
            case result of
                Just xs -> length xs `shouldBe` (3 :: Int)
                Nothing -> expectationFailure "Expected Just but got Nothing"

    ---------------------------------------------------------------------------
    -- scrape (alias for runScraperOnHtml)
    ---------------------------------------------------------------------------
    describe "scrape" $ do
        it "is an alias for runScraperOnHtml (finds matches)" $ do
            let result = scrape (string "hello") "say hello and hello again"
            case result of
                Just xs -> length xs `shouldBe` (2 :: Int)
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when no matches" $ do
            let result = scrape (string "missing") "nothing to see here"
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- runScraperOnHtml1
    ---------------------------------------------------------------------------
    describe "runScraperOnHtml1" $ do
        it "returns Just the first match" $ do
            let result = runScraperOnHtml1 (string "abc") "xxxabcxxxabcxxx"
            result `shouldBe` Just "abc"

        it "returns Nothing when no matches" $ do
            let result = runScraperOnHtml1 (string "abc") "xxxxxx"
            result `shouldBe` Nothing

        it "returns Nothing on empty input" $ do
            let result = runScraperOnHtml1 (string "abc") ""
            result `shouldBe` Nothing

        it "returns first match even when multiple exist" $ do
            let result = runScraperOnHtml1 (string "x") "axbxcx"
            result `shouldBe` Just "x"

    ---------------------------------------------------------------------------
    -- runScraperOnHtmlIO
    ---------------------------------------------------------------------------
    describe "runScraperOnHtmlIO" $ do
        it "finds matches in IO context" $ do
            result <- runScraperOnHtmlIO (string "abc") "xxxabcxxx"
            case result of
                Just xs -> length xs `shouldBe` (1 :: Int)
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when no matches in IO" $ do
            result <- runScraperOnHtmlIO (string "abc") "xxxxxx"
            result `shouldBe` Nothing

        it "returns Nothing on empty input in IO" $ do
            result <- runScraperOnHtmlIO (string "abc") ("" :: T.Text)
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- scrapeIO (alias for runScraperOnHtmlIO)
    ---------------------------------------------------------------------------
    describe "scrapeIO" $ do
        it "is an alias for runScraperOnHtmlIO" $ do
            result <- scrapeIO (string "xyz") "abcxyzdef"
            case result of
                Just xs -> do
                    length xs `shouldBe` (1 :: Int)
                    case xs of
                        (x : _) -> x `shouldBe` "xyz"
                        [] -> expectationFailure "Expected non-empty list"
                Nothing -> expectationFailure "Expected Just but got Nothing"

    ---------------------------------------------------------------------------
    -- scrapeFirst'
    ---------------------------------------------------------------------------
    describe "scrapeFirst'" $ do
        it "returns Just the first match" $ do
            let result = scrapeFirst' (string "abc") "xxxabcxxxabcxxx"
            result `shouldBe` Just "abc"

        it "returns Nothing when no matches" $ do
            let result = scrapeFirst' (string "abc") "xxxxxx"
            result `shouldBe` Nothing

        it "returns Nothing on empty input" $ do
            let result = scrapeFirst' (string "abc") ""
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- scrapeFirst (parser combinator)
    ---------------------------------------------------------------------------
    describe "scrapeFirst" $ do
        it "returns Just the first match when matches exist" $ do
            result <- shouldParseText (scrapeFirst (string "abc")) "xxxabcxxx"
            result `shouldBe` Just "abc"

        it "returns Nothing when no matches" $ do
            result <- shouldParseText (scrapeFirst (string "abc")) "xxxxxx"
            result `shouldBe` Nothing

        it "returns Nothing on empty input" $ do
            result <- shouldParseText (scrapeFirst (string "abc")) ""
            result `shouldBe` Nothing

        it "returns first of multiple matches" $ do
            result <- shouldParseText (scrapeFirst (string "x")) "axbxcx"
            result `shouldBe` Just "x"

    ---------------------------------------------------------------------------
    -- exists
    ---------------------------------------------------------------------------
    describe "exists" $ do
        it "returns True when pattern is found" $ do
            exists (string "abc") "xxxabcxxx" `shouldBe` True

        it "returns False when pattern is not found" $ do
            exists (string "abc") "xxxxxx" `shouldBe` False

        it "returns False on empty input" $ do
            exists (string "abc") "" `shouldBe` False

        it "returns True for match at start" $ do
            exists (string "abc") "abcdef" `shouldBe` True

        it "returns True for match at end" $ do
            exists (string "abc") "defabc" `shouldBe` True

    ---------------------------------------------------------------------------
    -- findCount
    ---------------------------------------------------------------------------
    describe "findCount" $ do
        it "counts multiple matches" $ do
            result <- shouldParseText (findCount (string "abc")) "xxxabcxxxabcxxx"
            result `shouldBe` (2 :: Int)

        it "returns 0 when no matches" $ do
            result <- shouldParseText (findCount (string "abc")) "xxxxxx"
            result `shouldBe` (0 :: Int)

        it "returns 0 on empty input" $ do
            result <- shouldParseText (findCount (string "abc")) ""
            result `shouldBe` (0 :: Int)

        it "counts single match" $ do
            result <- shouldParseText (findCount (string "abc")) "abc"
            result `shouldBe` (1 :: Int)

        it "counts single-character matches" $ do
            result <- shouldParseText (findCount (char 'x')) "axbxcx"
            result `shouldBe` (3 :: Int)

    ---------------------------------------------------------------------------
    -- coerceMaybeParser
    ---------------------------------------------------------------------------
    describe "coerceMaybeParser" $ do
        it "succeeds with Just value" $ do
            let p :: ScraperT String
                p = coerceMaybeParser (Just "hello")
            case parse p "" ("" :: T.Text) of
                Right v -> v `shouldBe` "hello"
                Left err -> expectationFailure (show err)

        it "fails with Nothing (parserZero)" $ do
            let p :: ScraperT String
                p = coerceMaybeParser Nothing
            case parse p "" ("anything" :: T.Text) of
                Right v -> expectationFailure ("Expected failure but got: " ++ show v)
                Left _ -> pure ()

    ---------------------------------------------------------------------------
    -- hoistMaybe
    ---------------------------------------------------------------------------
    describe "hoistMaybe" $ do
        it "lifts Just into MaybeT" $ do
            let result :: Maybe Int
                result = runIdentity (runMaybeT (hoistMaybe (Just (42 :: Int))))
            result `shouldBe` Just (42 :: Int)

        it "lifts Nothing into MaybeT" $ do
            let result :: Maybe Int
                result = runIdentity (runMaybeT (hoistMaybe Nothing))
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- getFirstSafe
    ---------------------------------------------------------------------------
    describe "getFirstSafe" $ do
        it "returns Just first element from Just non-empty list" $ do
            getFirstSafe (Just (["a", "b", "c"] :: [String])) `shouldBe` Just ("a" :: String)

        it "returns Nothing from Just empty list" $ do
            getFirstSafe (Just ([] :: [String])) `shouldBe` Nothing

        it "returns Nothing from Nothing" $ do
            getFirstSafe (Nothing :: Maybe [String]) `shouldBe` Nothing

        it "works with single-element list" $ do
            getFirstSafe (Just (["only"] :: [String])) `shouldBe` Just ("only" :: String)

    ---------------------------------------------------------------------------
    -- getFirstFitSafe
    ---------------------------------------------------------------------------
    describe "getFirstFitSafe" $ do
        it "returns Just first element that satisfies predicate" $ do
            getFirstFitSafe (> (3 :: Int)) (Just [1, 2, 3, 4, 5]) `shouldBe` Just (4 :: Int)

        it "returns Nothing when no element satisfies predicate" $ do
            getFirstFitSafe (> (10 :: Int)) (Just [1, 2, 3]) `shouldBe` Nothing

        it "returns Nothing from Just empty list" $ do
            getFirstFitSafe (> (0 :: Int)) (Just []) `shouldBe` Nothing

        it "returns Nothing from Nothing" $ do
            getFirstFitSafe (> (0 :: Int)) (Nothing :: Maybe [Int]) `shouldBe` Nothing

        it "returns the first matching element, not any later one" $ do
            getFirstFitSafe even (Just [1, 2, 4, 6 :: Int]) `shouldBe` Just (2 :: Int)

    ---------------------------------------------------------------------------
    -- findFit
    ---------------------------------------------------------------------------
    describe "findFit" $ do
        it "finds first element satisfying predicate" $ do
            findFit (> (3 :: Int)) [1, 2, 3, 4, 5] `shouldBe` Just (4 :: Int)

        it "returns Nothing when no match" $ do
            findFit (> (10 :: Int)) [1, 2, 3] `shouldBe` Nothing

        it "returns Nothing for empty list" $ do
            findFit (> (0 :: Int)) ([] :: [Int]) `shouldBe` Nothing

        it "returns first element if it matches" $ do
            findFit even [2, 4, 6 :: Int] `shouldBe` Just (2 :: Int)

        it "returns last element if only it matches" $ do
            findFit (== (5 :: Int)) [1, 2, 3, 4, 5] `shouldBe` Just (5 :: Int)

        it "works with single-element list that matches" $ do
            findFit (== (1 :: Int)) [1] `shouldBe` Just (1 :: Int)

        it "works with single-element list that does not match" $ do
            findFit (== (2 :: Int)) [1 :: Int] `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- filterFromTextP
    ---------------------------------------------------------------------------
    describe "filterFromTextP" $ do
        it "filters out all occurrences of a pattern" $ do
            case parse (filterFromTextP (string "del")) "" ("abdelcddelef" :: T.Text) of
                Right result -> T.unpack result `shouldBe` "abcdef"
                Left err -> expectationFailure (show err)

        it "returns original text when pattern is not found" $ do
            case parse (filterFromTextP (string "xyz")) "" ("abcdef" :: T.Text) of
                Right result -> T.unpack result `shouldBe` "abcdef"
                Left err -> expectationFailure (show err)

        it "handles pattern at start" $ do
            case parse (filterFromTextP (string "rm")) "" ("rmhello" :: T.Text) of
                Right result -> T.unpack result `shouldBe` "hello"
                Left err -> expectationFailure (show err)

    ---------------------------------------------------------------------------
    -- filterPattern
    ---------------------------------------------------------------------------
    describe "filterPattern" $ do
        it "removes all occurrences of a pattern from text" $ do
            let result = filterPattern "abXXcdXXef" (string "XX")
            T.unpack result `shouldBe` "abcdef"

        it "returns original text when pattern is absent" $ do
            let result = filterPattern "abcdef" (string "XX")
            T.unpack result `shouldBe` "abcdef"

        it "removes pattern at start of text" $ do
            let result = filterPattern "XXhello" (string "XX")
            T.unpack result `shouldBe` "hello"

        it "removes pattern at end of text" $ do
            let result = filterPattern "helloXX" (string "XX")
            T.unpack result `shouldBe` "hello"

        it "removes single-character patterns" $ do
            let result = filterPattern "axbxcx" (char 'x')
            T.unpack result `shouldBe` "abc"

    ---------------------------------------------------------------------------
    -- scrapePrefixed
    ---------------------------------------------------------------------------
    describe "scrapePrefixed" $ do
        it "scrapes content after a prefix" $ do
            let result = scrapePrefixed "prefix:" (string "value") "noise prefix:value more"
            case result of
                Just xs -> do
                    length xs `shouldBe` (1 :: Int)
                    case xs of
                        (x : _) -> x `shouldBe` "value"
                        [] -> expectationFailure "Expected non-empty list"
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when prefix is absent" $ do
            let result = scrapePrefixed "prefix:" (string "value") "no match here"
            result `shouldBe` Nothing

        it "finds multiple prefixed occurrences" $ do
            let result = scrapePrefixed "key=" (string "val") "key=val and key=val"
            case result of
                Just xs -> length xs `shouldBe` (2 :: Int)
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing on empty input" $ do
            let result = scrapePrefixed "pre" (string "x") ""
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- scrapeBracketed
    ---------------------------------------------------------------------------
    describe "scrapeBracketed" $ do
        it "scrapes content between two occurrences of a delimiter" $ do
            let result = scrapeBracketed "||" (char 'a') "noise ||aaa|| tail"
            case result of
                Just xs -> length xs `shouldBe` (3 :: Int)
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when delimiter is absent" $ do
            let result = scrapeBracketed "||" (char 'a') "no delimiters here"
            result `shouldBe` Nothing

        it "returns Nothing on empty input" $ do
            let result = scrapeBracketed "||" (char 'a') ""
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- skipToInBody
    ---------------------------------------------------------------------------
    describe "skipToInBody" $ do
        it "skips past html open, head element, and body open" $ do
            let htmlDoc = "<html><head><title>T</title></head><body>content" :: T.Text
            case parse (skipToInBody :: ParsecT T.Text () Identity ()) "" htmlDoc of
                Right _ -> pure ()
                Left err -> expectationFailure (show err)

        it "fails when there is no <html> tag" $ do
            shouldFailOnText (skipToInBody :: ParsecT T.Text () Identity ()) "no html here"

        it "fails on empty input" $ do
            shouldFailOnText (skipToInBody :: ParsecT T.Text () Identity ()) ""

    ---------------------------------------------------------------------------
    -- skipToBody
    ---------------------------------------------------------------------------
    describe "skipToBody" $ do
        it "skips past html open and head element" $ do
            let htmlDoc = "<html><head><title>T</title></head><body>rest" :: T.Text
            case parse (skipToBody :: ParsecT T.Text () Identity ()) "" htmlDoc of
                Right _ -> pure ()
                Left err -> expectationFailure (show err)

        it "fails when there is no <html> tag" $ do
            shouldFailOnText (skipToBody :: ParsecT T.Text () Identity ()) "no html tag"

        it "fails on empty input" $ do
            shouldFailOnText (skipToBody :: ParsecT T.Text () Identity ()) ""

    ---------------------------------------------------------------------------
    -- runScraperInBody
    ---------------------------------------------------------------------------
    describe "runScraperInBody" $ do
        it "finds matches within the body element" $ do
            let result = runScraperInBody (string "hello") fullHtmlDoc
            case result of
                Just xs -> do
                    length xs `shouldBe` (1 :: Int)
                    case xs of
                        (x : _) -> x `shouldBe` "hello"
                        [] -> expectationFailure "Expected non-empty list"
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when pattern is not in body" $ do
            let result = runScraperInBody (string "missing") fullHtmlDoc
            result `shouldBe` Nothing

        it "returns Nothing for non-HTML input" $ do
            let result = runScraperInBody (string "abc") "just plain text"
            result `shouldBe` Nothing

        it "does not find content that is only in the head" $ do
            let result = runScraperInBody (string "Test") fullHtmlDocNoMatch
            -- "Test" is in <title> in the head, but runScraperInBody skips head
            -- and only searches the body, which has "nothing here"
            result `shouldBe` Nothing

    ---------------------------------------------------------------------------
    -- runScraperOnBody
    ---------------------------------------------------------------------------
    describe "runScraperOnBody" $ do
        it "finds matches after skipping to body" $ do
            let result = runScraperOnBody (string "world") fullHtmlDoc
            case result of
                Just xs -> do
                    length xs `shouldBe` (1 :: Int)
                    case xs of
                        (x : _) -> x `shouldBe` "world"
                        [] -> expectationFailure "Expected non-empty list"
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when pattern is not found after body" $ do
            let result = runScraperOnBody (string "absent") fullHtmlDoc
            result `shouldBe` Nothing

        it "returns Nothing for non-HTML input" $ do
            let result = runScraperOnBody (string "abc") "not html at all"
            result `shouldBe` Nothing
