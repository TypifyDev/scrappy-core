{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Scrappy.Elem.ChainHTMLSpec (spec) where

import Data.Functor.Identity (Identity)
import Test.Hspec
import Text.Parsec (ParsecT, char, parse, string)

import Scrappy.Elem.ChainHTML (
    Shell,
    (</>>=),
    (</>>),
    clean,
    contains,
    contains',
    contains'',
    containsFirst,
    containsMany,
    htmlTag,
    manyHtml,
    manyTillHtml_,
    manyTill_,
    mustContain,
    nl,
    parseInShell,
    sequenceHtml,
    sequenceHtml_,
    someHtml,
 )
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Elem.Types (Elem' (..), elTag, innerText')
import TestUtils (shouldBeUndefined)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Type alias for a simple element parser producing Elem' String
type ElemParser = ParsecT String () Identity (Elem' String)

-- | Create an elemParser restricted to specific tags with no inner parser
elemParserS :: Maybe [String] -> [(String, Maybe String)] -> ElemParser
elemParserS tags attrss = elemParser tags (Nothing :: Maybe (ParsecT String () Identity String)) attrss

-- | Assert a parser succeeds and return the result
shouldParse :: (Show a) => ParsecT String () Identity a -> String -> IO a
shouldParse p input = case parse p "" input of
    Right a -> pure a
    Left err -> do
        expectationFailure ("Parse failed: " ++ show err)
        error "unreachable"

-- | Assert a parser fails on the given input
shouldFailOn :: (Show a) => ParsecT String () Identity a -> String -> Expectation
shouldFailOn p input = case parse p "" input of
    Right v -> expectationFailure ("Expected parse failure but got: " ++ show v)
    Left _ -> pure ()

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    -- -------------------------------------------------------------------
    -- nl
    -- -------------------------------------------------------------------
    describe "nl" $ do
        it "succeeds on empty input (optional whitespace)" $ do
            _ <- shouldParse nl ""
            pure () :: Expectation

        it "consumes newlines" $ do
            _ <- shouldParse nl "\n\n"
            pure () :: Expectation

        it "consumes spaces" $ do
            _ <- shouldParse nl "   "
            pure () :: Expectation

        it "consumes mixed newlines and spaces" $ do
            _ <- shouldParse nl "\n \n  \n"
            pure () :: Expectation

        it "succeeds without consuming when no whitespace" $ do
            -- nl is optional, so it succeeds even on non-whitespace input
            -- but does not consume the non-whitespace character
            let p = nl >> string "abc"
            result <- shouldParse p "abc"
            result `shouldBe` "abc"

    -- -------------------------------------------------------------------
    -- manyHtml
    -- -------------------------------------------------------------------
    describe "manyHtml" $ do
        it "returns empty list on empty input" $ do
            result <- shouldParse (manyHtml (string "abc")) ""
            result `shouldBe` ([] :: [String])

        it "returns empty list when no matches" $ do
            result <- shouldParse (manyHtml (string "abc")) ""
            result `shouldBe` ([] :: [String])

        it "parses a single occurrence" $ do
            result <- shouldParse (manyHtml (string "abc")) "abc"
            result `shouldBe` ["abc"]

        it "parses multiple occurrences separated by whitespace" $ do
            result <- shouldParse (manyHtml (string "abc")) "abc\nabc abc"
            result `shouldBe` ["abc", "abc", "abc"]

        it "parses multiple HTML elements with whitespace" $ do
            let liParser = elemParserS (Just ["li"]) []
            result <- shouldParse (manyHtml liParser) "<li>A</li>\n<li>B</li>"
            length result `shouldBe` (2 :: Int)
            map elTag result `shouldBe` ["li", "li"]
            map innerText' result `shouldBe` ["A", "B"]

    -- -------------------------------------------------------------------
    -- someHtml
    -- -------------------------------------------------------------------
    describe "someHtml" $ do
        it "fails on empty input (requires at least one)" $ do
            shouldFailOn (someHtml (string "abc")) ""

        it "parses a single occurrence" $ do
            result <- shouldParse (someHtml (string "abc")) "abc"
            result `shouldBe` ["abc"]

        it "parses multiple occurrences separated by whitespace" $ do
            result <- shouldParse (someHtml (string "abc")) "abc\nabc abc"
            result `shouldBe` ["abc", "abc", "abc"]

        it "parses multiple li elements with whitespace" $ do
            let liParser = elemParserS (Just ["li"]) []
            let html = "<li>First</li>\n  <li>Second</li>\n  <li>Third</li>"
            result <- shouldParse (someHtml liParser) html
            length result `shouldBe` (3 :: Int)
            map elTag result `shouldBe` ["li", "li", "li"]
            map innerText' result `shouldBe` ["First", "Second", "Third"]

        it "fails when no elements match" $ do
            let divParser = elemParserS (Just ["div"]) []
            shouldFailOn (someHtml divParser) "<span>nope</span>"

    -- -------------------------------------------------------------------
    -- manyTill_
    -- -------------------------------------------------------------------
    describe "manyTill_" $ do
        it "returns empty list when end matches immediately" $ do
            let p = manyTill_ (char 'a') (string "end")
            (items, endVal) <- shouldParse p "end"
            items `shouldBe` ([] :: [Char])
            endVal `shouldBe` "end"

        it "collects items until end parser matches" $ do
            let p = manyTill_ (char 'a') (string "bb")
            (items, endVal) <- shouldParse p "aaabb"
            items `shouldBe` "aaa"
            endVal `shouldBe` "bb"

        it "returns single item before end" $ do
            let p = manyTill_ (char 'x') (string "!")
            (items, endVal) <- shouldParse p "x!"
            items `shouldBe` "x"
            endVal `shouldBe` "!"

        it "fails when neither item nor end matches" $ do
            let p = manyTill_ (char 'a') (string "end") :: ParsecT String () Identity ([Char], String)
            shouldFailOn p "xyz"

    -- -------------------------------------------------------------------
    -- manyTillHtml_
    -- -------------------------------------------------------------------
    describe "manyTillHtml_" $ do
        it "returns empty list when end matches immediately" $ do
            let p = manyTillHtml_ (string "item") (string "STOP")
            (items, endVal) <- shouldParse p "STOP"
            items `shouldBe` ([] :: [String])
            endVal `shouldBe` "STOP"

        it "collects items with whitespace until end" $ do
            let p = manyTillHtml_ (string "item") (string "STOP")
            (items, endVal) <- shouldParse p "item\nitem STOP"
            length items `shouldBe` (2 :: Int)
            endVal `shouldBe` "STOP"

        it "handles single item before end" $ do
            let p = manyTillHtml_ (string "A") (string "Z")
            (items, endVal) <- shouldParse p "A Z"
            items `shouldBe` ["A"]
            endVal `shouldBe` "Z"

    -- -------------------------------------------------------------------
    -- htmlTag
    -- -------------------------------------------------------------------
    describe "htmlTag" $ do
        it "parses an html opening tag" $ do
            case parse htmlTag "" "<html>" of
                Right (tg, _ats) -> tg `shouldBe` "html"
                Left err -> expectationFailure $ show err

        it "parses an html tag with attributes" $ do
            case parse htmlTag "" "<html lang=\"en\">" of
                Right (tg, _ats) -> tg `shouldBe` "html"
                Left err -> expectationFailure $ show err

        it "fails on non-html tags" $ do
            shouldFailOn htmlTag "<div>"

        it "fails on empty input" $ do
            shouldFailOn htmlTag ""

    -- -------------------------------------------------------------------
    -- clean (undefined)
    -- -------------------------------------------------------------------
    describe "clean" $ do
        it "is undefined" $ do
            shouldBeUndefined (clean "test")

    -- -------------------------------------------------------------------
    -- mustContain
    -- -------------------------------------------------------------------
    describe "mustContain" $ do
        -- mustContain always searches for the literal "Search" pattern internally
        -- (hardcoded in the function body), so we test based on that behavior.
        it "succeeds when inner text contains enough Search matches" $ do
            -- Build an elem whose innerText contains "Search"
            let elemP = elemParserS (Just ["div"]) []
            let p = mustContain elemP (1 :: Int) (string "Search") :: ParsecT String () Identity (Elem' String)
            case parse p "" "<div>Search</div>" of
                Right e -> elTag e `shouldBe` "div"
                Left err -> expectationFailure $ show err

        it "succeeds with multiple Search occurrences when count is 1" $ do
            let elemP = elemParserS (Just ["div"]) []
            let p = mustContain elemP (1 :: Int) (string "Search") :: ParsecT String () Identity (Elem' String)
            case parse p "" "<div>Search and Search again</div>" of
                Right e -> elTag e `shouldBe` "div"
                Left err -> expectationFailure $ show err

        it "fails when inner text does not contain Search" $ do
            let elemP = elemParserS (Just ["div"]) []
            let p = mustContain elemP (1 :: Int) (string "anything") :: ParsecT String () Identity (Elem' String)
            shouldFailOn p "<div>nothing here</div>"

        it "fails when count exceeds number of Search matches" $ do
            let elemP = elemParserS (Just ["div"]) []
            let p = mustContain elemP (5 :: Int) (string "Search") :: ParsecT String () Identity (Elem' String)
            shouldFailOn p "<div>Search</div>"

    -- -------------------------------------------------------------------
    -- Shell type alias
    -- -------------------------------------------------------------------
    describe "Shell" $ do
        it "is a type alias for (Elem, [(String, Maybe String)])" $ do
            -- Construct a Shell value to verify the type compiles
            let _sh = ("div", [("class", Just "test")]) :: Shell
            pure () :: Expectation

        it "allows empty attribute list" $ do
            let _sh = ("span", []) :: Shell
            pure () :: Expectation

        it "allows Nothing attribute values" $ do
            let _sh = ("a", [("href", Nothing)]) :: Shell
            pure () :: Expectation

    -- -------------------------------------------------------------------
    -- sequenceHtml
    -- -------------------------------------------------------------------
    describe "sequenceHtml" $ do
        it "sequences two parsers keeping both results" $ do
            let p = sequenceHtml (string "hello") (string "world")
            (a, b) <- shouldParse p "hello world"
            a `shouldBe` "hello"
            b `shouldBe` "world"

        it "handles newlines between parsers" $ do
            let p = sequenceHtml (string "A") (string "B")
            (a, b) <- shouldParse p "A\nB"
            a `shouldBe` "A"
            b `shouldBe` "B"

        it "handles tabs between parsers" $ do
            let p = sequenceHtml (string "A") (string "B")
            (a, b) <- shouldParse p "A\tB"
            a `shouldBe` "A"
            b `shouldBe` "B"

        it "works with no whitespace between parsers" $ do
            let p = sequenceHtml (string "AB") (string "CD")
            (a, b) <- shouldParse p "ABCD"
            a `shouldBe` "AB"
            b `shouldBe` "CD"

        it "handles multiple whitespace characters" $ do
            let p = sequenceHtml (string "X") (string "Y")
            (a, b) <- shouldParse p "X  \n\t  Y"
            a `shouldBe` "X"
            b `shouldBe` "Y"

        it "sequences two HTML element parsers" $ do
            let divP = elemParserS (Just ["div"]) []
            let spanP = elemParserS (Just ["span"]) []
            let p = sequenceHtml divP spanP
            (d, s) <- shouldParse p "<div>hello</div>\n<span>world</span>"
            elTag d `shouldBe` "div"
            elTag s `shouldBe` "span"
            innerText' d `shouldBe` "hello"
            innerText' s `shouldBe` "world"

        it "fails when first parser fails" $ do
            let p = sequenceHtml (string "hello") (string "world") :: ParsecT String () Identity (String, String)
            shouldFailOn p "nope world"

        it "fails when second parser fails" $ do
            let p = sequenceHtml (string "hello") (string "world") :: ParsecT String () Identity (String, String)
            shouldFailOn p "hello nope"

    -- -------------------------------------------------------------------
    -- sequenceHtml_
    -- -------------------------------------------------------------------
    describe "sequenceHtml_" $ do
        it "sequences two parsers discarding the first result" $ do
            let p = sequenceHtml_ (string "prefix") (string "value")
            result <- shouldParse p "prefix value"
            result `shouldBe` "value"

        it "handles newlines between parsers" $ do
            let p = sequenceHtml_ (string "A") (string "B")
            result <- shouldParse p "A\nB"
            result `shouldBe` "B"

        it "handles tabs between parsers" $ do
            let p = sequenceHtml_ (string "A") (string "B")
            result <- shouldParse p "A\tB"
            result `shouldBe` "B"

        it "works with no whitespace" $ do
            let p = sequenceHtml_ (string "AB") (string "CD")
            result <- shouldParse p "ABCD"
            result `shouldBe` "CD"

        it "fails when first parser fails" $ do
            let p = sequenceHtml_ (string "hello") (string "world") :: ParsecT String () Identity String
            shouldFailOn p "nope world"

        it "fails when second parser fails" $ do
            let p = sequenceHtml_ (string "hello") (string "world") :: ParsecT String () Identity String
            shouldFailOn p "hello nope"

    -- -------------------------------------------------------------------
    -- (</>>)  operator (alias for sequenceHtml_)
    -- -------------------------------------------------------------------
    describe "(</>>) operator" $ do
        it "discards first result like sequenceHtml_" $ do
            let p = string "skip" </>> string "keep"
            result <- shouldParse p "skip keep"
            result `shouldBe` "keep"

        it "handles whitespace between operands" $ do
            let p = string "A" </>> string "B"
            result <- shouldParse p "A\n\tB"
            result `shouldBe` "B"

        it "fails when first parser fails" $ do
            let p = string "hello" </>> string "world" :: ParsecT String () Identity String
            shouldFailOn p "nope world"

        it "chains with HTML element parsers" $ do
            let divP = elemParserS (Just ["div"]) []
            let spanP = elemParserS (Just ["span"]) []
            let p = divP </>> spanP
            result <- shouldParse p "<div>skip</div>\n<span>keep</span>"
            elTag result `shouldBe` "span"
            innerText' result `shouldBe` "keep"

    -- -------------------------------------------------------------------
    -- (</>>= ) operator (alias for sequenceHtml)
    -- -------------------------------------------------------------------
    describe "(</>>=) operator" $ do
        it "keeps both results like sequenceHtml" $ do
            let p = string "left" </>>=  string "right"
            (a, b) <- shouldParse p "left right"
            a `shouldBe` "left"
            b `shouldBe` "right"

        it "handles whitespace between operands" $ do
            let p = string "X" </>>= string "Y"
            (a, b) <- shouldParse p "X\n  Y"
            a `shouldBe` "X"
            b `shouldBe` "Y"

        it "fails when either parser fails" $ do
            let p = string "A" </>>= string "B" :: ParsecT String () Identity (String, String)
            shouldFailOn p "A C"

    -- -------------------------------------------------------------------
    -- contains (deprecated) and parseInShell (alias)
    -- -------------------------------------------------------------------
    describe "contains" $ do
        it "parses content inside a shell element" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "hello"
            case parse (contains shellP innerP) "" "<div>hello</div>" of
                Right result -> result `shouldBe` "hello"
                Left err -> expectationFailure $ show err

        it "finds content after leading whitespace inside element" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "content"
            case parse (contains shellP innerP) "" "<div>  \ncontent</div>" of
                Right result -> result `shouldBe` "content"
                Left err -> expectationFailure $ show err

        it "fails when inner parser does not match" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "missing"
            shouldFailOn (contains shellP innerP) "<div>other stuff</div>"

        it "fails when shell element does not match" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "hello"
            shouldFailOn (contains shellP innerP) "<span>hello</span>"

    describe "parseInShell" $ do
        it "is an alias for contains" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "hello"
            case parse (parseInShell shellP innerP) "" "<div>hello</div>" of
                Right result -> result `shouldBe` "hello"
                Left err -> expectationFailure $ show err

    -- -------------------------------------------------------------------
    -- contains' / containsMany
    -- -------------------------------------------------------------------
    describe "contains'" $ do
        it "finds multiple matches inside an element" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "match"
            case parse (contains' shellP innerP) "" "<div>match and match</div>" of
                Right results -> length results `shouldBe` (2 :: Int)
                Left err -> expectationFailure $ show err

        it "finds a single match inside an element" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "found"
            case parse (contains' shellP innerP) "" "<div>found here</div>" of
                Right results -> length results `shouldBe` (1 :: Int)
                Left err -> expectationFailure $ show err

        it "fails when no matches exist inside the element" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "nope"
            shouldFailOn (contains' shellP innerP) "<div>nothing here</div>"

        it "fails when shell element does not match" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "hello"
            shouldFailOn (contains' shellP innerP) "<span>hello</span>"

    describe "containsMany" $ do
        it "is the same as contains'" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "x"
            case parse (containsMany shellP innerP) "" "<div>xax</div>" of
                Right results -> length results `shouldBe` (2 :: Int)
                Left err -> expectationFailure $ show err

    -- -------------------------------------------------------------------
    -- containsFirst
    -- -------------------------------------------------------------------
    describe "containsFirst" $ do
        it "returns the first match inside an element" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "target"
            case parse (containsFirst shellP innerP) "" "<div>target and target</div>" of
                Right result -> result `shouldBe` "target"
                Left err -> expectationFailure $ show err

        it "returns the single match when only one exists" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "only"
            case parse (containsFirst shellP innerP) "" "<div>only one</div>" of
                Right result -> result `shouldBe` "only"
                Left err -> expectationFailure $ show err

        it "fails when no matches exist" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "missing"
            shouldFailOn (containsFirst shellP innerP) "<div>nothing here</div>"

        it "fails when shell element does not match" $ do
            let shellP = elemParserS (Just ["div"]) []
            let innerP = string "hello"
            shouldFailOn (containsFirst shellP innerP) "<span>hello</span>"

    -- -------------------------------------------------------------------
    -- contains'' (shell-based parser with Elem/attrs pair)
    -- -------------------------------------------------------------------
    describe "contains''" $ do
        -- contains'' uses elemParser internally with a Shell = (Elem, [(String, Maybe String)])
        it "parses matches inside a shell element" $ do
            let sh = ("div", []) :: Shell
            let innerP = string "found" :: ParsecT String () Identity String
            -- contains'' wraps elemParser and extracts matches'
            case parse (contains'' sh innerP) "" "<div>found</div>" of
                Right results -> length results `shouldSatisfy` (>= (1 :: Int))
                Left err -> expectationFailure $ show err

        it "returns empty list when no inner matches" $ do
            let sh = ("div", []) :: Shell
            let innerP = string "nope" :: ParsecT String () Identity String
            case parse (contains'' sh innerP) "" "<div>other text</div>" of
                Right results -> results `shouldBe` ([] :: [String])
                Left err -> expectationFailure $ show err

        it "fails when shell tag does not match" $ do
            let sh = ("div", []) :: Shell
            let innerP = string "found" :: ParsecT String () Identity String
            shouldFailOn (contains'' sh innerP) "<span>found</span>"

    -- -------------------------------------------------------------------
    -- Integration: combining chainHTML functions
    -- -------------------------------------------------------------------
    describe "integration" $ do
        it "manyHtml with sequenceHtml parses paired elements" $ do
            let pairP = sequenceHtml (elemParserS (Just ["dt"]) []) (elemParserS (Just ["dd"]) [])
            result <- shouldParse (manyHtml pairP) "<dt>Key</dt>\n<dd>Value</dd>\n<dt>K2</dt>\n<dd>V2</dd>"
            length result `shouldBe` (2 :: Int)
            case result of
                ((d1, v1) : (d2, v2) : _) -> do
                    innerText' d1 `shouldBe` "Key"
                    innerText' v1 `shouldBe` "Value"
                    innerText' d2 `shouldBe` "K2"
                    innerText' v2 `shouldBe` "V2"
                _ -> expectationFailure "Expected 2 pairs"

        it "someHtml with </>> chains parsing correctly" $ do
            let skipDiv = elemParserS (Just ["div"]) []
            let keepSpan = elemParserS (Just ["span"]) []
            let p = skipDiv </>> keepSpan
            result <- shouldParse p "<div>skip</div>\n<span>keep</span>"
            elTag result `shouldBe` "span"
            innerText' result `shouldBe` "keep"
