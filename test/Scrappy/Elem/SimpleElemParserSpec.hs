{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Scrappy.Elem.SimpleElemParserSpec (spec) where

import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Test.Hspec
import Text.Parsec (ParsecT, anyChar, char, parse, string)

import Scrappy.Elem.ChainHTML (someHtml)
import Scrappy.Elem.SimpleElemParser (
    clickableHref,
    clickableHref',
    eitherP,
    el,
    elSelfC,
    elSelfClosing,
    elemParser,
    elemParserInternal,
    elemParserOld,
    elemParserWhere,
    elemWithBody,
    manyTill_,
    matchesInSameElTag,
    parseInnerHTMLAndEndTag,
    sameElTag,
    selfClosing,
    stylingElem,
    stylingTags,
 )
import Scrappy.Elem.Types (Clickable (..), Elem' (..), InnerTextResult (..), attrs, elTag, innerText', matches')
import Scrappy.Links (Link (..))

import TestUtils (parseSucceeds)

-- Helper type alias for parsers with String inner type
type ElemParser = ParsecT String () Identity (Elem' String)

-- Helper to create elemParser with explicit String type
elemParserS :: Maybe [String] -> [(String, Maybe String)] -> ElemParser
elemParserS tags attrss = elemParser tags (Nothing :: Maybe (ParsecT String () Identity String)) attrss

spec :: Spec
spec = do
    describe "el" $ do
        it "parses a simple div element" $ do
            let result = parse (el "div" []) "" "<div>content</div>"
            case result of
                Right e -> do
                    elTag e `shouldBe` "div"
                    innerText' e `shouldBe` "content"
                Left err -> expectationFailure $ show err

        it "parses an element with text content" $ do
            let result = parse (el "p" []) "" "<p>Hello World</p>"
            case result of
                Right e -> do
                    elTag e `shouldBe` "p"
                    innerText' e `shouldBe` "Hello World"
                Left err -> expectationFailure $ show err

        it "parses an element with specific attribute" $ do
            let result = parse (el "a" [("href", "/page")]) "" "<a href=\"/page\">Link</a>"
            case result of
                Right e -> do
                    elTag e `shouldBe` "a"
                    Map.lookup "href" (attrs e) `shouldBe` Just "/page"
                Left err -> expectationFailure $ show err

        it "fails when tag does not match" $ do
            parseSucceeds (el "div" []) "<span>content</span>" `shouldBe` False

        it "fails when required attribute is missing" $ do
            parseSucceeds (el "a" [("href", "/page")]) "<a>No href</a>" `shouldBe` False

        it "parses empty element" $ do
            let result = parse (el "div" []) "" "<div></div>"
            case result of
                Right e -> do
                    elTag e `shouldBe` "div"
                    innerText' e `shouldBe` ""
                Left err -> expectationFailure $ show err

    describe "elemParser" $ do
        it "matches any element when elemList is Nothing" $ do
            let result = parse (elemParserS Nothing []) "" "<span>text</span>"
            case result of
                Right e -> elTag e `shouldBe` "span"
                Left err -> expectationFailure $ show err

        it "matches only specified elements when elemList is Just [tags]" $ do
            let parser = elemParserS (Just ["a", "div"]) []
            parseSucceeds parser "<a>link</a>" `shouldBe` True
            parseSucceeds parser "<div>div</div>" `shouldBe` True
            parseSucceeds parser "<span>span</span>" `shouldBe` False

        it "captures innerHtmlFull correctly" $ do
            let result = parse (elemParserS (Just ["div"]) []) "" "<div>inner text</div>"
            case result of
                Right e -> innerText' e `shouldBe` "inner text"
                Left err -> expectationFailure $ show err

        it "parses element with multiple attributes" $ do
            let result = parse (elemParserS (Just ["input"]) [("type", Just "text")]) "" "<input type=\"text\" value=\"hello\">"
            case result of
                Right e -> do
                    elTag e `shouldBe` "input"
                    Map.lookup "type" (attrs e) `shouldBe` Just "text"
                Left err -> expectationFailure $ show err

    describe "self-closing elements" $ do
        it "includes common self-closing tags" $ do
            "br" `elem` selfClosing `shouldBe` True
            "img" `elem` selfClosing `shouldBe` True
            "input" `elem` selfClosing `shouldBe` True
            "hr" `elem` selfClosing `shouldBe` True
            "meta" `elem` selfClosing `shouldBe` True

        it "parses br element" $ do
            let result = parse (elemParserS (Just ["br"]) []) "" "<br>"
            case result of
                Right e -> elTag e `shouldBe` "br"
                Left err -> expectationFailure $ show err

        it "parses br element with self-closing slash" $ do
            let result = parse (elemParserS (Just ["br"]) []) "" "<br/>"
            case result of
                Right e -> elTag e `shouldBe` "br"
                Left err -> expectationFailure $ show err

        it "parses img element with attributes" $ do
            let result = parse (elemParserS (Just ["img"]) [("src", Just "test.jpg")]) "" "<img src=\"test.jpg\" alt=\"test\">"
            case result of
                Right e -> do
                    elTag e `shouldBe` "img"
                    Map.lookup "src" (attrs e) `shouldBe` Just "test.jpg"
                Left err -> expectationFailure $ show err

    describe "nested elements" $ do
        it "captures nested element as inner text" $ do
            let result = parse (el "div" []) "" "<div><span>nested</span></div>"
            case result of
                Right e -> do
                    elTag e `shouldBe` "div"
                    -- Inner HTML includes the nested span
                    innerText' e `shouldContain` "nested"
                Left err -> expectationFailure $ show err

    describe "sibling elements with whitespace" $ do
        -- This test ensures that sibling elements separated by whitespace
        -- are parsed correctly and don't get incorrectly nested
        it "parses multiple li siblings with whitespace between them" $ do
            let html = "<ul>\n  <li>A</li>\n  <li>B</li>\n  <li>C</li>\n</ul>"
            let result = parse (elemParserS (Just ["ul"]) []) "" html
            case result of
                Right e -> do
                    elTag e `shouldBe` "ul"
                    -- All three li elements should be captured in the inner text
                    innerText' e `shouldContain` "<li>A</li>"
                    innerText' e `shouldContain` "<li>B</li>"
                    innerText' e `shouldContain` "<li>C</li>"
                Left err -> expectationFailure $ show err

        it "parses sibling elements using someHtml" $ do
            let html = "<li>A</li>\n  <li>B</li>\n  <li>C</li>"
            let liParser = elemParserS (Just ["li"]) []
            let result = parse (someHtml liParser) "" html
            case result of
                Right elems -> do
                    length elems `shouldBe` 3
                    map elTag elems `shouldBe` ["li", "li", "li"]
                    map innerText' elems `shouldBe` ["A", "B", "C"]
                Left err -> expectationFailure $ show err

        it "handles whitespace before closing tags correctly" $ do
            -- This is the exact pattern that caused the bug in hand-rolled parsers:
            -- whitespace before </li> shouldn't cause the next <li> to be nested
            let html = "<li class=\"a\">\n    <a href=\"#\">Home</a>\n  </li>\n  <li class=\"b\">\n    <a href=\"#\">About</a>\n  </li>"
            let liParser = elemParserS (Just ["li"]) []
            let result = parse (someHtml liParser) "" html
            case result of
                Right elems -> do
                    length elems `shouldBe` 2
                    map elTag elems `shouldBe` ["li", "li"]
                Left err -> expectationFailure $ show err

    describe "GHCup page patterns" $ do
        -- Tests based on the actual ghcup.haskell.org navigation HTML
        -- that was causing parsing failures

        it "parses ghcup nav with complex class attributes" $ do
            let html =
                    unlines
                        [ "<ul class=\"nav flex-column\">"
                        , "            <li class=\"box-border flex flex-wrap items-baseline py-2 nav-item\">"
                        , "                <a href=\".\" class=\"block py-2 px-2 text-white/80 hover:text-white no-underline\">GHCup</a>"
                        , "            </li>"
                        , "            <li class=\"box-border flex flex-wrap items-baseline py-2 nav-item\">"
                        , "                <a href=\"install/\" class=\"nav-link\">Installation</a>"
                        , "            </li>"
                        , "            <li class=\"box-border flex flex-wrap items-baseline py-2 nav-item\">"
                        , "                <a href=\"steps/\" class=\"nav-link\">First steps</a>"
                        , "            </li>"
                        , "</ul>"
                        ]
            -- First verify we can parse the ul
            let ulParser = elemParserS (Just ["ul"]) []
            case parse ulParser "" html of
                Left err -> expectationFailure $ "Failed to parse ul: " ++ show err
                Right ul -> do
                    elTag ul `shouldBe` "ul"
                    -- The inner HTML should contain all 3 li elements
                    let inner = innerText' ul
                    inner `shouldContain` "<li class=\"box-border"
                    -- Count li occurrences
                    let liCount =
                            length $
                                filter (== '<') $
                                    concatMap (take 3) $
                                        filter (\s -> take 3 s == "<li") $
                                            tails inner
                    liCount `shouldSatisfy` (>= 3)

        it "parses ghcup nav li elements as siblings using someHtml (no leading whitespace)" $ do
            -- someHtml requires input to start with '<' - no leading whitespace
            -- This is a known limitation: use dropWhile to strip leading whitespace
            let html =
                    unlines
                        [ "<li class=\"box-border flex flex-wrap items-baseline py-2 nav-item\">"
                        , "                <a href=\".\" class=\"block py-2 px-2 text-white/80\">GHCup</a>"
                        , "            </li>"
                        , "            <li class=\"box-border flex flex-wrap items-baseline py-2 nav-item\">"
                        , "                <a href=\"install/\" class=\"nav-link\">Installation</a>"
                        , "            </li>"
                        , "            <li class=\"box-border flex flex-wrap items-baseline py-2 nav-item\">"
                        , "                <a href=\"steps/\" class=\"nav-link\">First steps</a>"
                        , "            </li>"
                        ]
            let liParser = elemParserS (Just ["li"]) []
            case parse (someHtml liParser) "" html of
                Left err -> expectationFailure $ "Failed to parse li siblings: " ++ show err
                Right elems -> do
                    length elems `shouldBe` 3
                    map elTag elems `shouldBe` ["li", "li", "li"]
                    -- Verify each li has proper content
                    all (\e -> "<a href=" `isInfixOf` innerText' e) elems `shouldBe` True

        it "parses ghcup nav li elements after stripping leading whitespace" $ do
            -- Real-world pattern: inner HTML from ul has leading whitespace
            let htmlWithLeading =
                    unlines
                        [ "            <li class=\"box-border flex nav-item\">"
                        , "                <a href=\".\">GHCup</a>"
                        , "            </li>"
                        , "            <li class=\"box-border flex nav-item\">"
                        , "                <a href=\"install/\">Installation</a>"
                        , "            </li>"
                        ]
            -- Strip leading whitespace before parsing
            let html = dropWhile (`elem` (" \n\t" :: String)) htmlWithLeading
            let liParser = elemParserS (Just ["li"]) []
            case parse (someHtml liParser) "" html of
                Left err -> expectationFailure $ "Failed to parse li siblings: " ++ show err
                Right elems -> do
                    length elems `shouldBe` 2
                    map elTag elems `shouldBe` ["li", "li"]

        it "handles deeply indented closing tags" $ do
            -- Real-world pattern: closing tag on its own line with indentation
            let html =
                    unlines
                        [ "<div class=\"container\">"
                        , "    <span class=\"content\">"
                        , "        Some text here"
                        , "    </span>"
                        , "</div>"
                        ]
            let result = parse (elemParserS (Just ["div"]) []) "" html
            case result of
                Left err -> expectationFailure $ show err
                Right divElem -> do
                    elTag divElem `shouldBe` "div"
                    innerText' divElem `shouldContain` "<span class=\"content\">"
                    innerText' divElem `shouldContain` "Some text here"

    describe "eitherP" $ do
        it "returns Left when first parser succeeds" $ do
            let p = eitherP (string "abc") (string "xyz") :: ParsecT String () Identity (Either String String)
            case parse p "" "abc" of
                Right (Left v) -> v `shouldBe` "abc"
                Right (Right _) -> expectationFailure "Expected Left"
                Left err -> expectationFailure $ show err

        it "returns Right when first fails and second succeeds" $ do
            let p = eitherP (string "abc") (string "xyz") :: ParsecT String () Identity (Either String String)
            case parse p "" "xyz" of
                Right (Right v) -> v `shouldBe` "xyz"
                Right (Left _) -> expectationFailure "Expected Right"
                Left err -> expectationFailure $ show err

        it "fails when both parsers fail" $ do
            let p = eitherP (string "abc") (string "xyz") :: ParsecT String () Identity (Either String String)
            case parse p "" "qqq" of
                Right _ -> expectationFailure "Expected failure"
                Left _ -> pure ()

    describe "manyTill_" $ do
        it "collects items and returns end result" $ do
            let p = manyTill_ anyChar (string "END") :: ParsecT String () Identity (String, String)
            case parse p "" "abcEND" of
                Right (collected, end) -> do
                    collected `shouldBe` "abc"
                    end `shouldBe` "END"
                Left err -> expectationFailure $ show err

        it "returns empty list when end is immediate" $ do
            let p = manyTill_ anyChar (string "END") :: ParsecT String () Identity (String, String)
            case parse p "" "END" of
                Right (collected, end) -> do
                    collected `shouldBe` ""
                    end `shouldBe` "END"
                Left err -> expectationFailure $ show err

    describe "elemParserWhere" $ do
        it "matches element when predicate holds" $ do
            let p = elemParserWhere Nothing (Nothing :: Maybe (ParsecT String () Identity String)) "class" (== "active")
            case parse p "" "<div class=\"active\">content</div>" of
                Right e -> do
                    elTag e `shouldBe` "div"
                    Map.lookup "class" (attrs e) `shouldBe` Just "active"
                Left err -> expectationFailure $ show err

        it "fails when predicate does not hold" $ do
            let p = elemParserWhere Nothing (Nothing :: Maybe (ParsecT String () Identity String)) "class" (== "active")
            case parse p "" "<div class=\"inactive\">content</div>" of
                Right _ -> expectationFailure "Expected failure"
                Left _ -> pure ()

    describe "sameElTag" $ do
        it "matches element by tag" $ do
            let p = sameElTag "div" (Nothing :: Maybe (ParsecT String () Identity String))
            case parse p "" "<div>hello</div>" of
                Right e -> do
                    elTag e `shouldBe` "div"
                    innerText' e `shouldBe` "hello"
                Left err -> expectationFailure $ show err

        it "fails on different tag" $ do
            let p = sameElTag "div" (Nothing :: Maybe (ParsecT String () Identity String))
            case parse p "" "<span>hello</span>" of
                Right _ -> expectationFailure "Expected failure"
                Left _ -> pure ()

    describe "matchesInSameElTag" $ do
        it "returns matches from inner parser" $ do
            let inner = Just (string "target") :: Maybe (ParsecT String () Identity String)
            let p = matchesInSameElTag "div" inner
            case parse p "" "<div>before target after</div>" of
                Right ms -> length ms `shouldSatisfy` (>= 1)
                Left err -> expectationFailure $ show err

        it "returns empty when no matches found" $ do
            let p = matchesInSameElTag "div" (Nothing :: Maybe (ParsecT String () Identity String))
            case parse p "" "<div>content</div>" of
                Right ms -> length ms `shouldBe` 0
                Left err -> expectationFailure $ show err

    describe "elSelfC" $ do
        it "parses self-closing element" $ do
            let p = elSelfC (Just ["br"]) [] :: ParsecT String () Identity (Elem' String)
            case parse p "" "<br>" of
                Right e -> elTag e `shouldBe` "br"
                Left err -> expectationFailure $ show err

        it "parses img with attributes" $ do
            let p = elSelfC (Just ["img"]) [("src", Just "pic.jpg")] :: ParsecT String () Identity (Elem' String)
            case parse p "" "<img src=\"pic.jpg\">" of
                Right e -> do
                    elTag e `shouldBe` "img"
                    Map.lookup "src" (attrs e) `shouldBe` Just "pic.jpg"
                Left err -> expectationFailure $ show err

    describe "elSelfClosing" $ do
        it "parses without inner spec" $ do
            let p = elSelfClosing (Just ["hr"]) (Nothing :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<hr>" of
                Right e -> elTag e `shouldBe` "hr"
                Left err -> expectationFailure $ show err

        it "fails with inner spec (self-closing can't have body)" $ do
            let p = elSelfClosing (Just ["hr"]) (Just (string "x") :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<hr>" of
                Right _ -> expectationFailure "Expected failure"
                Left _ -> pure ()

    describe "elemWithBody" $ do
        it "succeeds when inner matches are found" $ do
            let inner = Just (string "target") :: Maybe (ParsecT String () Identity String)
            let p = elemWithBody Nothing inner []
            case parse p "" "<div>before target after</div>" of
                Right e -> do
                    elTag e `shouldBe` "div"
                    length (matches' e) `shouldSatisfy` (>= 1)
                Left err -> expectationFailure $ show err

        it "succeeds with no inner spec (no match minimum)" $ do
            let p = elemWithBody Nothing (Nothing :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<div>content</div>" of
                Right e -> elTag e `shouldBe` "div"
                Left err -> expectationFailure $ show err

    describe "elemParserInternal" $ do
        it "parses normal element" $ do
            let p = elemParserInternal Nothing (Nothing :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<p>text</p>" of
                Right e -> do
                    elTag e `shouldBe` "p"
                    innerText' e `shouldBe` "text"
                Left err -> expectationFailure $ show err

        it "handles self-closing via />" $ do
            let p = elemParserInternal (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<div/>" of
                Right e -> elTag e `shouldBe` "div"
                Left err -> expectationFailure $ show err

    describe "stylingTags" $ do
        it "contains expected tags" $ do
            "b" `elem` stylingTags `shouldBe` True
            "em" `elem` stylingTags `shouldBe` True
            "strong" `elem` stylingTags `shouldBe` True
            "i" `elem` stylingTags `shouldBe` True

        it "does not contain structural tags" $ do
            "div" `elem` stylingTags `shouldBe` False
            "p" `elem` stylingTags `shouldBe` False

    describe "stylingElem" $ do
        it "parses bold element and returns inner text" $ do
            case parse stylingElem "" "<b>bold text</b>" of
                Right s -> s `shouldBe` "bold text"
                Left err -> expectationFailure $ show err

        it "parses em element" $ do
            case parse stylingElem "" "<em>emphasis</em>" of
                Right s -> s `shouldBe` "emphasis"
                Left err -> expectationFailure $ show err

        it "fails on non-styling element" $ do
            case parse stylingElem "" "<div>content</div>" of
                Right _ -> expectationFailure "Expected failure"
                Left _ -> pure ()

    describe "clickableHref" $ do
        it "parses anchor with href" $ do
            let cUrl = Link "https://example.com"
            let p = clickableHref False cUrl :: ParsecT String () Identity Clickable
            case parse p "" "<a href=\"https://example.com/page\">Link</a>" of
                Right (Clickable _ link) -> link `shouldBe` Link "https://example.com/page"
                Left err -> expectationFailure $ show err

    describe "clickableHref'" $ do
        it "parses anchor with inner pattern" $ do
            let cUrl = Link "https://example.com"
            let inner = string "text" :: ParsecT String () Identity String
            let p = clickableHref' inner False cUrl
            case parse p "" "<a href=\"https://example.com/page\">text</a>" of
                Right (Clickable _ link) -> link `shouldBe` Link "https://example.com/page"
                Left err -> expectationFailure $ show err

    describe "parseInnerHTMLAndEndTag (deprecated)" $ do
        it "parses inner HTML with pattern" $ do
            let inner = Just (string "target") :: Maybe (ParsecT String () Identity String)
            let p = char '<' >> string "div" >> parseInnerHTMLAndEndTag "div" inner
            case parse p "" "<div>before target after</div>" of
                Right itr -> do
                    length (_matchesITR itr) `shouldSatisfy` (>= 1)
                    _fullInner itr `shouldContain` "target"
                Left err -> expectationFailure $ show err

    describe "elemParserOld (deprecated)" $ do
        it "parses element like elemParser" $ do
            let p = elemParserOld (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<div>content</div>" of
                Right e -> do
                    elTag e `shouldBe` "div"
                    innerText' e `shouldBe` "content"
                Left err -> expectationFailure $ show err

-- Helper for tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : xs') = xs : tails xs'

-- Helper for isInfixOf
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
