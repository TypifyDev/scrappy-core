{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Elem.TreeElemParserSpec (spec) where

import Data.Functor.Identity (Identity)
import Data.Graph (Tree (Node))
import qualified Data.Map as Map
import Test.Hspec
import Text.Parsec (ParsecT, anyChar, char, parse, string)

import Scrappy.Elem.TreeElemParser (
    Many (..),
    findAllMutExGroups',
    fromMany,
    groupify,
    manyTill_,
    selfClosing,
    skipManyTill,
    specificChar,
    specificChar',
    stylingElem,
    stylingTags,
    takeTill,
    treeLookupIdx,
    treeElemParser,
    treeElemParser',
    tryElHeads',
    validateGPR,
 )
import qualified Scrappy.Elem.Types as ST

import TestUtils (shouldBeUndefined)

-- Helper type alias for TreeHTML parser
type TreeParser = ParsecT String () Identity (ST.TreeHTML String)

-- Helper to create treeElemParser with explicit String type
treeParserS :: Maybe [String] -> [(String, Maybe String)] -> TreeParser
treeParserS tags attrss = treeElemParser tags (Nothing :: Maybe (ParsecT String () Identity String)) attrss

spec :: Spec
spec = do
    describe "treeElemParser basic parsing" $ do
        it "parses a simple element" $ do
            let result = parse (treeParserS Nothing []) "" "<div>content</div>"
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    ST._innerText' tree `shouldBe` "content"
                Left err -> expectationFailure $ show err

        it "parses element with attributes" $ do
            let result = parse (treeParserS Nothing []) "" "<div class=\"test\">content</div>"
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    Map.lookup "class" (ST._topAttrs tree) `shouldBe` Just "test"
                Left err -> expectationFailure $ show err

    describe "nested elements" $ do
        it "parses simple nested elements" $ do
            let html = "<div><span>text</span></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    ST._innerText' tree `shouldContain` "<span>text</span>"
                Left err -> expectationFailure $ show err

        it "parses nested elements on same line" $ do
            let html = "<div class=\"outer\"><div class=\"inner\">content</div></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    Map.lookup "class" (ST._topAttrs tree) `shouldBe` Just "outer"
                    -- The inner div should be in the innerText
                    ST._innerText' tree `shouldContain` "<div class=\"inner\">content</div>"
                Left err -> expectationFailure $ show err

        -- THIS IS THE BUG: nested elements with whitespace/newline before them
        it "parses nested elements with newline before inner element" $ do
            let html = "<div class=\"outer\">\n<div class=\"inner\">content</div></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    Map.lookup "class" (ST._topAttrs tree) `shouldBe` Just "outer"
                    -- CRITICAL: The inner div MUST be captured in innerText
                    ST._innerText' tree `shouldContain` "<div class=\"inner\">content</div>"
                Left err -> expectationFailure $ show err

        it "parses nested elements with indentation before inner element" $ do
            let html = "<div class=\"outer\">\n            <div class=\"inner\">content</div></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    -- CRITICAL: The inner div MUST be captured in innerText
                    ST._innerText' tree `shouldContain` "<div class=\"inner\">"
                    ST._innerText' tree `shouldContain` "content"
                Left err -> expectationFailure $ show err

        it "parses deeply nested elements with whitespace" $ do
            let html =
                    unlines
                        [ "<div class=\"level1\">"
                        , "  <div class=\"level2\">"
                        , "    <div class=\"level3\">deep</div>"
                        , "  </div>"
                        , "</div>"
                        ]
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    ST._innerText' tree `shouldContain` "level2"
                    ST._innerText' tree `shouldContain` "level3"
                    ST._innerText' tree `shouldContain` "deep"
                Left err -> expectationFailure $ show err

    describe "ghcup page patterns" $ do
        -- This is the exact pattern from ghcup that was failing
        it "parses flex div with nested indented div" $ do
            let html =
                    unlines
                        [ "<div class=\"flex flex-wrap -mx-3\">"
                        , "            <div class=\"w-full md:w-1/4 px-3\"><div class=\"sticky\">sidebar</div></div>"
                        , "            <div class=\"w-full md:w-3/4 px-3\" role=\"main\">main content</div>"
                        , "</div>"
                        ]
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    Map.lookup "class" (ST._topAttrs tree) `shouldBe` Just "flex flex-wrap -mx-3"
                    -- Both nested divs should be in innerText
                    ST._innerText' tree `shouldContain` "w-full md:w-1/4"
                    ST._innerText' tree `shouldContain` "w-full md:w-3/4"
                    ST._innerText' tree `shouldContain` "sidebar"
                    ST._innerText' tree `shouldContain` "main content"
                Left err -> expectationFailure $ show err

        it "parses container with multiple nested sections" $ do
            let html =
                    unlines
                        [ "<div class=\"max-w-7xl mx-auto px-3 mt-5\">"
                        , "    <div class=\"flex flex-wrap -mx-3\">"
                        , "            <div class=\"w-full md:w-1/4 px-3\">"
                        , "                <nav class=\"sidebar\">nav content</nav>"
                        , "            </div>"
                        , "            <div class=\"w-full md:w-3/4 px-3\" role=\"main\">"
                        , "                <section>main content</section>"
                        , "            </div>"
                        , "    </div>"
                        , "</div>"
                        ]
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    -- The flex wrapper should be in innerText
                    ST._innerText' tree `shouldContain` "flex flex-wrap"
                    -- Both columns should be present
                    ST._innerText' tree `shouldContain` "w-full md:w-1/4"
                    ST._innerText' tree `shouldContain` "w-full md:w-3/4"
                    ST._innerText' tree `shouldContain` "nav content"
                    ST._innerText' tree `shouldContain` "main content"
                Left err -> expectationFailure $ show err

    describe "self-closing elements" $ do
        it "parses self-closing void elements" $ do
            let html = "<div><br><img src=\"test.jpg\"><hr></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    ST._innerText' tree `shouldContain` "<br>"
                    ST._innerText' tree `shouldContain` "<img"
                    ST._innerText' tree `shouldContain` "<hr>"
                Left err -> expectationFailure $ show err

        it "parses self-closing with slash notation" $ do
            let html = "<div><br/><img src=\"test.jpg\"/></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    -- Self-closing elements should be captured
                    ST._innerText' tree `shouldContain` "br"
                    ST._innerText' tree `shouldContain` "img"
                Left err -> expectationFailure $ show err

    describe "edge cases" $ do
        it "handles empty inner content" $ do
            let result = parse (treeParserS Nothing []) "" "<div></div>"
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    ST._innerText' tree `shouldBe` ""
                Left err -> expectationFailure $ show err

        it "handles whitespace-only inner content" $ do
            let result = parse (treeParserS Nothing []) "" "<div>   \n   </div>"
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    -- Whitespace should be preserved
                    ST._innerText' tree `shouldSatisfy` (not . null)
                Left err -> expectationFailure $ show err

        it "handles mixed text and elements" $ do
            let html = "<div>text before <span>span</span> text after</div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    ST._topEl tree `shouldBe` "div"
                    ST._innerText' tree `shouldContain` "text before"
                    ST._innerText' tree `shouldContain` "<span>span</span>"
                    ST._innerText' tree `shouldContain` "text after"
                Left err -> expectationFailure $ show err

    describe "_rawInner provides direct child access (THE FIX)" $ do
        -- _rawInner preserves the raw HTMLMatcher list, allowing direct iteration
        -- over child elements without re-parsing

        it "rawInner provides direct access to child elements" $ do
            -- Original HTML with nested div (same case that failed with re-parsing)
            let html = "<div class=\"outer\">\n  <div class=\"inner\">content</div>\n</div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right outerTree -> do
                    -- _rawInner gives us direct access to children!
                    let rawChildren = ST._rawInner outerTree
                    -- Should have: IText "\n  ", Element innerDiv, IText "\n"
                    length rawChildren `shouldSatisfy` (>= 2)

                    -- Find the Element in rawChildren
                    let elements = [t | ST.Element t <- rawChildren]
                    length elements `shouldBe` 1

                    -- Direct access to the inner TreeHTML - no re-parsing needed!
                    let innerTree = elements !! 0
                    ST._topEl innerTree `shouldBe` "div"
                    Map.lookup "class" (ST._topAttrs innerTree) `shouldBe` Just "inner"
                    ST._innerText' innerTree `shouldBe` "content"
                Left err -> expectationFailure $ show err

        it "rawInner preserves multiple children with text between" $ do
            -- Parse div with two child divs
            let html = "<div><div class=\"a\">first</div><div class=\"b\">second</div></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    let rawChildren = ST._rawInner tree
                    -- Extract just the Element children
                    let elements = [t | ST.Element t <- rawChildren]
                    length elements `shouldBe` 2

                    -- First child
                    ST._topEl (elements !! 0) `shouldBe` "div"
                    Map.lookup "class" (ST._topAttrs (elements !! 0)) `shouldBe` Just "a"
                    ST._innerText' (elements !! 0) `shouldBe` "first"

                    -- Second child
                    ST._topEl (elements !! 1) `shouldBe` "div"
                    Map.lookup "class" (ST._topAttrs (elements !! 1)) `shouldBe` Just "b"
                    ST._innerText' (elements !! 1) `shouldBe` "second"
                Left err -> expectationFailure $ show err

        it "rawInner captures text nodes between elements" $ do
            let html = "<div>before <span>middle</span> after</div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    let rawChildren = ST._rawInner tree
                    -- Should have: IText "before ", Element span, IText " after"
                    let texts = [s | ST.IText s <- rawChildren]
                    let elements = [t | ST.Element t <- rawChildren]

                    length elements `shouldBe` 1
                    -- Text content is captured (may be split into multiple IText)
                    concat texts `shouldContain` "before"
                    concat texts `shouldContain` "after"
                Left err -> expectationFailure $ show err

        it "rawInner provides full TreeHTML for deeply nested structures" $ do
            let html = "<div><span class=\"highlight\">important text</span></div>"
            let result = parse (treeParserS Nothing []) "" html
            case result of
                Right tree -> do
                    -- _innerTree' only has (Elem, Attrs) - no content
                    let forest = ST._innerTree' tree
                    length forest `shouldBe` 1

                    -- But _rawInner has full TreeHTML with content!
                    let elements = [t | ST.Element t <- ST._rawInner tree]
                    length elements `shouldBe` 1

                    let spanTree = elements !! 0
                    ST._topEl spanTree `shouldBe` "span"
                    ST._innerText' spanTree `shouldBe` "important text"
                Left err -> expectationFailure $ show err

    describe "skipManyTill" $ do
        it "skips characters until end parser succeeds" $ do
            let p = skipManyTill anyChar (string "END") :: ParsecT String () Identity String
            case parse p "" "xxxENDrest" of
                Right r -> r `shouldBe` "END"
                Left err -> expectationFailure $ show err

        it "succeeds immediately when end is at start" $ do
            let p = skipManyTill anyChar (string "END") :: ParsecT String () Identity String
            case parse p "" "END" of
                Right r -> r `shouldBe` "END"
                Left err -> expectationFailure $ show err

    describe "manyTill_" $ do
        it "collects items and end" $ do
            let p = manyTill_ anyChar (string "END") :: ParsecT String () Identity (String, String)
            case parse p "" "abcEND" of
                Right (xs, end) -> do
                    xs `shouldBe` "abc"
                    end `shouldBe` "END"
                Left err -> expectationFailure $ show err

        it "empty collection when end is immediate" $ do
            let p = manyTill_ anyChar (string "END") :: ParsecT String () Identity (String, String)
            case parse p "" "END" of
                Right (xs, end) -> do
                    xs `shouldBe` ""
                    end `shouldBe` "END"
                Left err -> expectationFailure $ show err

    describe "Many" $ do
        it "Many constructor" $ do
            show (Many (42 :: Int)) `shouldContain` "Many"

        it "One constructor" $ do
            show (One (42 :: Int)) `shouldContain` "One"

    describe "selfClosing" $ do
        it "contains expected tags" $ do
            "br" `elem` selfClosing `shouldBe` True
            "img" `elem` selfClosing `shouldBe` True
            "input" `elem` selfClosing `shouldBe` True
            "hr" `elem` selfClosing `shouldBe` True

        it "does not contain non-void tags" $ do
            "div" `elem` selfClosing `shouldBe` False
            "span" `elem` selfClosing `shouldBe` False

    describe "treeElemParser'" $ do
        it "parses self-closing element" $ do
            let p = treeElemParser' (Just ["br"]) (Nothing :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<br>" of
                Right tree -> ST._topEl tree `shouldBe` "br"
                Left err -> expectationFailure $ show err

        it "parses normal element" $ do
            let p = treeElemParser' Nothing (Nothing :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<p>text</p>" of
                Right tree -> do
                    ST._topEl tree `shouldBe` "p"
                    ST._innerText' tree `shouldBe` "text"
                Left err -> expectationFailure $ show err

        it "fails self-closing with inner spec" $ do
            let p = treeElemParser' (Just ["br"]) (Just (string "x") :: Maybe (ParsecT String () Identity String)) []
            case parse p "" "<br>" of
                Right _ -> expectationFailure "Expected failure"
                Left _ -> pure ()

    describe "takeTill" $ do
        it "takes until predicate matches (inclusive)" $ do
            takeTill (== (3 :: Int)) [1, 2, 3, 4, 5] `shouldBe` [1, 2, 3]

        it "returns empty for empty list" $ do
            takeTill (== (1 :: Int)) [] `shouldBe` []

        it "returns entire list if predicate never matches" $ do
            takeTill (> (10 :: Int)) [1, 2, 3] `shouldBe` [1, 2, 3]

        it "includes matching element" $ do
            takeTill (== (1 :: Int)) [1, 2, 3] `shouldBe` [1]

    describe "fromMany" $ do
        it "extracts from One" $ do
            fromMany (One (42 :: Int)) `shouldBe` 42

        it "extracts from Many" $ do
            fromMany (Many (99 :: Int)) `shouldBe` 99

    describe "groupify" $ do
        it "empty input yields accumulator" $ do
            groupify ([] :: [Tree (String, Map.Map String String)]) [] `shouldBe` []

        it "single element becomes One" $ do
            let tree = Node ("div", Map.empty) [] :: Tree (String, Map.Map String String)
            case groupify [tree] [] of
                [One _] -> pure ()
                other -> expectationFailure $ "Expected [One _] but got " ++ show other

        it "two identical elements become Many" $ do
            let tree = Node ("div", Map.empty) [] :: Tree (String, Map.Map String String)
            let result = groupify [tree, tree] []
            case result of
                [Many _] -> pure ()
                other -> expectationFailure $ "Expected [Many _] but got " ++ show other

        it "different elements stay separate" $ do
            let t1 = Node ("div", Map.empty) [] :: Tree (String, Map.Map String String)
            let t2 = Node ("span", Map.empty) [] :: Tree (String, Map.Map String String)
            let result = groupify [t1, t2] []
            length result `shouldBe` 2

    describe "tryElHeads'" $ do
        it "returns Left for empty list" $ do
            case tryElHeads' ("div", Map.empty) ([] :: [Many (Tree (String, Map.Map String String))]) of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected Left"

        it "matches Many element and keeps it" $ do
            let tree = Many (Node ("div", Map.empty) []) :: Many (Tree (String, Map.Map String String))
            case tryElHeads' ("div", Map.empty) [tree] of
                Right (forest, stack) -> do
                    length forest `shouldBe` 0
                    length stack `shouldBe` 1
                Left err -> expectationFailure err

        it "matches One element and removes it" $ do
            let tree = One (Node ("div", Map.empty) []) :: Many (Tree (String, Map.Map String String))
            case tryElHeads' ("div", Map.empty) [tree] of
                Right (forest, stack) -> do
                    length forest `shouldBe` 0
                    length stack `shouldBe` 0
                Left err -> expectationFailure err

        it "fails on mismatched One" $ do
            let tree = One (Node ("span", Map.empty) []) :: Many (Tree (String, Map.Map String String))
            case tryElHeads' ("div", Map.empty) [tree] of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected Left"

    describe "validateGPR" $ do
        it "succeeds when no One elements remain" $ do
            let xs = [Many (Node ("div", Map.empty) [])] :: [Many (Tree (String, Map.Map String String))]
            let p = validateGPR xs :: ParsecT String () Identity [ST.HTMLMatcher ST.TreeHTML String]
            case parse p "" "" of
                Right result -> length result `shouldBe` 0
                Left err -> expectationFailure $ show err

        it "fails when One elements remain" $ do
            let xs = [One (Node ("div", Map.empty) [])] :: [Many (Tree (String, Map.Map String String))]
            let p = validateGPR xs :: ParsecT String () Identity [ST.HTMLMatcher ST.TreeHTML String]
            case parse p "" "" of
                Right _ -> expectationFailure "Expected failure"
                Left _ -> pure ()

    describe "specificChar" $ do
        it "parses a non-tag character" $ do
            case parse (specificChar :: ParsecT String () Identity Char) "" "x" of
                Right c -> c `shouldBe` 'x'
                Left err -> expectationFailure $ show err

    describe "specificChar'" $ do
        it "parses a non-tag, non-endtag character" $ do
            case parse (specificChar' "div" :: ParsecT String () Identity Char) "" "x" of
                Right c -> c `shouldBe` 'x'
                Left err -> expectationFailure $ show err

    describe "stylingTags" $ do
        it "contains expected styling tags" $ do
            "b" `elem` stylingTags `shouldBe` True
            "em" `elem` stylingTags `shouldBe` True
            "i" `elem` stylingTags `shouldBe` True

        it "does not contain structural tags" $ do
            "div" `elem` stylingTags `shouldBe` False
            "p" `elem` stylingTags `shouldBe` False

    describe "stylingElem" $ do
        it "parses bold tag" $ do
            case parse (stylingElem :: ParsecT String () Identity String) "" "<b>bold</b>" of
                Right s -> s `shouldBe` "bold"
                Left err -> expectationFailure $ show err

        it "parses em tag" $ do
            case parse (stylingElem :: ParsecT String () Identity String) "" "<em>emph</em>" of
                Right s -> s `shouldBe` "emph"
                Left err -> expectationFailure $ show err

    describe "undefined functions" $ do
        it "treeLookupIdx is undefined" $
            shouldBeUndefined (treeLookupIdx ([] :: [Int]) [])

        it "findAllMutExGroups' is undefined" $
            shouldBeUndefined (findAllMutExGroups' :: ())
