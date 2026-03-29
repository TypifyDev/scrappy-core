{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Elem.ElemHeadParseSpec (spec) where

import qualified Data.Functor.Identity
import qualified Data.Map as Map
import Test.Hspec
import Test.QuickCheck
import qualified Text.Parsec
import Text.Parsec (parse)

import Scrappy.Elem.ElemHeadParse
    ( attrName
    , attrParser
    , attrValue
    , attrValueExists
    , attrValuesExist
    , attrsFit
    , attrsMatch
    , attrsMatch'
    , attrsParser
    , attrsParserDesc
    , buildElemsOpts
    , digitEq
    , digitEqFree
    , href
    , href'
    , hrefParser
    , isAttrsMatch
    , isAttrsMatch'
    , mkAttrsDesc
    , mkElemtagParser
    , parseAttrSafe
    , parseOpeningTag
    , parseOpeningTagDesc
    , parseOpeningTagF
    , parseOpeningTagWhere
    , saveDigitEq
    , svDigEq
    , unfit
    )
import Scrappy.Elem.Types (AttrsError (..))
import Scrappy.Links (Link (..))

import TestUtils (shouldBeUndefined)

spec :: Spec
spec = do
    ---------------------------------------------------------------------------
    -- parseOpeningTag
    ---------------------------------------------------------------------------
    describe "parseOpeningTag" $ do
        it "parses a simple div tag" $ do
            let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div>"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "div"
                    as `shouldBe` Map.empty
                Left err -> expectationFailure $ show err

        it "parses a tag with single attribute" $ do
            let result = parse (parseOpeningTag (Just ["a"]) []) "" "<a href=\"/page\">"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "a"
                    Map.lookup "href" as `shouldBe` Just "/page"
                Left err -> expectationFailure $ show err

        it "parses a tag with multiple attributes" $ do
            let result = parse (parseOpeningTag (Just ["input"]) []) "" "<input type=\"text\" name=\"field\" value=\"hello\">"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "input"
                    Map.lookup "type" as `shouldBe` Just "text"
                    Map.lookup "name" as `shouldBe` Just "field"
                    Map.lookup "value" as `shouldBe` Just "hello"
                Left err -> expectationFailure $ show err

        it "parses any element when elemOpts is Nothing" $ do
            let result = parse (parseOpeningTag Nothing []) "" "<span>"
            case result of
                Right (tg, _) -> tg `shouldBe` "span"
                Left err -> expectationFailure $ show err

        it "fails when element is not in allowed list" $ do
            let result = parse (parseOpeningTag (Just ["div", "span"]) []) "" "<p>"
            case result of
                Right _ -> expectationFailure "Should have failed"
                Left _ -> return ()

        it "parses tag with required attribute subset (name only)" $ do
            let result = parse (parseOpeningTag (Just ["a"]) [("href", Nothing)]) "" "<a href=\"/link\" class=\"btn\">"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "a"
                    Map.lookup "href" as `shouldBe` Just "/link"
                Left err -> expectationFailure $ show err

        it "fails when required attribute is missing" $ do
            let result = parse (parseOpeningTag (Just ["a"]) [("href", Nothing)]) "" "<a class=\"btn\">"
            case result of
                Right _ -> expectationFailure "Should have failed - href required"
                Left _ -> return ()

        it "parses tag with specific attribute value" $ do
            let result = parse (parseOpeningTag (Just ["input"]) [("type", Just "text")]) "" "<input type=\"text\">"
            case result of
                Right (tg, _) -> tg `shouldBe` "input"
                Left err -> expectationFailure $ show err

        it "fails when attribute value doesn't match" $ do
            let result = parse (parseOpeningTag (Just ["input"]) [("type", Just "text")]) "" "<input type=\"password\">"
            case result of
                Right _ -> expectationFailure "Should have failed - type should be 'text'"
                Left _ -> return ()

        it "handles trailing whitespace after attributes" $ do
            let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div class=\"a\" >"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "div"
                    Map.lookup "class" as `shouldBe` Just "a"
                Left err -> expectationFailure $ show err

    ---------------------------------------------------------------------------
    -- attrParser
    ---------------------------------------------------------------------------
    describe "attrParser" $ do
        it "parses attribute with double-quoted value" $ do
            let result = parse attrParser "" " href=\"/page\""
            case result of
                Right (nm, val) -> do
                    nm `shouldBe` "href"
                    val `shouldBe` "/page"
                Left err -> expectationFailure $ show err

        it "parses attribute with single-quoted value" $ do
            let result = parse attrParser "" " href='/page'"
            case result of
                Right (nm, val) -> do
                    nm `shouldBe` "href"
                    val `shouldBe` "/page"
                Left err -> expectationFailure $ show err

        it "parses boolean attribute (no value)" $ do
            let result = parse attrParser "" " disabled"
            case result of
                Right (nm, val) -> do
                    nm `shouldBe` "disabled"
                    val `shouldBe` ""
                Left err -> expectationFailure $ show err

        it "parses attribute with hyphen in name" $ do
            let result = parse attrParser "" " data-value=\"test\""
            case result of
                Right (nm, val) -> do
                    nm `shouldBe` "data-value"
                    val `shouldBe` "test"
                Left err -> expectationFailure $ show err

        it "parses attribute with underscore in name" $ do
            let result = parse attrParser "" " my_attr=\"value\""
            case result of
                Right (nm, _) -> nm `shouldBe` "my_attr"
                Left err -> expectationFailure $ show err

        it "parses attribute with numbers in name" $ do
            let result = parse attrParser "" " data1=\"value\""
            case result of
                Right (nm, _) -> nm `shouldBe` "data1"
                Left err -> expectationFailure $ show err

        it "handles leading whitespace including tabs and newlines" $ do
            let result = parse attrParser "" "\t\nclass=\"btn\""
            case result of
                Right (nm, val) -> do
                    nm `shouldBe` "class"
                    val `shouldBe` "btn"
                Left err -> expectationFailure $ show err

        it "parses empty attribute value" $ do
            let result = parse attrParser "" " title=\"\""
            case result of
                Right (nm, val) -> do
                    nm `shouldBe` "title"
                    val `shouldBe` ""
                Left err -> expectationFailure $ show err

    ---------------------------------------------------------------------------
    -- attrValue
    ---------------------------------------------------------------------------
    describe "attrValue" $ do
        it "parses double-quoted value" $ do
            let result = parse attrValue "" "\"hello world\""
            result `shouldBe` Right "hello world"

        it "parses single-quoted value" $ do
            let result = parse attrValue "" "'hello world'"
            result `shouldBe` Right "hello world"

        it "parses value with special characters" $ do
            let result = parse attrValue "" "\"hello&nbsp;world\""
            result `shouldBe` Right "hello&nbsp;world"

        it "parses value with URL" $ do
            let result = parse attrValue "" "\"https://example.com/path?query=1&other=2\""
            result `shouldBe` Right "https://example.com/path?query=1&other=2"

        it "parses empty double-quoted value" $ do
            let result = parse attrValue "" "\"\""
            result `shouldBe` Right ""

        it "parses empty single-quoted value" $ do
            let result = parse attrValue "" "''"
            result `shouldBe` Right ""

    ---------------------------------------------------------------------------
    -- attrName
    ---------------------------------------------------------------------------
    describe "attrName" $ do
        it "parses simple name" $ do
            let result = parse attrName "" "class"
            result `shouldBe` Right "class"

        it "parses name with hyphen" $ do
            let result = parse attrName "" "data-id"
            result `shouldBe` Right "data-id"

        it "parses name with underscore" $ do
            let result = parse attrName "" "my_attr"
            result `shouldBe` Right "my_attr"

        it "parses name with numbers" $ do
            let result = parse attrName "" "h1"
            result `shouldBe` Right "h1"

        it "fails on empty input" $ do
            let result = parse attrName "" ""
            case result of
                Right _ -> expectationFailure "Should fail on empty"
                Left _ -> return ()

    ---------------------------------------------------------------------------
    -- attrsParser
    ---------------------------------------------------------------------------
    describe "attrsParser" $ do
        it "returns Right Map when all required attrs present (name only)" $ do
            let result = parse (attrsParser [("href", Nothing)]) "" " href=\"/page\""
            case result of
                Right (Right mp) -> Map.lookup "href" mp `shouldBe` Just "/page"
                Right (Left IncorrectAttrs) -> expectationFailure "Should have matched"
                Left err -> expectationFailure $ show err

        it "returns Right Map when required attrs match by value" $ do
            let result = parse (attrsParser [("type", Just "text")]) "" " type=\"text\""
            case result of
                Right (Right mp) -> Map.lookup "type" mp `shouldBe` Just "text"
                Right (Left IncorrectAttrs) -> expectationFailure "Should have matched"
                Left err -> expectationFailure $ show err

        it "returns Left IncorrectAttrs when required attr is missing" $ do
            let result = parse (attrsParser [("id", Nothing)]) "" " class=\"btn\""
            case result of
                Right (Left IncorrectAttrs) -> return ()
                Right (Right _) -> expectationFailure "Should have returned IncorrectAttrs"
                Left err -> expectationFailure $ show err

        it "returns Left IncorrectAttrs when required value does not match" $ do
            let result = parse (attrsParser [("type", Just "text")]) "" " type=\"password\""
            case result of
                Right (Left IncorrectAttrs) -> return ()
                Right (Right _) -> expectationFailure "Should have returned IncorrectAttrs"
                Left err -> expectationFailure $ show err

        it "returns Right empty map with no desired attrs and no input attrs" $ do
            let result = parse (attrsParser []) "" ""
            case result of
                Right (Right mp) -> mp `shouldBe` Map.empty
                Right (Left IncorrectAttrs) -> expectationFailure "Should have matched"
                Left err -> expectationFailure $ show err

    ---------------------------------------------------------------------------
    -- isAttrsMatch
    ---------------------------------------------------------------------------
    describe "isAttrsMatch" $ do
        it "returns True for empty desired attrs" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            isAttrsMatch mapAttr [] `shouldBe` True

        it "returns True when attr exists with any value (Nothing)" $ do
            let mapAttr = Map.fromList [("href", "/page")]
            isAttrsMatch mapAttr [("href", Nothing)] `shouldBe` True

        it "returns True when attr exists with exact value" $ do
            let mapAttr = Map.fromList [("type", "text")]
            isAttrsMatch mapAttr [("type", Just "text")] `shouldBe` True

        it "returns False when attr value doesn't match" $ do
            let mapAttr = Map.fromList [("type", "password")]
            isAttrsMatch mapAttr [("type", Just "text")] `shouldBe` False

        it "returns False when attr is missing" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            isAttrsMatch mapAttr [("href", Nothing)] `shouldBe` False

        it "handles multiple attrs all matching" $ do
            let mapAttr = Map.fromList [("type", "text"), ("name", "field")]
            isAttrsMatch mapAttr [("type", Just "text"), ("name", Just "field")] `shouldBe` True

        it "handles multiple attrs with one not matching" $ do
            let mapAttr = Map.fromList [("type", "text"), ("name", "other")]
            isAttrsMatch mapAttr [("type", Just "text"), ("name", Just "field")] `shouldBe` False

    ---------------------------------------------------------------------------
    -- isAttrsMatch'
    ---------------------------------------------------------------------------
    describe "isAttrsMatch'" $ do
        it "returns True for empty desired attrs" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            isAttrsMatch' mapAttr [] `shouldBe` True

        it "returns True when attr exists with any value (Nothing)" $ do
            let mapAttr = Map.fromList [("href", "/page")]
            isAttrsMatch' mapAttr [("href", Nothing)] `shouldBe` True

        it "returns True when attr exists with exact value" $ do
            let mapAttr = Map.fromList [("type", "text")]
            isAttrsMatch' mapAttr [("type", Just "text")] `shouldBe` True

        it "returns False when attr value doesn't match" $ do
            let mapAttr = Map.fromList [("type", "password")]
            isAttrsMatch' mapAttr [("type", Just "text")] `shouldBe` False

        it "returns False when attr is missing" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            isAttrsMatch' mapAttr [("href", Nothing)] `shouldBe` False

    ---------------------------------------------------------------------------
    -- attrsMatch
    ---------------------------------------------------------------------------
    describe "attrsMatch" $ do
        it "returns True for empty input list" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            attrsMatch [] mapAttr `shouldBe` True

        it "matches when key exists and value passes digitEqFree" $ do
            let mapAttr = Map.fromList [("width", "100")]
            attrsMatch [("width", "100")] mapAttr `shouldBe` True

        it "allows any value for href attr" $ do
            let mapAttr = Map.fromList [("href", "/different")]
            attrsMatch [("href", "/page")] mapAttr `shouldBe` True

        it "allows any value for title attr" $ do
            let mapAttr = Map.fromList [("title", "different")]
            attrsMatch [("title", "original")] mapAttr `shouldBe` True

        it "allows any value for alt attr" $ do
            let mapAttr = Map.fromList [("alt", "different")]
            attrsMatch [("alt", "original")] mapAttr `shouldBe` True

        it "returns False when key is missing" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            attrsMatch [("id", "main")] mapAttr `shouldBe` False

        it "uses digitEqFree for non-special attrs" $ do
            let mapAttr = Map.fromList [("width", "100px")]
            -- digitEqFree "200px" "100px" strips digits: "px" == "px" => True
            attrsMatch [("width", "200px")] mapAttr `shouldBe` True

        it "fails when non-digit chars differ for non-special attrs" $ do
            let mapAttr = Map.fromList [("class", "foo")]
            attrsMatch [("class", "bar")] mapAttr `shouldBe` False

    ---------------------------------------------------------------------------
    -- attrsMatch'
    ---------------------------------------------------------------------------
    describe "attrsMatch'" $ do
        it "returns True when both maps are empty" $ do
            attrsMatch' Map.empty Map.empty `shouldBe` True

        it "returns True when first map is subset of second" $ do
            let a = Map.fromList [("class", "btn")]
            let b = Map.fromList [("class", "btn"), ("id", "x")]
            attrsMatch' a b `shouldBe` True

        it "returns False when key is missing from second map" $ do
            let a = Map.fromList [("id", "main")]
            let b = Map.fromList [("class", "btn")]
            attrsMatch' a b `shouldBe` False

    ---------------------------------------------------------------------------
    -- attrsFit
    ---------------------------------------------------------------------------
    describe "attrsFit" $ do
        it "returns True for empty predicate list" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            attrsFit mapAttr [] `shouldBe` True

        it "returns True when predicate passes" $ do
            let mapAttr = Map.fromList [("width", "100")]
            let predicates = [("width", (== "100"))]
            attrsFit mapAttr predicates `shouldBe` True

        it "returns False when predicate fails" $ do
            let mapAttr = Map.fromList [("width", "200")]
            let predicates = [("width", (== "100"))]
            attrsFit mapAttr predicates `shouldBe` False

        it "returns False when attr is missing" $ do
            let mapAttr = Map.fromList [("class", "btn")]
            let predicates = [("width", (== "100"))]
            attrsFit mapAttr predicates `shouldBe` False

        it "handles multiple predicates all passing" $ do
            let mapAttr = Map.fromList [("width", "100"), ("height", "200")]
            let predicates = [("width", (== "100")), ("height", (== "200"))]
            attrsFit mapAttr predicates `shouldBe` True

        it "returns False when one of multiple predicates fails" $ do
            let mapAttr = Map.fromList [("width", "100"), ("height", "200")]
            let predicates = [("width", (== "100")), ("height", (== "300"))]
            attrsFit mapAttr predicates `shouldBe` False

    ---------------------------------------------------------------------------
    -- attrValueExists
    ---------------------------------------------------------------------------
    describe "attrValueExists" $ do
        it "returns False for empty attr list" $ do
            attrValueExists [] ("class", Nothing) `shouldBe` False

        it "returns True when name matches and desired value is Nothing" $ do
            attrValueExists [("class", "btn")] ("class", Nothing) `shouldBe` True

        it "returns True when name and value match" $ do
            attrValueExists [("class", "btn")] ("class", Just "btn") `shouldBe` True

        it "returns False when name matches but value differs" $ do
            attrValueExists [("class", "btn")] ("class", Just "other") `shouldBe` False

        it "returns False when name does not match" $ do
            attrValueExists [("id", "main")] ("class", Nothing) `shouldBe` False

        it "finds attr later in the list" $ do
            attrValueExists [("id", "main"), ("class", "btn")] ("class", Nothing) `shouldBe` True

    ---------------------------------------------------------------------------
    -- attrValuesExist
    ---------------------------------------------------------------------------
    describe "attrValuesExist" $ do
        it "returns True for empty desired list" $ do
            attrValuesExist [("class", "btn")] [] `shouldBe` True

        it "returns True when attr exists with Nothing (any value)" $ do
            attrValuesExist [("class", "btn")] [("class", Nothing)] `shouldBe` True

        it "returns True when attr exists with matching Just value" $ do
            attrValuesExist [("class", "btn")] [("class", Just "btn")] `shouldBe` True

        it "returns False when attr is missing" $ do
            attrValuesExist [("class", "btn")] [("id", Nothing)] `shouldBe` False

        it "returns False when value doesn't match" $ do
            attrValuesExist [("class", "btn")] [("class", Just "other")] `shouldBe` False

        it "handles multiple desired attrs all matching" $ do
            attrValuesExist [("class", "btn"), ("id", "x")] [("class", Nothing), ("id", Nothing)] `shouldBe` True

        it "returns False when one of multiple desired attrs is missing" $ do
            attrValuesExist [("class", "btn")] [("class", Nothing), ("id", Nothing)] `shouldBe` False

    ---------------------------------------------------------------------------
    -- digitEq
    ---------------------------------------------------------------------------
    describe "digitEq" $ do
        it "returns True for equal strings" $ do
            digitEq "hello" "hello" `shouldBe` True

        it "returns True for equal numeric strings" $ do
            digitEq "123" "123" `shouldBe` True

        it "returns True when digits differ but are still digits" $ do
            digitEq "123" "124" `shouldBe` True

        it "returns False for completely different strings" $ do
            digitEq "hello" "world" `shouldBe` False

        it "returns False for different lengths (non-digit)" $ do
            digitEq "hello" "helloo" `shouldBe` False

        it "allows one digit difference in length" $ do
            digitEq "123" "1234" `shouldBe` True

        it "returns True for empty strings" $ do
            digitEq "" "" `shouldBe` True

        it "returns False when one is empty and the other is non-digit" $ do
            digitEq "" "a" `shouldBe` False
            digitEq "a" "" `shouldBe` False

        it "substitutes digits at same positions" $ do
            digitEq "a1b" "a2b" `shouldBe` True

        it "falls through to saveDigitEq when char vs digit mismatch" $ do
            -- "1a" vs "a" differs by 1 in length, first char is digit
            digitEq "1a" "a" `shouldBe` True

    ---------------------------------------------------------------------------
    -- saveDigitEq
    ---------------------------------------------------------------------------
    describe "saveDigitEq" $ do
        it "returns False for two empty strings" $ do
            saveDigitEq "" "" `shouldBe` False

        it "returns False for empty first" $ do
            saveDigitEq "" "abc" `shouldBe` False

        it "returns False for empty second" $ do
            saveDigitEq "abc" "" `shouldBe` False

        it "returns False when lengths differ by more than 1" $ do
            saveDigitEq "abcde" "ab" `shouldBe` False

        it "returns True when lengths differ by 1 and extra char is digit" $ do
            -- "1a" (len 2) vs "a" (len 1), diff=1, '1' is digit
            saveDigitEq "1a" "a" `shouldBe` True

        it "returns False when lengths differ by 1 but extra char is not digit" $ do
            saveDigitEq "ab" "b" `shouldBe` False

    ---------------------------------------------------------------------------
    -- svDigEq
    ---------------------------------------------------------------------------
    describe "svDigEq" $ do
        it "returns False for two empty strings" $ do
            svDigEq "" "" `shouldBe` False

        it "returns False for empty first" $ do
            svDigEq "" "abc" `shouldBe` False

        it "returns False for empty second" $ do
            svDigEq "abc" "" `shouldBe` False

        it "skips one digit in first string and matches rest" $ do
            -- "1a" vs "a": skip '1', 'a' == 'a', digitEq "" "" => True
            svDigEq "1a" "a" `shouldBe` True

        it "skips one digit in second string and matches rest" $ do
            -- "a" vs "1a": skip '1' from second, 'a' == 'a'
            svDigEq "a" "1a" `shouldBe` True

    ---------------------------------------------------------------------------
    -- digitEqFree
    ---------------------------------------------------------------------------
    describe "digitEqFree" $ do
        it "returns True for equal strings" $ do
            digitEqFree "hello" "hello" `shouldBe` True

        it "returns True ignoring leading digits" $ do
            digitEqFree "123hello" "hello" `shouldBe` True
            digitEqFree "hello" "123hello" `shouldBe` True

        it "returns True ignoring digits in middle" $ do
            digitEqFree "hel123lo" "hello" `shouldBe` True

        it "returns False when non-digit chars differ" $ do
            digitEqFree "hello" "world" `shouldBe` False

        it "returns True for empty strings" $ do
            digitEqFree "" "" `shouldBe` True

        it "returns True when one string is all digits" $ do
            digitEqFree "12345" "" `shouldBe` True
            digitEqFree "" "12345" `shouldBe` True

        it "handles mixed digit and char strings" $ do
            digitEqFree "a1b2c3" "abc" `shouldBe` True

        it "returns False when trailing non-digit differs" $ do
            digitEqFree "ax" "ay" `shouldBe` False

    ---------------------------------------------------------------------------
    -- unfit
    ---------------------------------------------------------------------------
    describe "unfit" $ do
        it "returns empty list for empty desired" $ do
            unfit [] Map.empty `shouldBe` []

        it "reports missing attr" $ do
            let result = unfit [("id", "main")] Map.empty
            result `shouldBe` [("id", "no attr")]

        it "skips href/alt/title attrs regardless of value" $ do
            let mp = Map.fromList [("href", "/other"), ("alt", "different"), ("title", "whatever")]
            unfit [("href", "/page"), ("alt", "pic"), ("title", "t")] mp `shouldBe` []

        it "reports failed digitEq test" $ do
            let mp = Map.fromList [("class", "foo")]
            let result = unfit [("class", "bar")] mp
            length result `shouldBe` (1 :: Int)

        it "skips attr when digitEq passes" $ do
            let mp = Map.fromList [("width", "100")]
            unfit [("width", "100")] mp `shouldBe` []

    ---------------------------------------------------------------------------
    -- mkAttrsDesc
    ---------------------------------------------------------------------------
    describe "mkAttrsDesc" $ do
        it "creates predicate list using digitEqFree" $ do
            let descs = mkAttrsDesc [("class", "btn")]
            -- The predicate should use digitEqFree, so "btn" matches "btn"
            case descs of
                [(nm, predFn)] -> do
                    nm `shouldBe` "class"
                    predFn "btn" `shouldBe` True
                    predFn "xyz" `shouldBe` False
                _ -> expectationFailure "Expected single element list"

        it "handles empty list" $ do
            length (mkAttrsDesc []) `shouldBe` (0 :: Int)

        it "created predicates ignore digits" $ do
            let descs = mkAttrsDesc [("width", "px")]
            case descs of
                [(_, predFn)] -> do
                    predFn "100px" `shouldBe` True
                    predFn "200px" `shouldBe` True
                    predFn "em" `shouldBe` False
                _ -> expectationFailure "Expected single element list"

    ---------------------------------------------------------------------------
    -- hrefParser
    ---------------------------------------------------------------------------
    describe "hrefParser" $ do
        it "extracts href from anchor tag" $ do
            let result = parse hrefParser "" "<a href=\"/page\">"
            result `shouldBe` Right "/page"

        it "fails when href is missing" $ do
            let result = parse hrefParser "" "<a class=\"btn\">"
            case result of
                Right _ -> expectationFailure "Should fail without href"
                Left _ -> return ()

        it "extracts full URL" $ do
            let result = parse hrefParser "" "<a href=\"https://example.com/path\">"
            result `shouldBe` Right "https://example.com/path"

        it "extracts href from non-anchor tag" $ do
            let result = parse hrefParser "" "<link href=\"/style.css\">"
            result `shouldBe` Right "/style.css"

    ---------------------------------------------------------------------------
    -- parseAttrSafe
    ---------------------------------------------------------------------------
    describe "parseAttrSafe" $ do
        it "extracts attribute value from tag" $ do
            let result = parse (parseAttrSafe "class") "" "<div class=\"container\">"
            result `shouldBe` Right "container"

        it "fails when attribute is missing" $ do
            let result = parse (parseAttrSafe "id") "" "<div class=\"container\">"
            case result of
                Right _ -> expectationFailure "Should fail - id not present"
                Left _ -> return ()

        it "extracts href from any tag" $ do
            let result = parse (parseAttrSafe "href") "" "<a href=\"/link\" class=\"btn\">"
            result `shouldBe` Right "/link"

    ---------------------------------------------------------------------------
    -- parseOpeningTagF
    ---------------------------------------------------------------------------
    describe "parseOpeningTagF" $ do
        it "matches when predicate passes on attribute" $ do
            let result = parse (parseOpeningTagF "class" (== "btn")) "" "<div class=\"btn\">"
            case result of
                Right (tg, _) -> tg `shouldBe` "div"
                Left err -> expectationFailure $ show err

        it "fails when predicate fails" $ do
            let result = parse (parseOpeningTagF "class" (== "btn")) "" "<div class=\"other\">"
            case result of
                Right _ -> expectationFailure "Should fail - predicate should not match"
                Left _ -> return ()

        it "fails when attribute is missing" $ do
            let result = parse (parseOpeningTagF "id" (== "main")) "" "<div class=\"btn\">"
            case result of
                Right _ -> expectationFailure "Should fail - id not present"
                Left _ -> return ()

    ---------------------------------------------------------------------------
    -- parseOpeningTagWhere
    ---------------------------------------------------------------------------
    describe "parseOpeningTagWhere" $ do
        it "matches when predicate passes" $ do
            let result = parse (parseOpeningTagWhere (Just ["a"]) "href" (== "/page")) "" "<a href=\"/page\">"
            case result of
                Right (tg, _) -> tg `shouldBe` "a"
                Left err -> expectationFailure $ show err

        it "fails when predicate fails" $ do
            let result = parse (parseOpeningTagWhere (Just ["a"]) "href" (== "/other")) "" "<a href=\"/page\">"
            case result of
                Right _ -> expectationFailure "Should fail - predicate should not match"
                Left _ -> return ()

        it "fails when element is not in allowed list" $ do
            let result = parse (parseOpeningTagWhere (Just ["span"]) "href" (== "/page")) "" "<a href=\"/page\">"
            case result of
                Right _ -> expectationFailure "Should fail - a not in [span]"
                Left _ -> return ()

        it "accepts any element when elemList is Nothing" $ do
            let result = parse (parseOpeningTagWhere Nothing "class" (== "btn")) "" "<div class=\"btn\">"
            case result of
                Right (tg, _) -> tg `shouldBe` "div"
                Left err -> expectationFailure $ show err

    ---------------------------------------------------------------------------
    -- attrsParserDesc
    ---------------------------------------------------------------------------
    describe "attrsParserDesc" $ do
        it "returns map when attrs match" $ do
            let result = parse (attrsParserDesc [("class", "btn")]) "" " class=\"btn\""
            case result of
                Right mp -> Map.lookup "class" mp `shouldBe` Just "btn"
                Left err -> expectationFailure $ show err

        it "fails with description when attrs do not match" $ do
            let result = parse (attrsParserDesc [("id", "main")]) "" " class=\"btn\""
            case result of
                Right _ -> expectationFailure "Should have failed"
                Left _ -> return ()

        it "handles empty desired attrs" $ do
            let result = parse (attrsParserDesc []) "" " class=\"btn\""
            case result of
                Right mp -> Map.lookup "class" mp `shouldBe` Just "btn"
                Left err -> expectationFailure $ show err

    ---------------------------------------------------------------------------
    -- parseOpeningTagDesc
    ---------------------------------------------------------------------------
    describe "parseOpeningTagDesc" $ do
        it "parses opening tag with descriptive attr matching" $ do
            let result = parse (parseOpeningTagDesc (Just ["div"]) [("class", "container")]) "" "<div class=\"container\">"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "div"
                    Map.lookup "class" as `shouldBe` Just "container"
                Left err -> expectationFailure $ show err

        it "fails when element not in allowed list" $ do
            let result = parse (parseOpeningTagDesc (Just ["span"]) []) "" "<div>"
            case result of
                Right _ -> expectationFailure "Should fail - div not in [span]"
                Left _ -> return ()

        it "fails when attr does not match" $ do
            let result = parse (parseOpeningTagDesc (Just ["div"]) [("id", "main")]) "" "<div class=\"btn\">"
            case result of
                Right _ -> expectationFailure "Should have failed"
                Left _ -> return ()

    ---------------------------------------------------------------------------
    -- buildElemsOpts
    ---------------------------------------------------------------------------
    describe "buildElemsOpts" $ do
        it "returns parserZero for empty list" $ do
            let result = parse (buildElemsOpts []) "" "div"
            case result of
                Right _ -> expectationFailure "Should fail for empty list"
                Left _ -> return ()

        it "matches first element in list" $ do
            let result = parse (buildElemsOpts ["div", "span"]) "" "div"
            result `shouldBe` Right "div"

        it "matches second element in list" $ do
            let result = parse (buildElemsOpts ["div", "span"]) "" "span"
            result `shouldBe` Right "span"

        it "fails for element not in list" $ do
            let result = parse (buildElemsOpts ["div", "span"]) "" "p"
            case result of
                Right _ -> expectationFailure "Should fail for 'p'"
                Left _ -> return ()

        it "handles single element list" $ do
            let result = parse (buildElemsOpts ["a"]) "" "a"
            result `shouldBe` Right "a"

    ---------------------------------------------------------------------------
    -- mkElemtagParser
    ---------------------------------------------------------------------------
    describe "mkElemtagParser" $ do
        it "parses any alphanumeric tag when Nothing" $ do
            let result = parse (mkElemtagParser Nothing) "" "customtag"
            result `shouldBe` Right "customtag"

        it "parses tag with hyphen when Nothing" $ do
            let result = parse (mkElemtagParser Nothing) "" "custom-tag"
            result `shouldBe` Right "custom-tag"

        it "uses buildElemsOpts when Just" $ do
            let result = parse (mkElemtagParser (Just ["div", "span"])) "" "div"
            result `shouldBe` Right "div"

        it "fails for unknown tag when Just" $ do
            let result = parse (mkElemtagParser (Just ["div"])) "" "span"
            case result of
                Right _ -> expectationFailure "Should fail for span"
                Left _ -> return ()

    ---------------------------------------------------------------------------
    -- href
    ---------------------------------------------------------------------------
    describe "href" $ do
        it "extracts href Link from anchor tag with absolute URL (same-site=False)" $ do
            let lastUrl = Link "https://example.com"
            let result = parse (href False lastUrl) "" "<a href=\"https://other.com/page\">"
            case result of
                Right (Link url) -> url `shouldBe` "https://other.com/page"
                Left err -> expectationFailure $ show err

        it "extracts href Link from anchor tag with relative URL" $ do
            let lastUrl = Link "https://example.com"
            let result = parse (href False lastUrl) "" "<a href=\"/about\">"
            case result of
                Right (Link url) -> url `shouldContain` "about"
                Left err -> expectationFailure $ show err

        it "fails on non-anchor tags" $ do
            let lastUrl = Link "https://example.com"
            let result = parse (href False lastUrl) "" "<div href=\"/page\">"
            case result of
                Right _ -> expectationFailure "Should fail - div is not an anchor tag"
                Left _ -> return ()

        it "fails when href attribute is missing" $ do
            let lastUrl = Link "https://example.com"
            let result = parse (href False lastUrl) "" "<a class=\"btn\">"
            case result of
                Right _ -> expectationFailure "Should fail - no href attr"
                Left _ -> return ()

    ---------------------------------------------------------------------------
    -- href' (undefined)
    ---------------------------------------------------------------------------
    describe "href'" $ do
        it "is undefined" $ do
            let parser = href' Nothing :: Text.Parsec.ParsecT String () Data.Functor.Identity.Identity Link
            shouldBeUndefined parser

    ---------------------------------------------------------------------------
    -- Edge cases: whitespace handling
    ---------------------------------------------------------------------------
    describe "edge cases - whitespace" $ do
        it "handles multiple spaces between attributes" $ do
            let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div   class=\"btn\"   id=\"main\">"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "div"
                    Map.lookup "class" as `shouldBe` Just "btn"
                    Map.lookup "id" as `shouldBe` Just "main"
                Left err -> expectationFailure $ show err

        it "handles newlines between attributes" $ do
            let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div\n  class=\"btn\"\n  id=\"main\">"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "div"
                    Map.lookup "class" as `shouldBe` Just "btn"
                Left err -> expectationFailure $ show err

        it "handles tabs between attributes" $ do
            let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div\tclass=\"btn\">"
            case result of
                Right (tg, as) -> do
                    tg `shouldBe` "div"
                    Map.lookup "class" as `shouldBe` Just "btn"
                Left err -> expectationFailure $ show err

    ---------------------------------------------------------------------------
    -- Edge cases: unicode
    ---------------------------------------------------------------------------
    describe "edge cases - unicode" $ do
        it "parses attribute with unicode value" $ do
            let result = parse attrParser "" " title=\"\26085\26412\35486\""
            case result of
                Right (nm, val) -> do
                    nm `shouldBe` "title"
                    val `shouldBe` "\26085\26412\35486"
                Left err -> expectationFailure $ show err

    ---------------------------------------------------------------------------
    -- Edge cases: case sensitivity
    ---------------------------------------------------------------------------
    describe "edge cases - case sensitivity" $ do
        it "element names are case-sensitive by default" $ do
            let resultLower = parse (parseOpeningTag (Just ["div"]) []) "" "<div>"
            let resultUpper = parse (parseOpeningTag (Just ["div"]) []) "" "<DIV>"
            case resultLower of
                Right (tg, _) -> tg `shouldBe` "div"
                Left err -> expectationFailure $ show err
            case resultUpper of
                Right _ -> expectationFailure "Should fail - DIV not in [div]"
                Left _ -> return ()

    ---------------------------------------------------------------------------
    -- QuickCheck properties
    ---------------------------------------------------------------------------
    describe "QuickCheck properties" $ do
        it "digitEqFree is reflexive" $ property $
            forAll (resize (50 :: Int) arbitrary) $ \s -> digitEqFree s s

        it "digitEq is reflexive" $ property $
            forAll (resize (50 :: Int) arbitrary) $ \s -> digitEq s s

        it "digitEqFree with all-digit string equals empty" $ property $ \(NonNegative n) ->
            digitEqFree (show (n :: Int)) ""

        it "isAttrsMatch with empty desired always True" $ property $
            forAll (resize (20 :: Int) arbitrary) $ \xs ->
                let mapAttr = Map.fromList (xs :: [(String, String)])
                 in isAttrsMatch mapAttr []

        it "isAttrsMatch' with empty desired always True" $ property $
            forAll (resize (20 :: Int) arbitrary) $ \xs ->
                let mapAttr = Map.fromList (xs :: [(String, String)])
                 in isAttrsMatch' mapAttr []

        it "attrsMatch with empty list always True" $ property $
            forAll (resize (20 :: Int) arbitrary) $ \xs ->
                let mapAttr = Map.fromList (xs :: [(String, String)])
                 in attrsMatch [] mapAttr

        it "attrsFit with empty predicates always True" $ property $
            forAll (resize (20 :: Int) arbitrary) $ \xs ->
                let mapAttr = Map.fromList (xs :: [(String, String)])
                 in attrsFit mapAttr []

        it "attrValuesExist with empty desired always True" $ property $
            forAll (resize (20 :: Int) arbitrary) $ \xs ->
                attrValuesExist (xs :: [(String, String)]) []

    ---------------------------------------------------------------------------
    -- buildElemsOpts (additional coverage for href/buildElemsOpts combo)
    ---------------------------------------------------------------------------
    describe "buildElemsOpts - prefix ambiguity" $ do
        it "handles prefix-overlapping tags with backtracking" $ do
            -- "div" and "dialog" share prefix "di"
            let result = parse (buildElemsOpts ["div", "dialog"]) "" "dialog"
            result `shouldBe` Right "dialog"

        it "matches first when prefix-overlapping" $ do
            let result = parse (buildElemsOpts ["div", "dialog"]) "" "div"
            result `shouldBe` Right "div"
