{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Elem.ElemHeadParseSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (parse, runParser)
import qualified Data.Map as Map

import Scrappy.Elem.ElemHeadParse
  ( parseOpeningTag
  , attrParser
  , attrsParser
  , attrName
  , attrValue
  , isAttrsMatch
  , isAttrsMatch'
  , attrsMatch
  , attrsMatch'
  , attrsFit
  , digitEq
  , digitEqFree
  , hrefParser
  , buildElemsOpts
  , mkElemtagParser
  , parseAttrSafe
  , parseOpeningTagF
  , parseOpeningTagWhere
  , parseOpeningTagDesc
  , attrsParserDesc
  , mkAttrsDesc
  , attrValuesExist
  , attrValueExists
  , saveDigitEq
  , svDigEq
  , unfit
  )
import Scrappy.Elem.Types (AttrsError(..))

spec :: Spec
spec = do
  describe "parseOpeningTag" $ do
    it "parses a simple div tag" $ do
      let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div>"
      case result of
        Right (elem, attrs) -> do
          elem `shouldBe` "div"
          attrs `shouldBe` Map.empty
        Left err -> expectationFailure $ show err

    it "parses a tag with single attribute" $ do
      let result = parse (parseOpeningTag (Just ["a"]) []) "" "<a href=\"/page\">"
      case result of
        Right (elem, attrs) -> do
          elem `shouldBe` "a"
          Map.lookup "href" attrs `shouldBe` Just "/page"
        Left err -> expectationFailure $ show err

    it "parses a tag with multiple attributes" $ do
      let result = parse (parseOpeningTag (Just ["input"]) []) "" "<input type=\"text\" name=\"field\" value=\"hello\">"
      case result of
        Right (elem, attrs) -> do
          elem `shouldBe` "input"
          Map.lookup "type" attrs `shouldBe` Just "text"
          Map.lookup "name" attrs `shouldBe` Just "field"
          Map.lookup "value" attrs `shouldBe` Just "hello"
        Left err -> expectationFailure $ show err

    it "parses any element when elemOpts is Nothing" $ do
      let result = parse (parseOpeningTag Nothing []) "" "<span>"
      case result of
        Right (elem, _) -> elem `shouldBe` "span"
        Left err -> expectationFailure $ show err

    it "fails when element is not in allowed list" $ do
      let result = parse (parseOpeningTag (Just ["div", "span"]) []) "" "<p>"
      case result of
        Right _ -> expectationFailure "Should have failed"
        Left _ -> return ()

    it "parses tag with required attribute subset" $ do
      let result = parse (parseOpeningTag (Just ["a"]) [("href", Nothing)]) "" "<a href=\"/link\" class=\"btn\">"
      case result of
        Right (elem, attrs) -> do
          elem `shouldBe` "a"
          Map.lookup "href" attrs `shouldBe` Just "/link"
        Left err -> expectationFailure $ show err

    it "fails when required attribute is missing" $ do
      let result = parse (parseOpeningTag (Just ["a"]) [("href", Nothing)]) "" "<a class=\"btn\">"
      case result of
        Right _ -> expectationFailure "Should have failed - href required"
        Left _ -> return ()

    it "parses tag with specific attribute value" $ do
      let result = parse (parseOpeningTag (Just ["input"]) [("type", Just "text")]) "" "<input type=\"text\">"
      case result of
        Right (elem, _) -> elem `shouldBe` "input"
        Left err -> expectationFailure $ show err

    it "fails when attribute value doesn't match" $ do
      let result = parse (parseOpeningTag (Just ["input"]) [("type", Just "text")]) "" "<input type=\"password\">"
      case result of
        Right _ -> expectationFailure "Should have failed - type should be 'text'"
        Left _ -> return ()

  describe "attrParser" $ do
    it "parses attribute with double-quoted value" $ do
      let result = parse attrParser "" " href=\"/page\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "href"
          value `shouldBe` "/page"
        Left err -> expectationFailure $ show err

    it "parses attribute with single-quoted value" $ do
      let result = parse attrParser "" " href='/page'"
      case result of
        Right (name, value) -> do
          name `shouldBe` "href"
          value `shouldBe` "/page"
        Left err -> expectationFailure $ show err

    it "parses boolean attribute (no value)" $ do
      let result = parse attrParser "" " disabled"
      case result of
        Right (name, value) -> do
          name `shouldBe` "disabled"
          value `shouldBe` ""
        Left err -> expectationFailure $ show err

    it "parses attribute with hyphen in name" $ do
      let result = parse attrParser "" " data-value=\"test\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "data-value"
          value `shouldBe` "test"
        Left err -> expectationFailure $ show err

    it "parses attribute with underscore in name" $ do
      let result = parse attrParser "" " my_attr=\"value\""
      case result of
        Right (name, _) -> name `shouldBe` "my_attr"
        Left err -> expectationFailure $ show err

    it "parses attribute with numbers in name" $ do
      let result = parse attrParser "" " data1=\"value\""
      case result of
        Right (name, _) -> name `shouldBe` "data1"
        Left err -> expectationFailure $ show err

    it "handles leading whitespace including tabs" $ do
      let result = parse attrParser "" "\t\nclass=\"btn\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "class"
          value `shouldBe` "btn"
        Left err -> expectationFailure $ show err

    it "parses empty attribute value" $ do
      let result = parse attrParser "" " title=\"\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "title"
          value `shouldBe` ""
        Left err -> expectationFailure $ show err

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

  describe "attrsMatch" $ do
    it "returns True for empty input list" $ do
      let mapAttr = Map.fromList [("class", "btn")]
      attrsMatch [] mapAttr `shouldBe` True

    it "matches when key exists and value passes digitEqFree" $ do
      let mapAttr = Map.fromList [("width", "100")]
      attrsMatch [("width", "100")] mapAttr `shouldBe` True

    it "allows any value for href, title, alt attrs" $ do
      let mapAttr = Map.fromList [("href", "/different")]
      attrsMatch [("href", "/page")] mapAttr `shouldBe` True

    it "returns False when key is missing" $ do
      let mapAttr = Map.fromList [("class", "btn")]
      attrsMatch [("id", "main")] mapAttr `shouldBe` False

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

    it "returns False when one is empty" $ do
      digitEq "" "a" `shouldBe` False
      digitEq "a" "" `shouldBe` False

  describe "digitEqFree" $ do
    it "returns True for equal strings" $ do
      digitEqFree "hello" "hello" `shouldBe` True

    it "returns True ignoring leading/trailing digits" $ do
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

  describe "parseAttrSafe" $ do
    it "extracts attribute value from tag" $ do
      let result = parse (parseAttrSafe "class") "" "<div class=\"container\">"
      result `shouldBe` Right "container"

    it "fails when attribute is missing" $ do
      let result = parse (parseAttrSafe "id") "" "<div class=\"container\">"
      case result of
        Right _ -> expectationFailure "Should fail - id not present"
        Left _ -> return ()

  describe "attrValuesExist" $ do
    it "returns True for empty input list" $ do
      attrValuesExist [("class", "btn")] [] `shouldBe` True

    it "returns True when attr exists with Nothing (any value)" $ do
      attrValuesExist [("class", "btn")] [("class", Nothing)] `shouldBe` True

    it "returns True when attr exists with matching Just value" $ do
      attrValuesExist [("class", "btn")] [("class", Just "btn")] `shouldBe` True

    it "returns False when attr is missing" $ do
      attrValuesExist [("class", "btn")] [("id", Nothing)] `shouldBe` False

    it "returns False when value doesn't match" $ do
      attrValuesExist [("class", "btn")] [("class", Just "other")] `shouldBe` False

  describe "edge cases - unicode" $ do
    it "parses attribute with unicode value" $ do
      let result = parse attrParser "" " title=\"日本語\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "title"
          value `shouldBe` "日本語"
        Left err -> expectationFailure $ show err

    it "parses attribute with emoji" $ do
      let result = parse attrParser "" " data-emoji=\"🎉\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "data-emoji"
          value `shouldBe` "🎉"
        Left err -> expectationFailure $ show err

  describe "edge cases - whitespace" $ do
    it "handles multiple spaces between attributes" $ do
      let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div   class=\"btn\"   id=\"main\">"
      case result of
        Right (elem, attrs) -> do
          elem `shouldBe` "div"
          Map.lookup "class" attrs `shouldBe` Just "btn"
          Map.lookup "id" attrs `shouldBe` Just "main"
        Left err -> expectationFailure $ show err

    it "handles newlines between attributes" $ do
      let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div\n  class=\"btn\"\n  id=\"main\">"
      case result of
        Right (elem, attrs) -> do
          elem `shouldBe` "div"
          Map.lookup "class" attrs `shouldBe` Just "btn"
        Left err -> expectationFailure $ show err

    it "handles tabs between attributes" $ do
      let result = parse (parseOpeningTag (Just ["div"]) []) "" "<div\tclass=\"btn\">"
      case result of
        Right (elem, attrs) -> do
          elem `shouldBe` "div"
          Map.lookup "class" attrs `shouldBe` Just "btn"
        Left err -> expectationFailure $ show err

  describe "edge cases - empty and special values" $ do
    it "parses attribute with empty double-quoted value" $ do
      let result = parse attrParser "" " value=\"\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "value"
          value `shouldBe` ""
        Left err -> expectationFailure $ show err

    it "parses attribute with empty single-quoted value" $ do
      let result = parse attrParser "" " value=''"
      case result of
        Right (name, value) -> do
          name `shouldBe` "value"
          value `shouldBe` ""
        Left err -> expectationFailure $ show err

    it "parses attribute with spaces in value" $ do
      let result = parse attrParser "" " title=\"hello world foo\""
      case result of
        Right (name, value) -> do
          name `shouldBe` "title"
          value `shouldBe` "hello world foo"
        Left err -> expectationFailure $ show err

  describe "edge cases - case sensitivity" $ do
    it "element names are case-sensitive by default" $ do
      let resultLower = parse (parseOpeningTag (Just ["div"]) []) "" "<div>"
      let resultUpper = parse (parseOpeningTag (Just ["div"]) []) "" "<DIV>"
      case resultLower of
        Right (elem, _) -> elem `shouldBe` "div"
        Left err -> expectationFailure $ show err
      -- Upper case should fail since "DIV" is not in ["div"]
      case resultUpper of
        Right _ -> expectationFailure "Should fail - DIV not in [div]"
        Left _ -> return ()

  describe "parseOpeningTagWhere" $ do
    it "matches when predicate passes" $ do
      let result = parse (parseOpeningTagWhere (Just ["a"]) "href" (== "/page")) "" "<a href=\"/page\">"
      case result of
        Right (elem, _) -> elem `shouldBe` "a"
        Left err -> expectationFailure $ show err

    it "fails when predicate fails" $ do
      let result = parse (parseOpeningTagWhere (Just ["a"]) "href" (== "/other")) "" "<a href=\"/page\">"
      case result of
        Right _ -> expectationFailure "Should fail - predicate should not match"
        Left _ -> return ()

  describe "parseOpeningTagF" $ do
    it "matches when predicate passes on attribute" $ do
      let result = parse (parseOpeningTagF "class" (elem "btn" . words)) "" "<div class=\"btn primary\">"
      case result of
        Right (elem, _) -> elem `shouldBe` "div"
        Left err -> expectationFailure $ show err

  describe "QuickCheck properties" $ do
    it "digitEqFree is reflexive" $ property $
      forAll (resize 50 arbitrary) $ \s -> digitEqFree s s

    it "digitEq is reflexive" $ property $
      forAll (resize 50 arbitrary) $ \s -> digitEq s s

    it "digitEqFree with all-digit string equals empty" $ property $ \(NonNegative n) ->
      digitEqFree (show (n :: Int)) ""

    it "isAttrsMatch with empty desired always True" $ property $
      forAll (resize 20 arbitrary) $ \xs ->
        let mapAttr = Map.fromList (xs :: [(String, String)])
        in isAttrsMatch mapAttr []
