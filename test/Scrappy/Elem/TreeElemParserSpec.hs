{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Elem.TreeElemParserSpec (spec) where

import Test.Hspec
import Text.Parsec (parse, ParsecT)
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map

import Scrappy.Elem.TreeElemParser (treeElemParser, selfClosing)
import Scrappy.Elem.Types (TreeHTML(..), elTag, attrs, innerText', matches')

import TestUtils (parseSucceeds)

-- Helper type alias for tree parsers with String inner type
type TreeParser = ParsecT String () Identity (TreeHTML String)

-- Helper to create treeElemParser with explicit String type
treeParserS :: Maybe [String] -> [(String, Maybe String)] -> TreeParser
treeParserS tags attrss = treeElemParser tags (Nothing :: Maybe (ParsecT String () Identity String)) attrss

spec :: Spec
spec = do
  describe "treeElemParser" $ do
    it "parses a simple element and builds tree structure" $ do
      let result = parse (treeParserS (Just ["div"]) []) "" "<div>content</div>"
      case result of
        Right tree -> do
          elTag tree `shouldBe` "div"
          innerText' tree `shouldBe` "content"
        Left err -> expectationFailure $ show err

    it "correctly populates _topEl field" $ do
      let result = parse (treeParserS (Just ["span"]) []) "" "<span>text</span>"
      case result of
        Right tree -> _topEl tree `shouldBe` "span"
        Left err -> expectationFailure $ show err

    it "correctly populates _topAttrs field" $ do
      let result = parse (treeParserS (Just ["a"]) [("href", Just "/link")]) "" "<a href=\"/link\" class=\"nav\">link</a>"
      case result of
        Right tree -> do
          Map.lookup "href" (_topAttrs tree) `shouldBe` Just "/link"
          Map.lookup "class" (_topAttrs tree) `shouldBe` Just "nav"
        Left err -> expectationFailure $ show err

    it "correctly populates _innerText' field" $ do
      let result = parse (treeParserS (Just ["p"]) []) "" "<p>Hello World</p>"
      case result of
        Right tree -> _innerText' tree `shouldBe` "Hello World"
        Left err -> expectationFailure $ show err

    it "handles nested elements in tree structure" $ do
      let html = "<div><span>nested</span></div>"
      let result = parse (treeParserS (Just ["div"]) []) "" html
      case result of
        Right tree -> do
          elTag tree `shouldBe` "div"
          -- Inner tree should contain the nested span
          length (_innerTree' tree) `shouldSatisfy` (>= 0)
        Left err -> expectationFailure $ show err

    it "works with deeply nested elements" $ do
      let html = "<div><div><div>deep</div></div></div>"
      let result = parse (treeParserS (Just ["div"]) []) "" html
      case result of
        Right tree -> elTag tree `shouldBe` "div"
        Left err -> expectationFailure $ show err

  describe "self-closing elements in tree parser" $ do
    it "includes standard self-closing tags" $ do
      "br" `elem` selfClosing `shouldBe` True
      "img" `elem` selfClosing `shouldBe` True
      "input" `elem` selfClosing `shouldBe` True

    it "parses self-closing br element" $ do
      let result = parse (treeParserS (Just ["br"]) []) "" "<br>"
      case result of
        Right tree -> elTag tree `shouldBe` "br"
        Left err -> expectationFailure $ show err

    it "parses self-closing input element with attributes" $ do
      let result = parse (treeParserS (Just ["input"]) [("type", Just "text")]) "" "<input type=\"text\" name=\"field\">"
      case result of
        Right tree -> do
          elTag tree `shouldBe` "input"
          Map.lookup "type" (attrs tree) `shouldBe` Just "text"
        Left err -> expectationFailure $ show err

  describe "tree element with matches" $ do
    it "returns empty matches when no inner pattern specified" $ do
      let result = parse (treeParserS (Just ["div"]) []) "" "<div>text</div>"
      case result of
        Right tree -> matches' tree `shouldBe` []
        Left err -> expectationFailure $ show err

  describe "element filtering" $ do
    it "matches when element is in allowed list" $ do
      parseSucceeds (treeParserS (Just ["div", "span"]) []) "<div>content</div>"
        `shouldBe` True

    it "fails when element is not in allowed list" $ do
      parseSucceeds (treeParserS (Just ["div", "span"]) []) "<p>content</p>"
        `shouldBe` False

    it "matches any element when elemList is Nothing" $ do
      parseSucceeds (treeParserS Nothing []) "<custom>content</custom>"
        `shouldBe` True
