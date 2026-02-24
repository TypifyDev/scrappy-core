{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Elem.SimpleElemParserSpec (spec) where

import Test.Hspec
import Text.Parsec (parse, ParsecT)
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map

import Scrappy.Elem.SimpleElemParser (el, elemParser, selfClosing)
import Scrappy.Elem.Types (Elem'(..), elTag, attrs, innerText')

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
        Right elem -> do
          elTag elem `shouldBe` "div"
          innerText' elem `shouldBe` "content"
        Left err -> expectationFailure $ show err

    it "parses an element with text content" $ do
      let result = parse (el "p" []) "" "<p>Hello World</p>"
      case result of
        Right elem -> do
          elTag elem `shouldBe` "p"
          innerText' elem `shouldBe` "Hello World"
        Left err -> expectationFailure $ show err

    it "parses an element with specific attribute" $ do
      let result = parse (el "a" [("href", "/page")]) "" "<a href=\"/page\">Link</a>"
      case result of
        Right elem -> do
          elTag elem `shouldBe` "a"
          Map.lookup "href" (attrs elem) `shouldBe` Just "/page"
        Left err -> expectationFailure $ show err

    it "fails when tag does not match" $ do
      parseSucceeds (el "div" []) "<span>content</span>" `shouldBe` False

    it "fails when required attribute is missing" $ do
      parseSucceeds (el "a" [("href", "/page")]) "<a>No href</a>" `shouldBe` False

    it "parses empty element" $ do
      let result = parse (el "div" []) "" "<div></div>"
      case result of
        Right elem -> do
          elTag elem `shouldBe` "div"
          innerText' elem `shouldBe` ""
        Left err -> expectationFailure $ show err

  describe "elemParser" $ do
    it "matches any element when elemList is Nothing" $ do
      let result = parse (elemParserS Nothing []) "" "<span>text</span>"
      case result of
        Right elem -> elTag elem `shouldBe` "span"
        Left err -> expectationFailure $ show err

    it "matches only specified elements when elemList is Just [tags]" $ do
      let parser = elemParserS (Just ["a", "div"]) []
      parseSucceeds parser "<a>link</a>" `shouldBe` True
      parseSucceeds parser "<div>div</div>" `shouldBe` True
      parseSucceeds parser "<span>span</span>" `shouldBe` False

    it "captures innerHtmlFull correctly" $ do
      let result = parse (elemParserS (Just ["div"]) []) "" "<div>inner text</div>"
      case result of
        Right elem -> innerText' elem `shouldBe` "inner text"
        Left err -> expectationFailure $ show err

    it "parses element with multiple attributes" $ do
      let result = parse (elemParserS (Just ["input"]) [("type", Just "text")]) "" "<input type=\"text\" value=\"hello\">"
      case result of
        Right elem -> do
          elTag elem `shouldBe` "input"
          Map.lookup "type" (attrs elem) `shouldBe` Just "text"
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
        Right elem -> elTag elem `shouldBe` "br"
        Left err -> expectationFailure $ show err

    it "parses br element with self-closing slash" $ do
      let result = parse (elemParserS (Just ["br"]) []) "" "<br/>"
      case result of
        Right elem -> elTag elem `shouldBe` "br"
        Left err -> expectationFailure $ show err

    it "parses img element with attributes" $ do
      let result = parse (elemParserS (Just ["img"]) [("src", Just "test.jpg")]) "" "<img src=\"test.jpg\" alt=\"test\">"
      case result of
        Right elem -> do
          elTag elem `shouldBe` "img"
          Map.lookup "src" (attrs elem) `shouldBe` Just "test.jpg"
        Left err -> expectationFailure $ show err

  describe "nested elements" $ do
    it "captures nested element as inner text" $ do
      let result = parse (el "div" []) "" "<div><span>nested</span></div>"
      case result of
        Right elem -> do
          elTag elem `shouldBe` "div"
          -- Inner HTML includes the nested span
          innerText' elem `shouldContain` "nested"
        Left err -> expectationFailure $ show err
