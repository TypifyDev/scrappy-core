{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Scrappy.Elem.TreeElemParserSpec (spec) where

import Test.Hspec
import Text.Parsec (parse, ParsecT)
import Data.Functor.Identity (Identity)
import qualified Data.Map as Map

import Scrappy.Elem.TreeElemParser (treeElemParser)
import qualified Scrappy.Elem.Types as ST

import TestUtils (parseSucceeds)

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
      let html = unlines
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
      let html = unlines
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
      let html = unlines
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
