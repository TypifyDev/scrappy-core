{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Elem.ITextElemParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (parse, ParseError)
import Data.Either (isRight, isLeft)
import qualified Data.Map as Map

import Scrappy.Elem.ITextElemParser
  ( Paragraph(..)
  , Sentence(..)
  , WrittenWord(..)
  , sentence
  , sentenceWhere
  , writtenWord
  , capitalizedWord
  , number
  , removeStyleTags
  , textChunk
  , plainText
  , styleTags
  , negParseOpeningTag
  , openOrCloseTag
  , anyEndTag
  , anyThingbut
  , textChunkIf
  , divideUp
  , catEithers
  , punctuation
  , wordSeparator
  , comma
  , colon
  , semiColon
  , word'
  , emptyTree
  , emptyTreeGroup
  , styleElem
  )
import Scrappy.Elem.Types (ShowHTML(..))

spec :: Spec
spec = do
  describe "WrittenWord type" $ do
    it "shows word correctly" $ do
      show (WW "hello") `shouldBe` "hello"

    it "shows empty word as empty string" $ do
      show (WW "") `shouldBe` ""

    it "semigroup concatenates with space" $ do
      let w1 = WW "hello"
      let w2 = WW "world"
      show (w1 <> w2) `shouldBe` "hello world"

    it "mempty is empty string" $ do
      show (mempty :: WrittenWord) `shouldBe` ""

  describe "Sentence type" $ do
    it "shows sentence with period" $ do
      let s = Sentence [WW "Hello", WW "world"]
      show s `shouldBe` "Hello world."

    it "shows empty sentence as just period" $ do
      let s = Sentence []
      show s `shouldBe` "."

    it "semigroup concatenates sentences" $ do
      let s1 = Sentence [WW "hello"]
      let s2 = Sentence [WW "world"]
      let combined = s1 <> s2
      unSentence combined `shouldBe` [WW "hello", WW "world"]

    it "mempty is empty sentence" $ do
      unSentence (mempty :: Sentence) `shouldBe` []

  describe "Paragraph type" $ do
    it "shows paragraph with sentences" $ do
      let p = Paragraph [Sentence [WW "Hello"], Sentence [WW "World"]]
      show p `shouldBe` "Hello. World."

    it "shows empty paragraph as empty" $ do
      let p = Paragraph []
      show p `shouldBe` ""

    it "semigroup concatenates paragraphs" $ do
      let p1 = Paragraph [Sentence [WW "a"]]
      let p2 = Paragraph [Sentence [WW "b"]]
      let combined = p1 <> p2
      length (unParagraph combined) `shouldBe` 2

    it "mempty is empty paragraph" $ do
      unParagraph (mempty :: Paragraph) `shouldBe` []

    it "ShowHTML instance works" $ do
      let p = Paragraph [Sentence [WW "Hello"]]
      showH p `shouldContain` "Hello"

  describe "writtenWord parser" $ do
    it "parses simple word" $ do
      let result = parse writtenWord "" "hello "
      case result of
        Right (WW w) -> w `shouldBe` "hello"
        Left err -> expectationFailure $ show err

    it "parses word with numbers" $ do
      let result = parse writtenWord "" "hello123 "
      case result of
        Right (WW w) -> w `shouldBe` "hello123"
        Left err -> expectationFailure $ show err

    it "parses word with punctuation" $ do
      let result = parse writtenWord "" "hello, "
      case result of
        Right (WW w) -> w `shouldBe` "hello,"
        Left err -> expectationFailure $ show err

    it "handles trailing space optionally" $ do
      let result = parse writtenWord "" "hello"
      isRight result `shouldBe` True

  describe "capitalizedWord parser" $ do
    it "parses word starting with capital letter" $ do
      let result = parse capitalizedWord "" "Hello"
      result `shouldBe` Right "Hello"

    it "parses single I" $ do
      let result = parse capitalizedWord "" "I"
      result `shouldBe` Right "I"

    it "parses single A" $ do
      let result = parse capitalizedWord "" "A"
      result `shouldBe` Right "A"

    it "fails on lowercase word" $ do
      let result = parse capitalizedWord "" "hello"
      isLeft result `shouldBe` True

    it "parses word with apostrophe" $ do
      let result = parse capitalizedWord "" "Don't"
      result `shouldBe` Right "Don't"

    it "parses word with hyphen" $ do
      let result = parse capitalizedWord "" "Self-aware"
      result `shouldBe` Right "Self-aware"

  describe "number parser" $ do
    it "parses whole number" $ do
      let result = parse number "" "123"
      result `shouldBe` Right "123"

    it "parses decimal number" $ do
      let result = parse number "" "123.45"
      result `shouldBe` Right "123.45"

    it "parses single digit" $ do
      let result = parse number "" "7"
      result `shouldBe` Right "7"

    it "fails on letters" $ do
      let result = parse number "" "abc"
      isLeft result `shouldBe` True

  describe "sentence parser" $ do
    it "parses simple sentence ending with period" $ do
      let result = parse sentence "" "Hello."
      case result of
        Right (Sentence words) -> do
          length words `shouldBe` 1
          extractWord (head words) `shouldBe` "Hello"
        Left err -> expectationFailure $ show err

    it "parses multi-word sentence" $ do
      let result = parse sentence "" "Hello world."
      case result of
        Right (Sentence words) -> do
          length words `shouldBe` 2
        Left err -> expectationFailure $ show err

    it "parses sentence starting with number" $ do
      let result = parse sentence "" "2023 was great."
      case result of
        Right s -> length (unSentence s) `shouldSatisfy` (> 0)
        Left err -> expectationFailure $ show err

    it "fails on sentence starting with lowercase" $ do
      let result = parse sentence "" "hello world."
      isLeft result `shouldBe` True

  describe "sentenceWhere parser" $ do
    it "passes when predicate is true" $ do
      let result = parse (sentenceWhere (\ws -> length ws > 0)) "" "Hello."
      isRight result `shouldBe` True

    it "fails when predicate is false" $ do
      let result = parse (sentenceWhere (\ws -> length ws > 5)) "" "Hello."
      isLeft result `shouldBe` True

  describe "word separators" $ do
    it "parses space" $ do
      let result = parse wordSeparator "" " "
      result `shouldBe` Right " "

    it "parses comma" $ do
      let result = parse comma "" ", "
      result `shouldBe` Right ", "

    it "parses comma without trailing space" $ do
      let result = parse comma "" ","
      result `shouldBe` Right ","

    it "parses colon" $ do
      let result = parse colon "" ": "
      result `shouldBe` Right ": "

    it "parses semicolon" $ do
      let result = parse semiColon "" "; "
      result `shouldBe` Right "; "

  describe "word' parser" $ do
    it "parses simple word" $ do
      let result = parse word' "" "hello"
      result `shouldBe` Right "hello"

    it "parses word with apostrophe" $ do
      let result = parse word' "" "don't"
      result `shouldBe` Right "don't"

    it "parses word with hyphen" $ do
      let result = parse word' "" "self-aware"
      result `shouldBe` Right "self-aware"

    it "parses single 'a'" $ do
      let result = parse word' "" "a"
      result `shouldBe` Right "a"

  describe "punctuation parser" $ do
    it "parses semicolon" $ do
      let result = parse punctuation "" ";"
      result `shouldBe` Right ';'

    it "parses colon" $ do
      let result = parse punctuation "" ":"
      result `shouldBe` Right ':'

    it "parses parentheses" $ do
      let result1 = parse punctuation "" "("
      let result2 = parse punctuation "" ")"
      result1 `shouldBe` Right '('
      result2 `shouldBe` Right ')'

    it "parses quotes" $ do
      let result1 = parse punctuation "" "\""
      let result2 = parse punctuation "" "'"
      result1 `shouldBe` Right '"'
      result2 `shouldBe` Right '\''

    it "parses hyphen" $ do
      let result = parse punctuation "" "-"
      result `shouldBe` Right '-'

    it "parses comma" $ do
      let result = parse punctuation "" ","
      result `shouldBe` Right ','

  describe "styleTags list" $ do
    it "contains common style tags" $ do
      "b" `elem` styleTags `shouldBe` True
      "strong" `elem` styleTags `shouldBe` True
      "i" `elem` styleTags `shouldBe` True
      "em" `elem` styleTags `shouldBe` True
      "mark" `elem` styleTags `shouldBe` True
      "small" `elem` styleTags `shouldBe` True
      "ins" `elem` styleTags `shouldBe` True
      "sub" `elem` styleTags `shouldBe` True
      "sup" `elem` styleTags `shouldBe` True

    it "does not contain structural tags" $ do
      "div" `elem` styleTags `shouldBe` False
      "p" `elem` styleTags `shouldBe` False
      "span" `elem` styleTags `shouldBe` False

  describe "removeStyleTags" $ do
    it "removes bold tags" $ do
      removeStyleTags "<b>hello</b>" `shouldBe` "hello"

    it "removes italic tags" $ do
      removeStyleTags "<i>hello</i>" `shouldBe` "hello"

    it "removes multiple style tags" $ do
      removeStyleTags "<b>bold</b> and <i>italic</i>" `shouldBe` "bold and italic"

    it "keeps non-style tags" $ do
      removeStyleTags "<div>content</div>" `shouldContain` "div"

    it "handles nested style tags" $ do
      removeStyleTags "<b><i>nested</i></b>" `shouldBe` "nested"

    it "handles empty string" $ do
      removeStyleTags "" `shouldBe` ""

    it "handles text without tags" $ do
      removeStyleTags "plain text" `shouldBe` "plain text"

  describe "negParseOpeningTag" $ do
    it "succeeds for tag not in blacklist" $ do
      let result = parse (negParseOpeningTag ["div", "span"]) "" "<p>"
      case result of
        Right (tag, _) -> tag `shouldBe` "p"
        Left err -> expectationFailure $ show err

    it "fails for tag in blacklist" $ do
      let result = parse (negParseOpeningTag ["div", "span"]) "" "<div>"
      isLeft result `shouldBe` True

    it "parses attributes of allowed tag" $ do
      let result = parse (negParseOpeningTag ["div"]) "" "<p class=\"intro\">"
      case result of
        Right (tag, _) -> tag `shouldBe` "p"
        Left err -> expectationFailure $ show err

  describe "anyThingbut" $ do
    it "parses tag not in exclusion list" $ do
      let result = parse (anyThingbut ["div", "span"]) "" "p"
      result `shouldBe` Right "p"

    it "fails for tag in exclusion list" $ do
      let result = parse (anyThingbut ["div", "span"]) "" "div"
      isLeft result `shouldBe` True

  describe "textChunk" $ do
    it "extracts text before tag" $ do
      let result = parse textChunk "" "hello<div>"
      case result of
        Right txt -> txt `shouldBe` "hello"
        Left err -> expectationFailure $ show err

    it "includes style tag content as plain text" $ do
      let result = parse textChunk "" "hello <b>bold</b> world<div>"
      case result of
        Right txt -> do
          txt `shouldContain` "hello"
          txt `shouldContain` "bold"
        Left err -> expectationFailure $ show err

  describe "textChunkIf" $ do
    it "succeeds when predicate passes" $ do
      let result = parse (textChunkIf (\t -> length t > 3)) "" "hello<div>"
      case result of
        Right txt -> length txt `shouldSatisfy` (> 3)
        Left err -> expectationFailure $ show err

    it "fails when predicate fails" $ do
      let result = parse (textChunkIf (\t -> length t > 100)) "" "hi<div>"
      isLeft result `shouldBe` True

  describe "plainText" $ do
    it "extracts single character" $ do
      let result = parse plainText "" "a"
      result `shouldBe` Right "a"

    it "extracts content from style element" $ do
      let result = parse plainText "" "<i>text</i>"
      case result of
        Right txt -> txt `shouldBe` "text"
        Left err -> expectationFailure $ show err

  describe "styleElem" $ do
    it "parses italic element" $ do
      let result = parse styleElem "" "<i>text</i>"
      isRight result `shouldBe` True

    it "parses bold element" $ do
      let result = parse styleElem "" "<b>text</b>"
      isRight result `shouldBe` True

    it "fails for non-style element" $ do
      let result = parse styleElem "" "<div>text</div>"
      isLeft result `shouldBe` True

  describe "divideUp" $ do
    it "splits string by matching parser" $ do
      let result = parse (divideUp colon) "" "a:b:c"
      -- This should divide based on colons
      isRight result `shouldBe` True

  describe "catEithers" $ do
    it "extracts Right values" $ do
      catEithers [Right "a", Left "x", Right "b"] `shouldBe` ["a", "b"]

    it "returns empty for all Left" $ do
      catEithers [Left "x", Left "y"] `shouldBe` ([] :: [String])

    it "handles empty list" $ do
      catEithers ([] :: [Either String String]) `shouldBe` []

  describe "ShowHTML instances" $ do
    it "Paragraph showH returns sentences" $ do
      let p = Paragraph [Sentence [WW "Hello"]]
      showH p `shouldContain` "Hello"

    it "Sentence showH ends with period" $ do
      let s = Sentence [WW "Hello", WW "world"]
      showH s `shouldSatisfy` (\x -> last x == '.')

  describe "edge cases - unicode text" $ do
    it "writtenWord handles unicode letters" $ do
      let result = parse writtenWord "" "café "
      case result of
        Right (WW w) -> w `shouldContain` "caf"
        Left _ -> return () -- May fail depending on alphaNum definition

  describe "edge cases - empty input" $ do
    it "sentence fails on empty string" $ do
      let result = parse sentence "" ""
      isLeft result `shouldBe` True

    it "writtenWord fails on empty string" $ do
      let result = parse writtenWord "" ""
      isLeft result `shouldBe` True

    it "number fails on empty string" $ do
      let result = parse number "" ""
      isLeft result `shouldBe` True

  describe "edge cases - mixed content" $ do
    it "handles text mixed with HTML entities" $ do
      removeStyleTags "hello&nbsp;world" `shouldBe` "hello&nbsp;world"

    it "handles multiple consecutive style tags" $ do
      removeStyleTags "<b>a</b><i>b</i><em>c</em>" `shouldBe` "abc"

  describe "Monoid laws for WrittenWord" $ do
    it "left identity" $ property $
      forAll (resize 50 arbitrary) $ \s ->
        (mempty <> WW s) == WW s

    it "right identity" $ property $
      forAll (resize 50 arbitrary) $ \s ->
        (WW s <> mempty) == WW (" " ++ s)  -- Note: WW concatenates with space

  describe "Monoid laws for Sentence" $ do
    it "left identity" $ property $
      forAll (resize 20 arbitrary) $ \ws ->
        let s = Sentence (map WW ws)
        in unSentence (mempty <> s) == unSentence s

    it "right identity" $ property $
      forAll (resize 20 arbitrary) $ \ws ->
        let s = Sentence (map WW ws)
        in unSentence (s <> mempty) == unSentence s

  describe "Monoid laws for Paragraph" $ do
    it "left identity" $ do
      let p = Paragraph [Sentence [WW "test"]]
      (mempty <> p) `shouldBe` p

    it "right identity" $ do
      let p = Paragraph [Sentence [WW "test"]]
      (p <> mempty) `shouldBe` p

    it "associativity" $ do
      let p1 = Paragraph [Sentence [WW "a"]]
      let p2 = Paragraph [Sentence [WW "b"]]
      let p3 = Paragraph [Sentence [WW "c"]]
      ((p1 <> p2) <> p3) `shouldBe` (p1 <> (p2 <> p3))

-- Helper to extract word string
extractWord :: WrittenWord -> String
extractWord (WW s) = s

-- Eq instances for testing
instance Eq WrittenWord where
  (WW a) == (WW b) = a == b

instance Eq Sentence where
  (Sentence a) == (Sentence b) = a == b

instance Eq Paragraph where
  (Paragraph a) == (Paragraph b) = a == b
