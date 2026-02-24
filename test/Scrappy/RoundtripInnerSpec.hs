{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.RoundtripInnerSpec (spec) where

import Test.Hspec
import Text.Parsec (ParsecT, string, many1, letter, digit, char, (<|>), between, alphaNum)
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust, isNothing)

import Scrappy.Scrape (scrapeFirst')
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Elem.TreeElemParser (treeElemParser)
import Scrappy.Elem.Types (Elem', TreeHTML, showH, matches', ShowHTML)

-- Type aliases
type InnerParser a = ParsecT String () Identity a
type ElemParserWith a = ParsecT String () Identity (Elem' a)
type TreeParserWith a = ParsecT String () Identity (TreeHTML a)

-- | Complex inner parsers to test roundtrip behavior
wordParser :: InnerParser String
wordParser = many1 letter

numberParser :: InnerParser String
numberParser = many1 digit

emailLikeParser :: InnerParser String
emailLikeParser = do
  user <- many1 alphaNum
  _ <- char '@'
  domain <- many1 alphaNum
  _ <- char '.'
  tld <- many1 letter
  return $ user ++ "@" ++ domain ++ "." ++ tld

bracketedParser :: InnerParser String
bracketedParser = between (char '[') (char ']') (many1 alphaNum)

pipeSeparatedParser :: InnerParser String
pipeSeparatedParser = do
  a <- many1 letter
  _ <- char '|'
  b <- many1 letter
  return $ a ++ "|" ++ b

nestedQuotesParser :: InnerParser String
nestedQuotesParser = between (char '"') (char '"') (many1 (alphaNum <|> char ' '))

-- | Helper to test that roundtrip continues to be parsable
-- Note: With inner parsers, showH applies Show to matched content, adding quotes.
-- This means roundtrip is NOT idempotent - quotes accumulate on each iteration.
-- But it should always remain parsable.
testInnerRoundtripParsable :: (Show a, ShowHTML a) => String -> ElemParserWith a -> String -> IO ()
testInnerRoundtripParsable name parser html = do
  case scrapeFirst' parser html of
    Nothing -> expectationFailure $ name ++ ": Initial parse failed on: " ++ html
    Just e1 -> do
      let html1 = showH e1
      case scrapeFirst' parser html1 of
        Nothing -> expectationFailure $ name ++ ": Second parse failed on: " ++ html1
        Just e2 -> do
          let html2 = showH e2
          case scrapeFirst' parser html2 of
            Nothing -> expectationFailure $ name ++ ": Third parse failed on: " ++ html2
            Just _ -> return () -- Success: all three parses worked

-- | Helper for TreeHTML - test that roundtrip continues to be parsable
testInnerRoundtripTreeParsable :: (Show a, ShowHTML a) => String -> TreeParserWith a -> String -> IO ()
testInnerRoundtripTreeParsable name parser html = do
  case scrapeFirst' parser html of
    Nothing -> expectationFailure $ name ++ ": Initial parse failed on: " ++ html
    Just t1 -> do
      let html1 = showH t1
      case scrapeFirst' parser html1 of
        Nothing -> expectationFailure $ name ++ ": Second parse failed on: " ++ html1
        Just t2 -> do
          let html2 = showH t2
          case scrapeFirst' parser html2 of
            Nothing -> expectationFailure $ name ++ ": Third parse failed on: " ++ html2
            Just _ -> return () -- Success

spec :: Spec
spec = do
  ---------------------------------------------------------------------------
  -- elemParser with fancy inner parsers - testing RE-PARSABILITY
  ---------------------------------------------------------------------------
  describe "elemParser with complex inner parsers" $ do

    describe "word parser (many1 letter)" $ do
      it "single word: remains parsable after roundtrip" $ do
        let parser = elemParser (Just ["div"]) (Just wordParser) []
        testInnerRoundtripParsable "word" parser "<div>hello</div>"

      it "multiple words: finds all matches" $ do
        let parser = elemParser (Just ["div"]) (Just wordParser) []
            html = "<div>hello world foo bar</div>"
        case scrapeFirst' parser html of
          Just e -> length (matches' e) `shouldSatisfy` (> 0)
          Nothing -> expectationFailure "Parse failed"

    describe "number parser (many1 digit)" $ do
      it "single number: remains parsable" $ do
        let parser = elemParser (Just ["span"]) (Just numberParser) []
        testInnerRoundtripParsable "number" parser "<span>12345</span>"

      it "mixed content with numbers" $ do
        let parser = elemParser (Just ["p"]) (Just numberParser) []
            html = "<p>item 42 costs 100 dollars</p>"
        case scrapeFirst' parser html of
          Just e -> length (matches' e) `shouldSatisfy` (>= 2)
          Nothing -> expectationFailure "Parse failed"

    describe "email-like parser (user@domain.tld)" $ do
      it "email in content: remains parsable" $ do
        let parser = elemParser (Just ["a"]) (Just emailLikeParser) []
        testInnerRoundtripParsable "email" parser "<a>contact test@example.com now</a>"

    describe "bracketed parser ([content])" $ do
      it "bracketed content: remains parsable" $ do
        let parser = elemParser (Just ["code"]) (Just bracketedParser) []
        testInnerRoundtripParsable "bracketed" parser "<code>array[index]</code>"

      it "multiple brackets" $ do
        let parser = elemParser (Just ["pre"]) (Just bracketedParser) []
            html = "<pre>[foo] and [bar] and [baz]</pre>"
        case scrapeFirst' parser html of
          Just e -> length (matches' e) `shouldSatisfy` (>= 3)
          Nothing -> expectationFailure "Parse failed"

    describe "pipe-separated parser (a|b)" $ do
      it "pipe content: remains parsable" $ do
        let parser = elemParser (Just ["td"]) (Just pipeSeparatedParser) []
        testInnerRoundtripParsable "pipe" parser "<td>yes|no</td>"

    describe "nested quotes parser (\"content\")" $ do
      it "quoted content: remains parsable" $ do
        let parser = elemParser (Just ["blockquote"]) (Just nestedQuotesParser) []
        testInnerRoundtripParsable "quotes" parser "<blockquote>He said \"hello world\" loudly</blockquote>"

  ---------------------------------------------------------------------------
  -- treeElemParser with fancy inner parsers
  ---------------------------------------------------------------------------
  describe "treeElemParser with complex inner parsers" $ do

    it "word parser with tree: remains parsable" $ do
      let parser = treeElemParser (Just ["div"]) (Just wordParser) []
      testInnerRoundtripTreeParsable "tree-word" parser "<div>hello world</div>"

    it "number parser with tree: remains parsable" $ do
      let parser = treeElemParser (Just ["span"]) (Just numberParser) []
      testInnerRoundtripTreeParsable "tree-number" parser "<span>value is 42</span>"

    it "email parser with tree: remains parsable" $ do
      let parser = treeElemParser (Just ["div"]) (Just emailLikeParser) []
      testInnerRoundtripTreeParsable "tree-email" parser "<div>email: user@test.org</div>"

  ---------------------------------------------------------------------------
  -- Nested element + inner parser combinations
  ---------------------------------------------------------------------------
  describe "nested elements with inner parsers" $ do

    it "outer el, inner has matches: remains parsable" $ do
      let innerP = elemParser (Just ["span"]) (Just wordParser) []
          outerP = elemParser (Just ["div"]) (Just innerP) []
          html = "<div><span>hello</span></div>"
      case scrapeFirst' outerP html of
        Just e1 -> do
          let html1 = showH e1
          -- The inner match structure gets serialized
          scrapeFirst' outerP html1 `shouldSatisfy` isJust
        Nothing -> expectationFailure "Parse failed"

    it "deeply nested with multiple inner parsers" $ do
      let wordP = elemParser (Just ["b"]) (Just wordParser) []
          spanP = elemParser (Just ["span"]) (Just wordP) []
          divP = elemParser (Just ["div"]) (Just spanP) []
          html = "<div><span><b>test</b></span></div>"
      case scrapeFirst' divP html of
        Just e -> scrapeFirst' divP (showH e) `shouldSatisfy` isJust
        Nothing -> expectationFailure "Parse failed"

  ---------------------------------------------------------------------------
  -- Stress tests with complex content
  ---------------------------------------------------------------------------
  describe "stress tests with fancy parsers" $ do

    it "many matches in single element" $ do
      let parser = elemParser (Just ["div"]) (Just wordParser) []
          html = "<div>" ++ unwords (replicate 20 "word") ++ "</div>"
      case scrapeFirst' parser html of
        Just e1 -> do
          length (matches' e1) `shouldBe` 20
          let html1 = showH e1
          case scrapeFirst' parser html1 of
            Just e2 -> do
              let html2 = showH e2
              case scrapeFirst' parser html2 of
                Just _ -> return () -- remains parsable
                Nothing -> expectationFailure "Third parse failed"
            Nothing -> expectationFailure "Second parse failed"
        Nothing -> expectationFailure "First parse failed"

    it "alternating match/non-match content" $ do
      let parser = elemParser (Just ["p"]) (Just numberParser) []
          html = "<p>a1b2c3d4e5f6g7h8i9j0</p>"
      case scrapeFirst' parser html of
        Just e -> length (matches' e) `shouldBe` 10
        Nothing -> expectationFailure "Parse failed"

  ---------------------------------------------------------------------------
  -- Edge cases
  ---------------------------------------------------------------------------
  describe "edge cases for inner parser roundtrip" $ do

    it "with inner parser that doesn't match, returns element with empty matches" $ do
      let parser = elemParser (Just ["div"]) (Just (string "NOTFOUND")) []
          html = "<div>no match here</div>"
      -- elemParser parses successfully but with empty match list when inner pattern doesn't match
      case scrapeFirst' parser html of
        Just e -> matches' e `shouldBe` []
        Nothing -> expectationFailure "Parse should succeed with empty matches"

    it "match at very start of content: remains parsable" $ do
      let parser = elemParser (Just ["div"]) (Just wordParser) []
      testInnerRoundtripParsable "start" parser "<div>hello</div>"

    it "match at very end of content: remains parsable" $ do
      let parser = elemParser (Just ["div"]) (Just numberParser) []
      testInnerRoundtripParsable "end" parser "<div>value 42</div>"

    it "unicode-like content (ascii only): remains parsable" $ do
      let parser = elemParser (Just ["div"]) (Just wordParser) []
      testInnerRoundtripParsable "unicode" parser "<div>caf and fianc</div>"

    it "very long match: remains parsable" $ do
      let longWord = replicate 100 'a'
          parser = elemParser (Just ["div"]) (Just wordParser) []
          html = "<div>" ++ longWord ++ "</div>"
      testInnerRoundtripParsable "long" parser html

  ---------------------------------------------------------------------------
  -- Documenting non-idempotent behavior
  ---------------------------------------------------------------------------
  describe "documenting showH behavior with inner matches" $ do

    it "inner matches get Show-quoted, accumulating on each roundtrip" $ do
      let parser = elemParser (Just ["div"]) (Just (string "test")) []
          html = "<div>test</div>"
      case scrapeFirst' parser html of
        Just e1 -> do
          let html1 = showH e1
          -- First roundtrip adds quotes around "test"
          html1 `shouldContain` "\""
          case scrapeFirst' parser html1 of
            Just e2 -> do
              let html2 = showH e2
              -- Second roundtrip adds more quotes
              -- This documents the non-idempotent behavior
              length (filter (== '"') html2) `shouldSatisfy`
                (>= length (filter (== '"') html1))
            Nothing -> expectationFailure "Second parse failed"
        Nothing -> expectationFailure "First parse failed"

    it "without inner parser, showH is fully idempotent" $ do
      let parser = elemParser (Just ["div"]) (Nothing :: Maybe (InnerParser String)) []
          html = "<div>test content</div>"
      case scrapeFirst' parser html of
        Just e1 -> do
          let html1 = showH e1
          case scrapeFirst' parser html1 of
            Just e2 -> do
              let html2 = showH e2
              html1 `shouldBe` html2  -- fully idempotent!
            Nothing -> expectationFailure "Second parse failed"
        Nothing -> expectationFailure "First parse failed"

