{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.RoundtripSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (ParsecT, string)
import Data.Functor.Identity (Identity)
import Data.Maybe (isJust)

-- Scrappy.Scrape functions for running parsers
import Scrappy.Scrape
  ( scrape
  , scrapeFirst'
  , runScraperOnHtml
  , runScraperOnHtml1
  )

-- All parsers from SimpleElemParser
import Scrappy.Elem.SimpleElemParser
  ( el
  , elemParser
  , elemParserWhere
  , sameElTag
  , clickableHref
  )

-- All parsers from TreeElemParser
import Scrappy.Elem.TreeElemParser
  ( treeElemParser
  , htmlGroup
  , htmlGroupSimilar
  , sameTreeH
  , similarTreeH
  )

-- Types and ShowHTML for roundtrip
import Scrappy.Elem.Types
  ( Elem'
  , TreeHTML
  , GroupHtml
  , showH
  , elTag
  , attrs
  , innerText'
  , matches'
  , _topEl
  , _topAttrs
  , _innerText'
  )

import Scrappy.Links (Link(..))

-- Type aliases for cleaner signatures
type ElemParserS = ParsecT String () Identity (Elem' String)
type TreeParserS = ParsecT String () Identity (TreeHTML String)

-- Helper: elemParser with explicit String type
elemParserS :: Maybe [String] -> [(String, Maybe String)] -> ElemParserS
elemParserS tags attrss = elemParser tags (Nothing :: Maybe (ParsecT String () Identity String)) attrss

-- Helper: treeElemParser with explicit String type
treeParserS :: Maybe [String] -> [(String, Maybe String)] -> TreeParserS
treeParserS tags attrss = treeElemParser tags (Nothing :: Maybe (ParsecT String () Identity String)) attrss

-- | Scrape then show, using Scrappy.Scrape functions
scrapeAndShow :: ElemParserS -> String -> Maybe String
scrapeAndShow parser html = case scrapeFirst' parser html of
  Just e  -> Just (showH e)
  Nothing -> Nothing

-- | Scrape then show for TreeHTML
scrapeAndShowTree :: TreeParserS -> String -> Maybe String
scrapeAndShowTree parser html = case scrapeFirst' parser html of
  Just t  -> Just (showH t)
  Nothing -> Nothing

-- | Iterate scrape-show n times for Elem'
iterateScrapeShow :: ElemParserS -> Int -> String -> Maybe String
iterateScrapeShow _ 0 html = Just html
iterateScrapeShow parser n html = case scrapeAndShow parser html of
  Just html' -> iterateScrapeShow parser (n - 1) html'
  Nothing    -> Nothing

-- | Iterate scrape-show n times for TreeHTML
iterateScrapeShowTree :: TreeParserS -> Int -> String -> Maybe String
iterateScrapeShowTree _ 0 html = Just html
iterateScrapeShowTree parser n html = case scrapeAndShowTree parser html of
  Just html' -> iterateScrapeShowTree parser (n - 1) html'
  Nothing    -> Nothing

spec :: Spec
spec = do
  ---------------------------------------------------------------------------
  -- el parser roundtrips
  ---------------------------------------------------------------------------
  describe "el parser roundtrip" $ do
    it "el: Text -> Elem' -> Text is stable after multiple iterations" $ do
      let html = "<div>content</div>"
          parser = el "div" []
      let once = scrapeAndShow parser html
          twice = once >>= scrapeAndShow parser
          thrice = twice >>= scrapeAndShow parser
      once `shouldBe` twice
      twice `shouldBe` thrice

    it "el: stable after 10 iterations" $ do
      let html = "<a href=\"/link\">click me</a>"
          parser = el "a" []
      let first = scrapeAndShow parser html
          tenth = iterateScrapeShow parser 10 html
      first `shouldBe` tenth

    it "el with attributes: roundtrip preserves attributes" $ do
      let html = "<div id=\"main\" class=\"container\">text</div>"
          parser = el "div" [("id", "main")]
      case scrapeFirst' parser html of
        Just e1 -> do
          let html' = showH e1
          case scrapeFirst' parser html' of
            Just e2 -> do
              elTag e1 `shouldBe` elTag e2
              attrs e1 `shouldBe` attrs e2
              innerText' e1 `shouldBe` innerText' e2
            Nothing -> expectationFailure "Second scrape failed"
        Nothing -> expectationFailure "First scrape failed"

    it "el: empty element roundtrip" $ do
      let html = "<span></span>"
          parser = el "span" []
      let first = scrapeAndShow parser html
          second = first >>= scrapeAndShow parser
      first `shouldBe` second

  ---------------------------------------------------------------------------
  -- elemParser roundtrips
  ---------------------------------------------------------------------------
  describe "elemParser roundtrip" $ do
    it "elemParser Nothing: matches any element, stable roundtrip" $ do
      let html = "<custom>content</custom>"
          parser = elemParserS Nothing []
      let once = scrapeAndShow parser html
          twice = once >>= scrapeAndShow parser
      once `shouldBe` twice

    it "elemParser Just [tags]: stable roundtrip" $ do
      let html = "<article>news</article>"
          parser = elemParserS (Just ["article", "section"]) []
      let first = scrapeAndShow parser html
          fifth = iterateScrapeShow parser 5 html
      first `shouldBe` fifth

    it "elemParser with attrs subset: roundtrip" $ do
      let html = "<input type=\"text\" name=\"field\">"
          parser = elemParserS (Just ["input"]) [("type", Just "text")]
      let once = scrapeAndShow parser html
          twice = once >>= scrapeAndShow parser
      once `shouldBe` twice

    -- Note: When using inner patterns, the matched content gets Show-quoted
    -- in innerMatches, which affects showH output. This is expected library
    -- behavior - inner pattern matching is for extraction, not roundtrip.
    -- We test that the parser still finds matches on re-parse.
    it "elemParser with inner pattern: re-parsable (not idempotent)" $ do
      let html = "<div>hello world</div>"
          parser = elemParser (Just ["div"]) (Just (string "hello")) []
      case scrapeFirst' parser html of
        Just e1 -> do
          -- Verify we found the match
          length (matches' e1) `shouldBe` 1
          -- The showH output can be parsed again (may have different content)
          let html' = showH e1
          scrapeFirst' parser html' `shouldSatisfy` isJust
        Nothing -> expectationFailure "First parse failed"

  ---------------------------------------------------------------------------
  -- sameElTag roundtrips
  ---------------------------------------------------------------------------
  describe "sameElTag roundtrip" $ do
    it "sameElTag: stable for nested same-tag elements" $ do
      let html = "<div><div>inner</div></div>"
          parser = sameElTag "div" (Nothing :: Maybe (ParsecT String () Identity String))
      let first = scrapeAndShow parser html
          third = iterateScrapeShow parser 3 html
      first `shouldBe` third

  ---------------------------------------------------------------------------
  -- treeElemParser roundtrips
  ---------------------------------------------------------------------------
  describe "treeElemParser roundtrip" $ do
    it "treeElemParser: Text -> TreeHTML -> Text is stable" $ do
      let html = "<div>content</div>"
          parser = treeParserS (Just ["div"]) []
      let once = scrapeAndShowTree parser html
          twice = once >>= scrapeAndShowTree parser
          thrice = twice >>= scrapeAndShowTree parser
      once `shouldBe` twice
      twice `shouldBe` thrice

    it "treeElemParser: stable after 10 iterations" $ do
      let html = "<section>text here</section>"
          parser = treeParserS (Just ["section"]) []
      let first = scrapeAndShowTree parser html
          tenth = iterateScrapeShowTree parser 10 html
      first `shouldBe` tenth

    it "treeElemParser with nested elements: roundtrip preserves structure" $ do
      let html = "<div><span>nested</span></div>"
          parser = treeParserS (Just ["div"]) []
      case scrapeFirst' parser html of
        Just t1 -> do
          let html' = showH t1
          case scrapeFirst' parser html' of
            Just t2 -> do
              _topEl t1 `shouldBe` _topEl t2
              _topAttrs t1 `shouldBe` _topAttrs t2
              _innerText' t1 `shouldBe` _innerText' t2
            Nothing -> expectationFailure "Second scrape failed"
        Nothing -> expectationFailure "First scrape failed"

    it "treeElemParser Nothing: any element, stable" $ do
      let html = "<custom>data</custom>"
          parser = treeParserS Nothing []
      let once = scrapeAndShowTree parser html
          twice = once >>= scrapeAndShowTree parser
      once `shouldBe` twice

    it "treeElemParser with attrs: roundtrip" $ do
      let html = "<a href=\"/page\" class=\"link\">click</a>"
          parser = treeParserS (Just ["a"]) [("href", Just "/page")]
      let first = scrapeAndShowTree parser html
          fifth = iterateScrapeShowTree parser 5 html
      first `shouldBe` fifth

  ---------------------------------------------------------------------------
  -- sameTreeH roundtrips
  ---------------------------------------------------------------------------
  describe "sameTreeH roundtrip" $ do
    it "sameTreeH: parses same structure repeatedly" $ do
      let html = "<li class=\"item\">first</li>"
          parser = treeParserS (Just ["li"]) [("class", Just "item")]
      case scrapeFirst' parser html of
        Just treeH -> do
          let sameParser = sameTreeH (Nothing :: Maybe (ParsecT String () Identity String)) treeH
          case scrapeFirst' sameParser html of
            Just treeH2 -> do
              _topEl treeH `shouldBe` _topEl treeH2
              _topAttrs treeH `shouldBe` _topAttrs treeH2
            Nothing -> expectationFailure "sameTreeH failed"
        Nothing -> expectationFailure "Initial parse failed"

  ---------------------------------------------------------------------------
  -- Cross-parser roundtrips: Elem' <-> TreeHTML
  ---------------------------------------------------------------------------
  describe "Cross-parser roundtrips" $ do
    it "el output can be parsed by treeElemParser" $ do
      let html = "<div>content</div>"
      case scrapeFirst' (el "div" []) html of
        Just e -> do
          let html' = showH e
          case scrapeFirst' (treeParserS (Just ["div"]) []) html' of
            Just t -> _topEl t `shouldBe` "div"
            Nothing -> expectationFailure "TreeParser failed on Elem' output"
        Nothing -> expectationFailure "el parse failed"

    it "treeElemParser output can be parsed by el" $ do
      let html = "<span>text</span>"
      case scrapeFirst' (treeParserS (Just ["span"]) []) html of
        Just t -> do
          let html' = showH t
          case scrapeFirst' (el "span" []) html' of
            Just e -> elTag e `shouldBe` "span"
            Nothing -> expectationFailure "el failed on TreeHTML output"
        Nothing -> expectationFailure "TreeParser failed"

    it "alternating parsers: el -> tree -> el -> tree" $ do
      let html = "<p>paragraph</p>"
      case scrapeFirst' (el "p" []) html of
        Just e1 -> do
          let h1 = showH e1
          case scrapeFirst' (treeParserS (Just ["p"]) []) h1 of
            Just t1 -> do
              let h2 = showH t1
              case scrapeFirst' (el "p" []) h2 of
                Just e2 -> do
                  let h3 = showH e2
                  case scrapeFirst' (treeParserS (Just ["p"]) []) h3 of
                    Just t2 -> do
                      innerText' e1 `shouldBe` innerText' e2
                      _innerText' t1 `shouldBe` _innerText' t2
                    Nothing -> expectationFailure "Fourth parse failed"
                Nothing -> expectationFailure "Third parse failed"
            Nothing -> expectationFailure "Second parse failed"
        Nothing -> expectationFailure "First parse failed"

  ---------------------------------------------------------------------------
  -- scrape (multiple results) roundtrips
  ---------------------------------------------------------------------------
  describe "scrape (multiple matches) roundtrip" $ do
    it "scrape: all results re-scrapable" $ do
      let html = "<a>one</a><a>two</a><a>three</a>"
      case scrape (el "a" []) html of
        Just elems -> do
          length elems `shouldBe` 3
          -- Each showH output should be scrapable
          let htmls = map showH elems
          let reScraped = map (scrapeFirst' (el "a" [])) htmls
          all isJust reScraped `shouldBe` True
        Nothing -> expectationFailure "scrape failed"

    it "scrape results showH then scrape again: same count" $ do
      let html = "<li>a</li><li>b</li>"
      case scrape (el "li" []) html of
        Just elems -> do
          let combined = concatMap showH elems
          case scrape (el "li" []) combined of
            Just elems' -> length elems' `shouldBe` length elems
            Nothing -> expectationFailure "Re-scrape failed"
        Nothing -> expectationFailure "Initial scrape failed"

  ---------------------------------------------------------------------------
  -- runScraperOnHtml roundtrips
  ---------------------------------------------------------------------------
  describe "runScraperOnHtml roundtrip" $ do
    it "runScraperOnHtml: results are re-parsable" $ do
      let html = "text<div>a</div>more<div>b</div>end"
      case runScraperOnHtml (el "div" []) html of
        Just elems -> do
          length elems `shouldBe` 2
          let htmls = map showH elems
          mapM_ (\h -> runScraperOnHtml1 (el "div" []) h `shouldSatisfy` isJust) htmls
        Nothing -> expectationFailure "runScraperOnHtml failed"

  ---------------------------------------------------------------------------
  -- QuickCheck properties
  ---------------------------------------------------------------------------
  describe "QuickCheck roundtrip properties" $ do
    it "prop: scrape . showH . scrape is idempotent for el parser" $ property $
      \(content :: String) ->
        let safeContent = filter (`notElem` "<>&\"'\\") content
            html = "<div>" ++ safeContent ++ "</div>"
            parser = el "div" []
        in case scrapeAndShow parser html of
             Nothing -> property True
             Just h1 -> case scrapeAndShow parser h1 of
               Nothing -> property False
               Just h2 -> h1 === h2

    it "prop: scrape . showH . scrape is idempotent for treeElemParser" $ property $
      \(content :: String) ->
        let safeContent = filter (`notElem` "<>&\"'\\") content
            html = "<span>" ++ safeContent ++ "</span>"
            parser = treeParserS (Just ["span"]) []
        in case scrapeAndShowTree parser html of
             Nothing -> property True
             Just h1 -> case scrapeAndShowTree parser h1 of
               Nothing -> property False
               Just h2 -> h1 === h2

    it "prop: iterations converge to fixed point (el)" $ property $
      \(Positive n) ->
        let iterations = min n 15
            html = "<div>test</div>"
            parser = el "div" []
            r1 = iterateScrapeShow parser iterations html
            r2 = iterateScrapeShow parser (iterations + 1) html
        in r1 === r2

    it "prop: iterations converge to fixed point (treeElemParser)" $ property $
      \(Positive n) ->
        let iterations = min n 15
            html = "<p>test</p>"
            parser = treeParserS (Just ["p"]) []
            r1 = iterateScrapeShowTree parser iterations html
            r2 = iterateScrapeShowTree parser (iterations + 1) html
        in r1 === r2

  ---------------------------------------------------------------------------
  -- Edge cases
  ---------------------------------------------------------------------------
  describe "Roundtrip edge cases" $ do
    it "self-closing elements: roundtrip stable" $ do
      let html = "<br>"
          parser = elemParserS (Just ["br"]) []
      let once = scrapeAndShow parser html
          twice = once >>= scrapeAndShow parser
      once `shouldBe` twice

    it "element with many attributes: roundtrip preserves all" $ do
      let html = "<div id=\"x\" class=\"y\" data-value=\"z\">content</div>"
          parser = el "div" []
      case scrapeFirst' parser html of
        Just e1 -> do
          let html' = showH e1
          case scrapeFirst' parser html' of
            Just e2 -> attrs e1 `shouldBe` attrs e2
            Nothing -> expectationFailure "Second parse failed"
        Nothing -> expectationFailure "First parse failed"

    it "deeply nested: roundtrip stable" $ do
      let html = "<div><div><div>deep</div></div></div>"
          parser = el "div" []
      let first = scrapeAndShow parser html
          tenth = iterateScrapeShow parser 10 html
      first `shouldBe` tenth

    it "whitespace content: roundtrip stable" $ do
      let html = "<pre>  spaces  \n  newlines  </pre>"
          parser = el "pre" []
      let once = scrapeAndShow parser html
          twice = once >>= scrapeAndShow parser
      once `shouldBe` twice

    it "special characters in content: roundtrip stable" $ do
      let html = "<code>foo &amp; bar</code>"
          parser = el "code" []
      let once = scrapeAndShow parser html
          twice = once >>= scrapeAndShow parser
      once `shouldBe` twice
