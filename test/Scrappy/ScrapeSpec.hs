module Scrappy.ScrapeSpec (spec) where

import Test.Hspec
import Data.Maybe (isNothing, isJust)

import Scrappy.Scrape (scrape, scrapeFirst', exists)
import Scrappy.Elem.SimpleElemParser (el)
import Scrappy.Elem.Types (elTag, innerText')

import TestUtils (loadFixture)

spec :: Spec
spec = do
  describe "scrape" $ do
    it "returns Just [a] for single match" $ do
      let html = "<div>content</div>"
      case scrape (el "div" []) html of
        Just [e] -> do
          elTag e `shouldBe` "div"
          innerText' e `shouldBe` "content"
        Just xs -> expectationFailure $ "Expected 1 match, got " ++ show (length xs)
        Nothing -> expectationFailure "Expected Just, got Nothing"

    it "returns Just [a, b, ...] for multiple matches" $ do
      let html = "<a>first</a><a>second</a><a>third</a>"
      case scrape (el "a" []) html of
        Just xs -> length xs `shouldBe` 3
        Nothing -> expectationFailure "Expected Just, got Nothing"

    it "returns Nothing when pattern is not found" $ do
      let html = "<div>no links here</div>"
      scrape (el "a" []) html `shouldSatisfy` isNothing

    it "returns Nothing on empty input" $ do
      scrape (el "div" []) "" `shouldSatisfy` isNothing

    it "works with fixture file" $ do
      html <- loadFixture "simple.html"
      case scrape (el "a" []) html of
        Just xs -> length xs `shouldSatisfy` (> 0)
        Nothing -> expectationFailure "Expected to find links in simple.html"

    it "finds elements with specific attributes" $ do
      let html = "<div id=\"main\">main</div><div id=\"other\">other</div>"
      case scrape (el "div" [("id", "main")]) html of
        Just [e] -> innerText' e `shouldBe` "main"
        Just xs -> expectationFailure $ "Expected 1 match, got " ++ show (length xs)
        Nothing -> expectationFailure "Expected Just, got Nothing"

  describe "scrapeFirst'" $ do
    it "returns Just a for first match" $ do
      let html = "<p>first</p><p>second</p>"
      case scrapeFirst' (el "p" []) html of
        Just e -> innerText' e `shouldBe` "first"
        Nothing -> expectationFailure "Expected Just, got Nothing"

    it "returns Nothing when no matches" $ do
      let html = "<div>no paragraphs</div>"
      scrapeFirst' (el "p" []) html `shouldSatisfy` isNothing

    it "returns first match even with many" $ do
      let html = "<span>1</span><span>2</span><span>3</span>"
      case scrapeFirst' (el "span" []) html of
        Just e -> innerText' e `shouldBe` "1"
        Nothing -> expectationFailure "Expected Just, got Nothing"

  describe "exists" $ do
    it "returns True when pattern exists" $ do
      let html = "<div><a>link</a></div>"
      exists (el "a" []) html `shouldBe` True

    it "returns False when pattern does not exist" $ do
      let html = "<div>no links</div>"
      exists (el "a" []) html `shouldBe` False

    it "returns False on empty input" $ do
      exists (el "div" []) "" `shouldBe` False

    it "works with nested elements" $ do
      html <- loadFixture "nested.html"
      exists (el "div" [("id", "deep")]) html `shouldBe` True
      exists (el "div" [("id", "nonexistent")]) html `shouldBe` False
