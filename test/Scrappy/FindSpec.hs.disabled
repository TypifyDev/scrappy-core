module Scrappy.FindSpec (spec) where

import Test.Hspec
import Text.Parsec (parse, string)

import Scrappy.Find (findNaive, findUntilMatch, streamEdit)
import Scrappy.Elem.SimpleElemParser (el)
import Scrappy.Elem.Types (elTag)

spec :: Spec
spec = do
  describe "findNaive" $ do
    it "returns Just [a] when single pattern is found" $ do
      let result = parse (findNaive (el "a" [])) "" "<a>link</a>"
      case result of
        Right (Just [elem]) -> elTag elem `shouldBe` "a"
        Right (Just xs) -> expectationFailure $ "Expected 1 match, got " ++ show (length xs)
        Right Nothing -> expectationFailure "Expected Just, got Nothing"
        Left err -> expectationFailure $ show err

    it "returns Just [a, b, ...] when multiple patterns are found" $ do
      let html = "<a>first</a>some text<a>second</a>"
      let result = parse (findNaive (el "a" [])) "" html
      case result of
        Right (Just xs) -> length xs `shouldBe` 2
        Right Nothing -> expectationFailure "Expected Just, got Nothing"
        Left err -> expectationFailure $ show err

    it "returns Nothing when no patterns are found" $ do
      let result = parse (findNaive (el "a" [])) "" "no links here"
      case result of
        Right Nothing -> return ()
        Right (Just _) -> expectationFailure "Expected Nothing, got Just"
        Left err -> expectationFailure $ show err

    it "returns Nothing on empty input" $ do
      let result = parse (findNaive (el "a" [])) "" ""
      case result of
        Right Nothing -> return ()
        Right (Just _) -> expectationFailure "Expected Nothing, got Just"
        Left err -> expectationFailure $ show err

    it "finds patterns interspersed with non-matching text" $ do
      let html = "text<div>1</div>more text<div>2</div>end"
      let result = parse (findNaive (el "div" [])) "" html
      case result of
        Right (Just xs) -> length xs `shouldBe` 2
        Right Nothing -> expectationFailure "Expected Just, got Nothing"
        Left err -> expectationFailure $ show err

    it "finds patterns at start of input" $ do
      let result = parse (findNaive (el "p" [])) "" "<p>start</p>rest"
      case result of
        Right (Just [elem]) -> elTag elem `shouldBe` "p"
        Right _ -> expectationFailure "Expected exactly 1 match"
        Left err -> expectationFailure $ show err

    it "finds patterns at end of input" $ do
      let result = parse (findNaive (el "p" [])) "" "start<p>end</p>"
      case result of
        Right (Just [elem]) -> elTag elem `shouldBe` "p"
        Right _ -> expectationFailure "Expected exactly 1 match"
        Left err -> expectationFailure $ show err

  describe "findUntilMatch" $ do
    it "returns the first match found" $ do
      let html = "prefix<a>first</a><a>second</a>"
      let result = parse (findUntilMatch (el "a" [])) "" html
      case result of
        Right elem -> elTag elem `shouldBe` "a"
        Left err -> expectationFailure $ show err

    it "skips non-matching content correctly" $ do
      let html = "lots of text here <div>found</div> more text"
      let result = parse (findUntilMatch (el "div" [])) "" html
      case result of
        Right elem -> elTag elem `shouldBe` "div"
        Left err -> expectationFailure $ show err

  describe "streamEdit" $ do
    it "replaces matched patterns" $ do
      let result = streamEdit (string "foo") (const "bar") "foo baz foo"
      result `shouldBe` "bar baz bar"

    it "preserves non-matching text" $ do
      let result = streamEdit (string "x") (const "y") "abcdef"
      result `shouldBe` "abcdef"

    it "works with multiple replacements" $ do
      let result = streamEdit (string "a") (const "X") "abracadabra"
      result `shouldBe` "XbrXcXdXbrX"
