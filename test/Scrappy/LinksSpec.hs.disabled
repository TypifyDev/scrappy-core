module Scrappy.LinksSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Scrappy.Links
  ( Link(..)
  , fixRelativeUrl
  , parseLink
  , maybeUsefulUrl
  , deriveBaseUrl
  , sameAuthority
  )

spec :: Spec
spec = do
  describe "fixRelativeUrl" $ do
    it "returns base URL when URL is empty" $ do
      fixRelativeUrl (Link "https://example.com") "" `shouldBe` "https://example.com"

    it "returns base URL when URL is /" $ do
      fixRelativeUrl (Link "https://example.com") "/" `shouldBe` "https://example.com"

    it "returns URL unchanged when it contains the base URL" $ do
      fixRelativeUrl (Link "https://example.com") "https://example.com/page"
        `shouldBe` "https://example.com/page"

    it "handles relative URL with leading / when base has trailing /" $ do
      fixRelativeUrl (Link "https://example.com/") "/path"
        `shouldBe` "https://example.com/path"

    it "handles relative URL without leading / when base has trailing /" $ do
      fixRelativeUrl (Link "https://example.com/") "path"
        `shouldBe` "https://example.com/path"

    it "handles relative URL with leading / when base has no trailing /" $ do
      fixRelativeUrl (Link "https://example.com") "/path"
        `shouldBe` "https://example.com/path"

    it "handles relative URL without leading / when base has no trailing /" $ do
      fixRelativeUrl (Link "https://example.com") "path"
        `shouldBe` "https://example.com/path"

    it "is idempotent for already-fixed URLs" $ property $
      \path -> let base = Link "https://example.com"
                   fixed = fixRelativeUrl base ("/" ++ filter (/= '/') path)
                   fixedAgain = fixRelativeUrl base fixed
               in fixed == fixedAgain

  describe "parseLink" $ do
    it "returns Just Link for valid same-site URL when onlySameSite is True" $ do
      let lastLink = Link "https://example.com/page1"
      parseLink True lastLink "https://example.com/page2"
        `shouldBe` Just (Link "https://example.com/page2")

    -- Note: sameAuthority checks TLD only, so example.com and other.com are considered same
    it "returns Nothing for different-site URL when onlySameSite is True" $ do
      let lastLink = Link "https://example.com/page1"
      -- Different TLD would fail (e.g. .com vs .org)
      parseLink True lastLink "https://other.org/page" `shouldBe` Nothing

    it "returns Just Link for any valid HTTP URL when onlySameSite is False" $ do
      let lastLink = Link "https://example.com/page1"
      parseLink False lastLink "https://other.com/page"
        `shouldBe` Just (Link "https://other.com/page")

    it "returns Nothing for javascript: URLs" $ do
      let lastLink = Link "https://example.com/"
      parseLink False lastLink "javascript:void(0)" `shouldBe` Nothing

    it "fixes relative URLs correctly" $ do
      let lastLink = Link "https://example.com/dir/"
      case parseLink False lastLink "/other" of
        Just (Link url) -> url `shouldBe` "https://example.com/other"
        Nothing -> expectationFailure "Expected Just Link"

  describe "maybeUsefulUrl" $ do
    it "filters out JavaScript URLs" $ do
      let base = Link "https://example.com"
      maybeUsefulUrl base (Link "https://example.com/javascript:void(0)") `shouldBe` Nothing

    it "filters out fragment-only URLs" $ do
      let base = Link "https://example.com"
      maybeUsefulUrl base (Link "https://example.com/#section") `shouldBe` Nothing

    it "filters out URLs with query parameters" $ do
      let base = Link "https://example.com"
      maybeUsefulUrl base (Link "https://example.com/page?query=1") `shouldBe` Nothing

    it "filters out URLs not containing base URL" $ do
      let base = Link "https://example.com"
      maybeUsefulUrl base (Link "https://other.com/page") `shouldBe` Nothing

    it "allows URLs with allowed file extensions" $ do
      let base = Link "https://example.com"
      maybeUsefulUrl base (Link "https://example.com/doc.html")
        `shouldBe` Just (Link "https://example.com/doc.html")
      maybeUsefulUrl base (Link "https://example.com/doc.pdf")
        `shouldBe` Just (Link "https://example.com/doc.pdf")

    it "filters out disallowed file extensions" $ do
      let base = Link "https://example.com"
      maybeUsefulUrl base (Link "https://example.com/file.zip") `shouldBe` Nothing
      maybeUsefulUrl base (Link "https://example.com/file.exe") `shouldBe` Nothing

  describe "deriveBaseUrl" $ do
    it "extracts scheme and host from full URL" $ do
      deriveBaseUrl (Link "https://example.com/path/page.html")
        `shouldBe` Just (Link "https://example.com")

    it "works with http scheme" $ do
      deriveBaseUrl (Link "http://example.com/path")
        `shouldBe` Just (Link "http://example.com")

    it "returns Nothing for invalid URLs" $ do
      deriveBaseUrl (Link "not-a-url") `shouldBe` Nothing

  describe "sameAuthority" $ do
    it "returns True for same domain" $ do
      sameAuthority "https://example.com/page" (Link "https://example.com/other")
        `shouldBe` True

    -- Note: sameAuthority compares TLDs only (last segment after dot)
    it "returns False for different TLDs" $ do
      sameAuthority "https://example.com/page" (Link "https://example.org/page")
        `shouldBe` False

    it "handles subdomains correctly" $ do
      -- Same TLD means same authority in this implementation
      sameAuthority "https://www.example.com" (Link "https://api.example.com")
        `shouldBe` True
