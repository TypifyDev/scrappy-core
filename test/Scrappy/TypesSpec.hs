module Scrappy.TypesSpec (spec) where

import Data.Functor.Identity (Identity)
import Test.Hspec
import Text.Parsec (ParsecT, anyChar, parse, parserZero)

import Scrappy.Types (ScrapeFail (..), mapMaybe)

spec :: Spec
spec = do
    describe "ScrapeFail" $ do
        it "shows Eof" $
            show Eof `shouldBe` "Eof"

        it "shows NonMatch" $
            show NonMatch `shouldBe` "NonMatch"

    describe "mapMaybe" $ do
        it "succeeds when function returns Just" $ do
            let p :: ParsecT String () Identity Char
                p = anyChar
                f c = if c == 'a' then Just 'A' else Nothing
            case parse (mapMaybe f p) "" "a" of
                Right r -> r `shouldBe` 'A'
                Left err -> expectationFailure $ show err

        it "fails when function returns Nothing" $ do
            let p :: ParsecT String () Identity Char
                p = anyChar
                f _ = Nothing :: Maybe Char
            case parse (mapMaybe f p) "" "x" of
                Right _ -> expectationFailure "Expected failure but parser succeeded"
                Left _ -> pure ()

        it "fails when underlying parser fails" $ do
            let p :: ParsecT String () Identity Char
                p = parserZero
                f = Just
            case parse (mapMaybe f p) "" "x" of
                Right _ -> expectationFailure "Expected failure but parser succeeded"
                Left _ -> pure ()
