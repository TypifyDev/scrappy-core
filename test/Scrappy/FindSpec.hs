{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.FindSpec (spec) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Either (isRight)
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Test.Hspec
import Text.Parsec (
    ParsecT,
    char,
    digit,
    parse,
    runParserT,
    string,
 )

import Scrappy.Find (
    StreamEditCase (..),
    baseParser,
    buildSequentialElemsParser,
    editFirst,
    endStream,
    find,
    findAllBetween,
    findEdit,
    findIO,
    findNaive,
    findNaiveIO,
    findSequential,
    findSequential2,
    findSequential3,
    findSomeHTML,
    findSomeHTMLNaive,
    findUntilMatch,
    givesNothing,
    streamEdit,
 )
import Scrappy.Types (ScrapeFail (..))
import TestUtils (shouldBeUndefined)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Unwrap a successful parse, failing the test on error.
shouldParse :: (Show a) => ParsecT String () Identity a -> String -> IO a
shouldParse p input = case parse p "" input of
    Right a -> pure a
    Left err -> do
        expectationFailure ("Parse failed: " ++ show err)
        -- unreachable, but needed to satisfy the type checker
        error "unreachable"

-- | Assert that a parser fails on the given input.
shouldFailOn :: (Show a) => ParsecT String () Identity a -> String -> Expectation
shouldFailOn p input = case parse p "" input of
    Right v -> expectationFailure ("Expected parse failure but got: " ++ show v)
    Left _ -> pure ()

-- | Count how many Right values are in a list of Eithers.
countRights :: [Either a b] -> Int
countRights = length . filter isRight

-- | Assert an Either is a Left with a specific ScrapeFail constructor.
expectLeftEof :: Either ScrapeFail a -> Expectation
expectLeftEof (Left Eof) = pure ()
expectLeftEof (Left NonMatch) = expectationFailure "Expected Left Eof but got Left NonMatch"
expectLeftEof (Right _) = expectationFailure "Expected Left Eof but got Right"

expectLeftNonMatch :: Either ScrapeFail a -> Expectation
expectLeftNonMatch (Left NonMatch) = pure ()
expectLeftNonMatch (Left Eof) = expectationFailure "Expected Left NonMatch but got Left Eof"
expectLeftNonMatch (Right _) = expectationFailure "Expected Left NonMatch but got Right"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
    -- -----------------------------------------------------------------------
    -- find
    -- -----------------------------------------------------------------------
    describe "find" $ do
        it "finds multiple occurrences in a stream" $ do
            results <- shouldParse (find (string "abc")) "xxxabcxxxabcxxx"
            countRights results `shouldBe` 2

        it "returns empty list on empty input" $ do
            results <- shouldParse (find (string "abc")) ""
            length results `shouldBe` 0

        it "returns empty list when there are no matches" $ do
            results <- shouldParse (find (string "abc")) "xxxxxx"
            countRights results `shouldBe` 0

        it "finds a match at the very start" $ do
            results <- shouldParse (find (string "abc")) "abcdef"
            countRights results `shouldBe` 1

        it "finds a match at the very end" $ do
            results <- shouldParse (find (string "abc")) "defabc"
            countRights results `shouldBe` 1

        it "finds adjacent matches" $ do
            results <- shouldParse (find (string "aa")) "aaaa"
            countRights results `shouldBe` 2

        it "finds single-character matches" $ do
            results <- shouldParse (find (char 'x')) "axbxcx"
            countRights results `shouldBe` 3

        it "collects only Right values (no Left in final list)" $ do
            results <- shouldParse (find (string "abc")) "xxxabcxxx"
            -- find discards Left NonMatch and Left Eof internally;
            -- the returned list contains only Rights.
            all isRight results `shouldBe` True

    -- -----------------------------------------------------------------------
    -- findNaive
    -- -----------------------------------------------------------------------
    describe "findNaive" $ do
        it "returns Just [results] when matches exist" $ do
            result <- shouldParse (findNaive (string "abc")) "xxxabcxxxabcxxx"
            case result of
                Just xs -> length xs `shouldBe` 2
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when no matches exist" $ do
            result <- shouldParse (findNaive (string "abc")) "xxxxxx"
            result `shouldBe` Nothing

        it "returns Nothing on empty input" $ do
            result <- shouldParse (findNaive (string "abc")) ""
            result `shouldBe` Nothing

        it "returns Just with single match" $ do
            result <- shouldParse (findNaive (string "abc")) "abc"
            case result of
                Just xs -> do
                    length xs `shouldBe` 1
                    case xs of
                        (x : _) -> x `shouldBe` "abc"
                        [] -> expectationFailure "Expected non-empty list"
                Nothing -> expectationFailure "Expected Just but got Nothing"

    -- -----------------------------------------------------------------------
    -- findUntilMatch
    -- -----------------------------------------------------------------------
    describe "findUntilMatch" $ do
        it "skips noise and returns the first match" $ do
            result <- shouldParse (findUntilMatch (string "target")) "noise noise target rest"
            result `shouldBe` "target"

        it "succeeds when match is at the start" $ do
            result <- shouldParse (findUntilMatch (string "abc")) "abcdef"
            result `shouldBe` "abc"

        it "fails when eof is reached without a match" $ do
            shouldFailOn (findUntilMatch (string "abc")) "xxxxxx"

        it "fails on empty input" $ do
            shouldFailOn (findUntilMatch (string "abc")) ""

    -- -----------------------------------------------------------------------
    -- findSequential2
    -- -----------------------------------------------------------------------
    describe "findSequential2" $ do
        it "finds two patterns in order with noise between them" $ do
            let p = findSequential2 (string "hello", string "world")
            (a, b) <- shouldParse p "noise hello noise world tail"
            a `shouldBe` "hello"
            b `shouldBe` "world"

        it "works when patterns are adjacent" $ do
            let p = findSequential2 (string "ab", string "cd")
            (a, b) <- shouldParse p "abcd"
            a `shouldBe` "ab"
            b `shouldBe` "cd"

        it "fails when first pattern is missing" $ do
            let p = findSequential2 (string "hello", string "world")
            shouldFailOn p "noise world noise"

        it "fails when second pattern is missing" $ do
            let p = findSequential2 (string "hello", string "world")
            shouldFailOn p "noise hello noise"

    -- -----------------------------------------------------------------------
    -- findSequential3
    -- -----------------------------------------------------------------------
    describe "findSequential3" $ do
        it "finds three patterns in order" $ do
            let p = findSequential3 (string "a", string "b", string "c")
            (x, y, z) <- shouldParse p "..a..b..c.."
            x `shouldBe` "a"
            y `shouldBe` "b"
            z `shouldBe` "c"

        it "fails when any pattern is missing" $ do
            let p = findSequential3 (string "a", string "b", string "c")
            shouldFailOn p "..a..b.."

    -- -----------------------------------------------------------------------
    -- streamEdit
    -- -----------------------------------------------------------------------
    describe "streamEdit" $ do
        it "replaces all occurrences" $ do
            let result = streamEdit (string "foo") (const "bar") "hello foo world foo end"
            result `shouldBe` "hello bar world bar end"

        it "passes through when there are no matches" $ do
            let result = streamEdit (string "foo") (const "bar") "hello world"
            result `shouldBe` "hello world"

        it "handles empty input" $ do
            let result = streamEdit (string "foo") (const "bar") ""
            result `shouldBe` ""

        it "replaces at start of string" $ do
            let result = streamEdit (string "foo") (const "bar") "foo end"
            result `shouldBe` "bar end"

        it "replaces at end of string" $ do
            let result = streamEdit (string "foo") (const "bar") "start foo"
            result `shouldBe` "start bar"

        it "handles replacement with different length" $ do
            let result = streamEdit (string "x") (const "LONG") "axb"
            result `shouldBe` "aLONGb"

        it "handles replacement with empty string (deletion)" $ do
            let result = streamEdit (string "del") (const "") "abdelcd"
            result `shouldBe` "abcd"

        it "uses the matched value in the replacement function" $ do
            let result = streamEdit digit (\c -> T.pack ['[', c, ']']) "a1b2c3"
            result `shouldBe` "a[1]b[2]c[3]"

    -- -----------------------------------------------------------------------
    -- editFirst
    -- -----------------------------------------------------------------------
    describe "editFirst" $ do
        it "replaces only the first occurrence" $ do
            let result = parse (editFirst (const "bar") (string "foo")) "" ("hello foo world foo end" :: T.Text)
            case result of
                Right r -> r `shouldBe` "hello bar world foo end"
                Left err -> expectationFailure (show err)

        it "passes through when there are no matches" $ do
            let result = parse (editFirst (const "bar") (string "foo")) "" ("hello world" :: T.Text)
            case result of
                Right r -> r `shouldBe` "hello world"
                Left err -> expectationFailure (show err)

        it "handles match at start" $ do
            let result = parse (editFirst (const "bar") (string "foo")) "" ("foo rest" :: T.Text)
            case result of
                Right r -> r `shouldBe` "bar rest"
                Left err -> expectationFailure (show err)

        it "handles empty input" $ do
            let result = parse (editFirst (const "bar") (string "foo")) "" ("" :: T.Text)
            case result of
                Right r -> r `shouldBe` ""
                Left err -> expectationFailure (show err)

    -- -----------------------------------------------------------------------
    -- findEdit
    -- -----------------------------------------------------------------------
    describe "findEdit" $ do
        it "replaces all occurrences (like streamEdit but as a parser)" $ do
            let result = parse (findEdit (const "bar") (string "foo")) "" ("one foo two foo three" :: T.Text)
            case result of
                Right r -> r `shouldBe` "one bar two bar three"
                Left err -> expectationFailure (show err)

        it "passes through when there are no matches" $ do
            let result = parse (findEdit (const "bar") (string "foo")) "" ("no match here" :: T.Text)
            case result of
                Right r -> r `shouldBe` "no match here"
                Left err -> expectationFailure (show err)

    -- -----------------------------------------------------------------------
    -- findSomeHTMLNaive
    -- -----------------------------------------------------------------------
    describe "findSomeHTMLNaive" $ do
        it "returns Just [results] when matches are found" $ do
            let result = findSomeHTMLNaive (string "abc") ("xxxabcxxxabcxxx" :: String)
            case result of
                Just xs -> length xs `shouldBe` 2
                Nothing -> expectationFailure "Expected Just but got Nothing"

        it "returns Nothing when no matches are found" $ do
            let result = findSomeHTMLNaive (string "abc") ("xxxxxx" :: String)
            result `shouldBe` Nothing

        it "returns Nothing on empty input" $ do
            let result = findSomeHTMLNaive (string "abc") ("" :: String)
            result `shouldBe` Nothing

    -- -----------------------------------------------------------------------
    -- findSomeHTML
    -- -----------------------------------------------------------------------
    describe "findSomeHTML" $ do
        it "returns Right (Just results) on matches" $ do
            let result = findSomeHTML (string "abc") ("xxxabcxxx" :: String)
            case result of
                Right (Just xs) -> do
                    length xs `shouldBe` 1
                    case xs of
                        (x : _) -> x `shouldBe` "abc"
                        [] -> expectationFailure "Expected non-empty list"
                Right Nothing -> expectationFailure "Expected Just but got Nothing"
                Left err -> expectationFailure (show err)

        it "returns Right Nothing when no matches" $ do
            let result = findSomeHTML (string "abc") ("xxxxxx" :: String)
            case result of
                Right Nothing -> pure ()
                Right (Just _) -> expectationFailure "Expected Nothing but got Just"
                Left err -> expectationFailure (show err)

        it "returns Right Nothing on empty input" $ do
            let result = findSomeHTML (string "abc") ("" :: String)
            case result of
                Right Nothing -> pure ()
                Right (Just _) -> expectationFailure "Expected Nothing but got Just"
                Left err -> expectationFailure (show err)

    -- -----------------------------------------------------------------------
    -- baseParser
    -- -----------------------------------------------------------------------
    describe "baseParser" $ do
        it "wraps a successful parse in Right" $ do
            result <- shouldParse (baseParser (string "abc")) "abc"
            case result of
                Right v -> v `shouldBe` "abc"
                Left _ -> expectationFailure "Expected Right"

        it "fails when the inner parser fails (no Left wrapping)" $ do
            shouldFailOn (baseParser (string "abc")) "xyz"

    -- -----------------------------------------------------------------------
    -- givesNothing
    -- -----------------------------------------------------------------------
    describe "givesNothing" $ do
        it "consumes one character and returns Left NonMatch" $ do
            result <- shouldParse (givesNothing :: ParsecT String () Identity (Either ScrapeFail String)) "x"
            expectLeftNonMatch result

        it "fails on empty input (no char to consume)" $ do
            shouldFailOn (givesNothing :: ParsecT String () Identity (Either ScrapeFail String)) ""

    -- -----------------------------------------------------------------------
    -- endStream
    -- -----------------------------------------------------------------------
    describe "endStream" $ do
        it "returns Left Eof at end of input" $ do
            -- We need to consume input first so we are at eof.
            -- Or simply provide empty input.
            result <- shouldParse (endStream :: ParsecT String () Identity (Either ScrapeFail String)) ""
            expectLeftEof result

        it "fails when input remains" $ do
            shouldFailOn (endStream :: ParsecT String () Identity (Either ScrapeFail String)) "x"

    -- -----------------------------------------------------------------------
    -- StreamEditCase constructors
    -- -----------------------------------------------------------------------
    describe "StreamEditCase" $ do
        it "EOF constructor exists" $ do
            let _ = EOF
            pure () :: Expectation

        it "Carry constructor holds a Char" $ do
            let c = Carry 'x'
            case c of
                Carry ch -> ch `shouldBe` 'x'
                _ -> expectationFailure "Expected Carry"

        it "Edit constructor holds a String" $ do
            let e = Edit "hello"
            case e of
                Edit s -> s `shouldBe` "hello"
                _ -> expectationFailure "Expected Edit"

    -- -----------------------------------------------------------------------
    -- findIO / findNaiveIO (IO-based variants)
    -- -----------------------------------------------------------------------
    describe "findIO" $ do
        it "finds matches (IO variant)" $ do
            result <- runParserT (findIO (string "abc")) () "" ("xxxabcxxx" :: String)
            case result of
                Right rs -> countRights rs `shouldBe` 1
                Left err -> expectationFailure (show err)

        it "returns empty list on empty input (IO variant)" $ do
            result <- runParserT (findIO (string "abc")) () "" ("" :: String)
            case result of
                Right rs -> length rs `shouldBe` 0
                Left err -> expectationFailure (show err)

    describe "findNaiveIO" $ do
        it "returns Just [results] on matches (IO variant)" $ do
            result <- runParserT (findNaiveIO (string "abc")) () "" ("xxxabcxxxabcxxx" :: String)
            case result of
                Right (Just xs) -> length xs `shouldBe` 2
                Right Nothing -> expectationFailure "Expected Just but got Nothing"
                Left err -> expectationFailure (show err)

        it "returns Nothing when no matches (IO variant)" $ do
            result <- runParserT (findNaiveIO (string "abc")) () "" ("xxxxxx" :: String)
            case result of
                Right Nothing -> pure ()
                Right (Just _) -> expectationFailure "Expected Nothing but got Just"
                Left err -> expectationFailure (show err)

    -- -----------------------------------------------------------------------
    -- Undefined / unimplemented functions
    -- -----------------------------------------------------------------------
    describe "findSequential (undefined)" $ do
        it "is undefined" $ do
            -- findSequential is a function whose body is undefined;
            -- applying it and forcing the result triggers the error.
            result <- try (evaluate (findSequential [] :: ParsecT String () Identity [Either ScrapeFail String])) :: IO (Either ErrorCall (ParsecT String () Identity [Either ScrapeFail String]))
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected undefined but got a value"

    describe "findAllBetween (undefined)" $ do
        it "is undefined" $ do
            shouldBeUndefined (findAllBetween :: ())

    describe "buildSequentialElemsParser (undefined)" $ do
        it "is undefined" $ do
            result <- try (evaluate (buildSequentialElemsParser :: ParsecT String () Identity [String])) :: IO (Either ErrorCall (ParsecT String () Identity [String]))
            case result of
                Left _ -> pure ()
                Right _ -> expectationFailure "Expected undefined but got a value"
