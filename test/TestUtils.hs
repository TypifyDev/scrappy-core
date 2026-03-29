module TestUtils (
    loadFixture,
    fixturesDir,
    parseSucceeds,
    parseResult,
    parseTextSucceeds,
    parseTextResult,
    shouldBeUndefined,
) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import System.FilePath ((</>))
import Test.Hspec (Expectation, expectationFailure)
import Text.Parsec (ParseError, ParsecT, parse)

-- | Directory containing HTML fixture files
fixturesDir :: FilePath
fixturesDir = "test/Fixtures"

-- | Load an HTML fixture file
loadFixture :: FilePath -> IO String
loadFixture name = readFile (fixturesDir </> name)

-- | Check if a parser succeeds on the given input
parseSucceeds :: ParsecT String () Identity a -> String -> Bool
parseSucceeds p input = case parse p "" input of
    Right _ -> True
    Left _ -> False

-- | Get the parse result or error
parseResult :: ParsecT String () Identity a -> String -> Either ParseError a
parseResult p input = parse p "" input

-- | Check if a Text parser succeeds on the given input
parseTextSucceeds :: ParsecT T.Text () Identity a -> T.Text -> Bool
parseTextSucceeds p input = case parse p "" input of
    Right _ -> True
    Left _ -> False

-- | Get a Text parser result or error
parseTextResult :: ParsecT T.Text () Identity a -> T.Text -> Either ParseError a
parseTextResult p input = parse p "" input

-- | Assert that evaluating a value throws (for testing undefined functions)
shouldBeUndefined :: a -> Expectation
shouldBeUndefined x = do
    result <- try (evaluate (x `seq` ())) :: IO (Either ErrorCall ())
    case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected undefined but got a value"
