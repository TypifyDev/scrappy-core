module TestUtils
  ( loadFixture
  , fixturesDir
  , parseSucceeds
  , parseResult
  ) where

import Data.Functor.Identity (Identity)
import System.FilePath ((</>))
import Text.Parsec (ParsecT, parse, ParseError)

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
  Left _  -> False

-- | Get the parse result or error
parseResult :: ParsecT String () Identity a -> String -> Either ParseError a
parseResult p input = parse p "" input
