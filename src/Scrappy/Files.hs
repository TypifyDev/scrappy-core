module Scrappy.Files where

import Scrappy.Types
import Scrappy.Scrape
import Text.Parsec
import Scrappy.Elem
import qualified Data.Map.Strict as Map
import Control.Monad 
import Data.Map.Strict (Map,keys)
import Data.List (foldl')
import System.FilePath
import System.Directory

-- TEMPORARY: move to own package when im not lazy



-- | Recursively lists all files in a directory, returning absolute file paths.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
    contents <- listDirectory dir         -- Get directory contents
    paths <- forM contents $ \name -> do
        let fullPath = dir </> name        -- Create full path
        isDir <- doesDirectoryExist fullPath
        if isDir
            then listFilesRecursive fullPath  -- Recursively search subdirectories
            else do
                absPath <- makeAbsolute fullPath  -- Get absolute path in IO context
                return [absPath]                 -- Wrap in list for concatenation
    return (concat paths)  -- Flatten list of lists


searchFile :: ScraperT a -> FilePath -> IO Bool
searchFile p fp = do
  str <- readFile fp
  pure $ exists p str

searchStrFile :: String -> FilePath -> IO Bool
searchStrFile s fp = searchFile (string "s") fp 
  
searchManyFile :: [String] -> FilePath -> IO (Map String Int)
searchManyFile strs fp = do
  file <- readFile fp
  case scrape (buildElemsOpts strs) file of
    Nothing -> pure mempty
    Just results -> pure $ countOccurrences results

-- | Function to count occurrences of each unique string in a list
countOccurrences :: [String] -> Map String Int
countOccurrences = foldl' (\acc word -> Map.insertWith (+) word 1 acc) Map.empty



areFilesUsed :: FilePath -> FilePath -> IO ()
areFilesUsed sourceDir usageDir = do
  sources <- listFilesRecursive sourceDir
  searchFiles <- listFilesRecursive usageDir
  let sources' = takeFileName <$> sources
  maps <- mapM (\x -> searchManyFile sources' x) searchFiles
  let mapped = mconcat maps
  print mapped

  print "---"

  print $ filter (\s -> not $ elem s (keys mapped)) sources'
