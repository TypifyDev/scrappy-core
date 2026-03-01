{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Scrappy.Elem.ChainHTML where



import Scrappy.Find (findNaive)
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Elem.ElemHeadParse (parseOpeningTag)
import Scrappy.Elem.Types (Elem', ShowHTML, ElemHead, Elem, innerText'
                  , matches')

import Text.Parsec (ParsecT, Stream, char, (<|>), many, parserFail, parse, parserZero, string, optional)
import Control.Applicative (some)
import Data.Functor.Identity (Identity)
import Data.Maybe (listToMaybe)
-- functions for chaining free-range html patterns based on the previous
-- patterns to allow for maximum flexibility 

nl :: Stream s m Char => ParsecT s u m ()
nl = optional (many $ (char '\n' <|> char ' '))

manyHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
manyHtml p = many $ p <* nl

someHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
someHtml p = some $ p <* nl

manyTillHtml_ :: Stream s m Char => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTillHtml_ p end = manyTill_ (p <* nl) end 


htmlTag :: Stream s m Char => ParsecT s u m ElemHead
htmlTag = parseOpeningTag (Just ["html"]) [] 




manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go



clean :: String -> String
clean = undefined -- drop if == ( \n | \" | '\\' )


-- -- same site is guranteed
-- allLinks :: String -> ParsecT String () Identity [String] 
-- allLinks baseUrl = do
--   x <- findNaive hrefParser 
--   return $ case x of
--     Just (x':xs') -> catMaybes $ fmap (maybeUsefulUrl baseUrl) (x':xs')
--     Just [] -> []
--     Nothing -> [] 



mustContain :: ParsecT s u m (Elem' a) -> Int -> ParsecT s u m b -> ParsecT s u m (Elem' a)
mustContain e count _pat = do
  out <- e
  case parse (findNaive $ string "Search") "" (innerText' out) of
    Right (Just xs) -> if count > length xs then parserZero else return out
    _ -> parserZero
    
-- | An elem head configures the bracketing so this is all we need for
-- | crafting

type Shell =  (Elem, [(String, Maybe String)]) 

-- incomplete
contains'' :: (Stream s m Char, ShowHTML a) => Shell 
           -> ParsecT s u m a
           -> ParsecT s u m [a]
contains'' (e,as) p = matches' <$> elemParser (Just [e]) (Just p) as

parseInShell :: ParsecT s u m (Elem' a) -> ParsecT String () Identity b -> ParsecT s u m b
parseInShell = contains 

-- | This will be fully removed in the future
-- | 99% of the time this is gonna be desired to pair with findNaive 
{-# DEPRECATED contains "this should have been called parseInShell from the start, you probably want contains' or containsFirst" #-}
contains :: ParsecT s u m (Elem' a) -> ParsecT String () Identity b -> ParsecT s u m b
contains shell b = do
  x <- shell

  let
    ridNL p = (many (char ' ' <|> char '\n')) >> p 
  
  -- need to skip 
  case parse (ridNL b) "" (innerText' x) of
    Right match -> return match
    Left err -> parserFail (show err)

-- | finds multiple matches anywhere inside the passed elem
-- | This function is also quite extensible because when used with `scrape`
-- | this combo will return a list of list of elems where the hierarchy of HTML has been preserved
-- | but a great deal of information has been filtered out. An example use case would be knowing that
-- | you want <p> tags from a Set of very specific shells, this could allow you to analyze what and how many
-- | came from each shell.
-- | This also naturally extends to running this same scraper on multiple pages which would allow you to recover
-- | ample details on the number of match_A in shell_S on Page_P ~~ [[[MatchA]]] and this Match can be any
-- | arbitarily defined type. You can further imagine pairing with NLP analysis but this is a long enough point here.
containsMany, contains'
  :: ShowHTML a =>
     ParsecT s u m (Elem' a) 
  -> ParsecT String () Identity b
  -> ParsecT s u m [b]
containsMany = contains' -- legacy
contains' shell b = do
  x <- shell
  case parse (findNaive b) "" (innerText' x) of
    Right (Just matches) -> pure matches
    Left err -> parserFail (show err)
    Right Nothing -> parserFail "no matches in this container" 

containsFirst :: ShowHTML a =>
                 ParsecT s u m (Elem' a)
              -> ParsecT String () Identity b
              -> ParsecT s u m b
containsFirst shell b = do
  result <- contains' shell b
  case listToMaybe result of
    Just x -> return x
    Nothing -> parserFail "no matches in containsFirst"


sequenceHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
sequenceHtml p1 p2 = do
  x <- p1
  _ <- many (char ' ' <|> char '\n' <|> char '\t')
  y <- p2
  return (x, y)

sequenceHtml_ :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b
sequenceHtml_ p1 p2 = do
  _ <- p1
  _ <- many (char ' ' <|> char '\n' <|> char '\t')
  p2

(</>>) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m b 
(</>>) = sequenceHtml_

(</>>=) :: Stream s m Char => ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m (a, b)
(</>>=) = sequenceHtml

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- manyHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
-- manyHtml prsrHtml = (many (char ' ' <|> char '\n')) >> many (fmap snd $ manyTill_ (char ' ' <|> char '\n') prsrHtml)

-- someHtml :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
-- someHtml prsrHtml = many (char ' ' <|> char '\n') >> some prsrHtml



