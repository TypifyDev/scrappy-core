{-# LANGUAGE KindSignatures #-}

module Scrappy.Types where

-- may change Types + Links -> Navigation + Something else

import Text.Parsec (ParsecT, parserZero)
--import Witherable


-- -- | Upgrade an error to discard parser 
-- instance Filterable (ParsecT s u f) where
--   mapMaybe f ma = do
--     x <- ma
--     case f x of
--       Just a -> return a 
--       Nothing -> parserZero



mapMaybe :: (a -> Maybe b) -> ParsecT s u m a -> ParsecT s u m b
mapMaybe f ma = do
  x <- ma
  case f x of
    Just a -> pure a
    Nothing -> parserZero



data ScrapeFail = Eof | NonMatch deriving Show

-- | Note: both elemParser and treeElemParser are capable of doing greedy or non-greedy matching
  --treeElemParser (unless its really slow) should be better for non-greedy/focused
  --elemParser should be better for greedy



-- eitherP :: Alternative m => m a -> m b -> m (Either a b)
-- eitherP a b = (Left <$> a) <|> (Right <$> b)



  

-- IO for requests
-- Either for high level important errors
-- Maybe for Naive scraping logic 


-- State looking important for managing status of each site as well
-- as scrape coin 

-- |  data Processor a b = Processor ThreadId { runFunc :: (a -> b) }
type Html = String -- or could be just the pdf, but maybe even URL for storage sake --> could become research graph
                   -- but in a sense, would be a forest of uncited (yet) publicationsop
