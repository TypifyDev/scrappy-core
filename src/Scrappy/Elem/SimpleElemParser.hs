{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Scrappy.Elem.SimpleElemParser where

import Scrappy.Elem.Types

import Scrappy.Elem.ElemHeadParse (parseOpeningTag, parseOpeningTagWhere)
import Scrappy.Links (LastUrl)

import Scrappy.Types -- for witherable instance

import Control.Monad (when)  
import Control.Applicative (Alternative, liftA2, some, (<|>))
--import Witherable (mapMaybe)
--import Text.Megaparsec as MParsec (eitherP, some, manyTill)
import Text.Parsec (ParsecT, Stream, string, try, parserZero, anyChar, char, optional, anyToken, parserFail
                   , many, space, manyTill)
import Text.URI (URI, render)
import Data.Text (Text, unpack)
import Data.Map (Map, toList)
import Data.Maybe (fromMaybe)

import Control.Monad.IO.Class

-- TODO(galen): antiElemParser --- inner matches must be 0 ... maybe doesnt match any parameter 


eitherP :: Alternative m => m a -> m b -> m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)

manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go


-- | Try to cut out Megaparsec for now - get direct export from Control.Applicative

-- | Note: could make class HtmlP where { el :: a -> Elem, attrs :: a -> Attrs, innerText :: a -> Text } 

-- | A use-case/problem is popping up as I code:
 -- if elem 'a' contains elem 'a'
    -- then do what?
    -- 1) Restrict to identifying in parent only if not in some inner same element
    -- 2) Get all in parent element regardless
    -- 3) Consider being inside of same element a fail -> then get inner-same element
      -- like 1) but seeks to carry minimal data around it / more honed in



-- | Simplest interface to building element patterns 
el :: Stream s m Char => Elem -> [(String, String)] -> ParsecT s u m (Elem' String)
el element attrss = elemParser (Just (element:[])) Nothing ((fmap . fmap) Just attrss)



-- | Generic interface for building Html element patterns where we do not differentiate based on whats inside
-- | for control of allowable inner html patterns, see ChainHTML and/or TreeElemParser  
elemParser :: (ShowHTML a, Stream s m Char) =>
              Maybe [Elem]
           -> Maybe (ParsecT s u m a)
           -> [(String, Maybe String)]
           -> ParsecT s u m (Elem' a)
elemParser elemList innerSpec attrs = do
  -- liftIO $ print "hey"
  (elem', attrs') <- parseOpeningTag elemList attrs 
  -- we should now read the elem' to see if in list of self-closing tags
  -- TODO(galen): What about when the self closing tag actually doesnt?
  case elem elem' selfClosing of
    True -> do
      (try (string ">") <|> string "/>")
      case innerSpec of
        Nothing -> return $ Elem' elem' attrs' mempty mempty 
        Just _ -> parserZero 
    False -> do
      (asString, matches) <- fmap (foldr foldFuncTup mempty)  -- this cant be where we do "/>" if we parse ">" in parseOpeningTag
        $ (try (string "/>") >> return [])
        <|> (try $ innerElemParser elem' innerSpec) -- need to be sure that we have exhausted looking for an end tag, then we can do the following safely
        <|> (selfClosingTextful innerSpec)
      return $ Elem' elem' attrs' matches asString


-- | Generic interface for building Html element patterns where we do not differentiate based on whats inside
-- | for control of allowable inner html patterns, see ChainHTML and/or TreeElemParser  
elemParserWhere :: (ShowHTML a, Stream s m Char) =>
                   Maybe [Elem]
                -> Maybe (ParsecT s u m a)
                -> String -> (String -> Bool) -- GOAL: -> [(String, String -> Bool)]
                -- ^ An attr and a predicate
                -> ParsecT s u m (Elem' a)
elemParserWhere elemList innerSpec attr pred = do
  (elem', attrs') <- parseOpeningTagWhere elemList attr pred
  -- we should now read the elem' to see if in list of self-closing tags
  case elem elem' selfClosing of
    True -> do
      (try (string ">") <|> string "/>")
      case innerSpec of
        Nothing -> return $ Elem' elem' attrs' mempty mempty 
        Just _ -> parserZero 
    False -> do
      (asString, matches) <- fmap (foldr foldFuncTup mempty)  -- this cant be where we do "/>" if we parse ">" in parseOpeningTag
        $ (try (string "/>") >> return [])
        <|> (try $ innerElemParser elem' innerSpec) -- need to be sure that we have exhausted looking for an end tag, then we can do the following safely
        <|> (selfClosingTextful innerSpec)
      return $ Elem' elem' attrs' matches asString




clickableHref :: Stream s m Char => Bool -> LastUrl -> ParsecT s u m Clickable
clickableHref booly cUrl = do
  elA <- parseOpeningTag Nothing [("href", Nothing)]
  href <- mapMaybe (getHrefAttrs booly cUrl) (pure $ snd elA) 
  return $ Clickable elA href


-- eg clickable' (string "download") 
clickableHref' :: (Stream s m Char, ShowHTML a) =>
                  ParsecT s u m a
               -> Bool
               -> LastUrl
               -> ParsecT s u m Clickable 
clickableHref' innerPat booly cUrl = do
  e <- elemParser Nothing (Just $ innerPat) [("href", Nothing)]
  href <- mapMaybe (getHrefEl booly cUrl) (pure e)
  return $ Clickable (elTag e, attrs e) href

  
-- instance Monad Elem' where 

sameElTag :: (ShowHTML a, Stream s m Char) => Elem -> Maybe (ParsecT s u m a) -> ParsecT s u m (Elem' a)
sameElTag elem parser = elemParser (Just [elem]) parser []
  
  -- innerMatches el 
  -- return $ (elemToStr el, Match $ innerMatches el)  -- allowed to return a String or Match a

-- future concern for foldFuncMatchlist where Elem ~~ [] ; both of kind * -> *
matchesInSameElTag :: (ShowHTML a, Stream s m Char) => Elem -> Maybe (ParsecT s u m a) -> ParsecT s u m [a]
matchesInSameElTag elem parser = do
  el <- elemParser (Just [elem]) parser [] 
  return $ (matches' el)  -- allowed to return a String or Match a

-- | Might be worth it to do again with findNextMatch func
  -- this would open up ability to return multiple matches inside of a given element
  -- would need to retain ability to handle 3Cases{ self-closing(2 { /> or > .. eof}) | match | no match
  -- in case of no match { self-closing || simply, no match } -> needs to throw parserZero 

-- | findNextMatch already handles case of Eof
  -- would be re-definition of baseParser in `let`


-- the below class functions give rise to concept:
-- findInRecursive :: IsHTMLRep a => (Parser1, Parser2, Parser3, ...) -> Element a

-- where Element a is meant to represent anything that fits IsHTMLRep and has some number of summarized data
-- from the html

-- and is meant to work with a MessyTree ~ String in a context-enabled manner

-- this could in theory be as abstract as possible

-- | Case 1:

-- (ParserLevel1 a, ParserLevel2 b) ~ up to 2 levels specified ~ up to 2 patterns
  --  Given find functions, this can start at any arbitrary next index

-- | Case 2:

-- [ParserSomeLevel] ~ up to n levels specified, 1 possible pattern'

-- | Case 3: The most general

-- [Show a => forall a. a] ~ up to n levels specified, n possible patterns 


-- class MonoidFold a where
  -- foldMon :: [a] -> b
  -- newEmpty :: b

-- parseHtmlMatcher -> foldtoITR + opening tag -> elem 


-- | Maybe elemParser can be abstracted to be a class function

-- | Does this work with parser meant to take up whole inner?
  -- I suppose it would but this would allow other stuff
  -- that case is handled by treeElemParserSpecific

-- {-# DEPRECATED elemParser "needs logic for anyTagInner for text-ful, self-closing tags"  #-}
-- elemParser :: (InnerHTMLRep Elem' InnerTextResult a, ShowHTML a, Stream s m Char) =>
--               Maybe [Elem]
--            -> Maybe (ParsecT s u m a)
--            -> [(String, Maybe String)]
--            -> ParsecT s u m (Elem' a)
-- elemParser elemList innerSpec attrs = do
--   (elem', attrs') <- parseOpeningTag elemList attrs
--   let
--     parser = char '>'
--              >> manyTill (Match <$> (fromMaybe parserZero innerSpec)
--                           <|> Element <$> sameElTag elem' innerSpec
--                           <|> ((IText . (:[])) <$> anyChar)) (endTag elem')
                          

--   innerH <- (try (string "/>") >> return []) <|> (try parser) <|> (selfClosingTextful innerSpec)
--   let itr = foldHtmlMatcher innerH 
--   case length $ matches itr of
--     0 ->
--       case innerSpec of
--         Nothing -> return $ Elem' elem' attrs' (matches itr) (innerText itr)
--         _ -> parserZero
--     _ -> return $ Elem' elem' attrs' (matches itr) (innerText itr)

-- if innerSpec == Nothing
      -- then return $ Elem' elem' attrs' (matches itr) (innerText itr)
      -- else parserZero
  
    {- endTag is currently in TreeElemParser -}
    
-- selfClosingTextful :: ParsecT s u m a
-- selfClosingTextful = do
--   -

-- foldHtmlMatcherToTrup :: [HTMLMatcher e a] -> ([a], 
 
-- {-# DEPRECATED elemParser "needs logic for anyTagInner for text-ful, self-closing tags"  #-}

-- elemParser :: (ShowHTML a, Stream s m Char) =>
--               Maybe [Elem]
--            -> Maybe (ParsecT s u m a)
--            -> [(String, Maybe String)]
--            -> ParsecT s u m (Elem' a)
-- elemParser elemList innerSpec attrs = do
--   let
--     parser eTag = char '>'
--                    >> manyTill (Match <$> (fromMaybe parserZero innerSpec)
--                                 <|> Element <$> sameElTag eTag innerSpec
--                                 <|> ((IText . (:[])) <$> anyChar)) (endTag eTag)
--     required = case innerSpec of
--                  { Nothing -> 0; _ -> 1 }
--     enoughMatches e a (asString, matches) = 
--       if required > (length matches)
--       then return $ Elem' e a matches asString
--       else parserZero
                    
--   (elem', attrs') <- parseOpeningTag elemList attrs
--   innerH <- fmap (foldr foldFuncTup mempty) 
--             $ (try (string "/>") >> return [])
--             <|> (try $ parser elem')
--             <|> (selfClosingTextful innerSpec)
--   enoughMatches elem' attrs' innerH

selfClosing :: [String]
selfClosing = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

elSelfC :: Stream s m Char => Maybe [Elem] -> [(String, Maybe String)] -> ParsecT s u m (Elem' a)
elSelfC elemOpts attrsSubset = do
  (tag, attrs) <- parseOpeningTag elemOpts attrsSubset
  return $ Elem' tag attrs mempty mempty 

elSelfClosing :: Stream s m Char => Maybe [Elem] -> Maybe (ParsecT s u m a) -> [(String, Maybe String)] -> ParsecT s u m (Elem' a)
elSelfClosing elemOpts innerSpec attrsSubset = do
  (tag, attrs) <- parseOpeningTag elemOpts attrsSubset
  case innerSpec of
    Just _ -> parserZero
    Nothing -> return $ Elem' tag attrs mempty mempty 

elemWithBody :: (ShowHTML a, Stream s m Char) =>
              Maybe [Elem]
           -> Maybe (ParsecT s u m a)
           -> [(String, Maybe String)]
           -> ParsecT s u m (Elem' a)
elemWithBody elemList innerSpec attrs = do
  e <- elemParserInternal elemList innerSpec attrs
  when (length (matches' e) < (case innerSpec of { Nothing -> 0; _ -> 1 })) (parserFail "not enough matches")
  return e
  
elemParserInternal :: (ShowHTML a, Stream s m Char) =>
              Maybe [Elem]
           -> Maybe (ParsecT s u m a)
           -> [(String, Maybe String)]
           -> ParsecT s u m (Elem' a)
elemParserInternal elemList innerSpec attrs = do
  (elem', attrs') <- parseOpeningTag elemList attrs
  -- we should now read the elem' to see if in list of self-closing tags
  -- case elem elem' selfClosing of
    -- True -> return Elem' elem' attrs' 
  (asString, matches) <- fmap (foldr foldFuncTup mempty)  -- this cant be where we do "/>" if we parse ">" in parseOpeningTag
    $ (try (string "/>") >> return [])
    <|> (try $ innerElemParser elem' innerSpec) -- need to be sure that we have exhausted looking for an end tag, then we can do the following safely
    <|> (selfClosingTextful innerSpec)
  return $ Elem' elem' attrs' matches asString


-- elemParserInternalV2 :: (ShowHTML a, Stream s m Char) =>
--               Maybe [Elem]
--            -> Maybe (ParsecT s u m a)
--            -> [(String, Maybe String)]
--            -> ParsecT s u m (Elem' a)
-- elemParserInternalV2 elemList innerSpec attrs = do
--   (elem', attrs') <- parseOpeningTag elemList attrs
--   -- we should now read the elem' to see if in list of self-closing tags
--   case elem elem' selfClosing of
--     True -> (try string ">" <|> string "/>") >> return Elem' elem' attrs' mempty mempty 
--     False -> do
--       (asString, matches) <- fmap (foldr foldFuncTup mempty)  -- this cant be where we do "/>" if we parse ">" in parseOpeningTag
--         $ (try (string "/>") >> return [])
--         <|> (try $ innerElemParser elem' innerSpec) -- need to be sure that we have exhausted looking for an end tag, then we can do the following safely
--         <|> (selfClosingTextful innerSpec)
--       return $ Elem' elem' attrs' matches (reverse asString)


  
-- elemParser :: (ShowHTML a, Stream s m Char) =>
--               Maybe [Elem]
--            -> Maybe (ParsecT s u m a)
--            -> [(String, Maybe String)]
--            -> ParsecT s u m (Elem' a)
-- elemParser elemList innerSpec attrs = do
--   -- let required = case innerSpec of { Nothing -> 0; _ -> 1 }
--   (elem', attrs') <- parseOpeningTag elemList attrs
--   innerH <- fmap (foldr foldFuncTup mempty)
--             -- this cant be where we do "/>" if we parse ">" in parseOpeningTag
--             $ (try (string "/>") >> return [])
--             <|> (try $ innerElemParser elem' innerSpec)
--             -- need to be sure that we have exhausted looking for an end tag
--             -- then we can do the following safely
--             <|> (selfClosingTextful innerSpec)
--   enoughMatches 0 elem' attrs' innerH


innerElemParser :: (ShowHTML a, Stream s m Char) =>
                   String
                -> Maybe (ParsecT s u m a)
                -> ParsecT s u m [HTMLMatcher Elem' a]
innerElemParser eTag innerSpec = char '>'
                                 >> manyTill (try (Match <$> (fromMaybe parserZero innerSpec))
                                              <|> (try (IText <$> stylingElem)) -- this line is new/unstable
                                              <|> try (Element <$> sameElTag eTag innerSpec)
                                              <|> ((IText . (:[])) <$> anyChar)) (endTag eTag)
                                              

-- Doesnt change the structure of the page at all just how text is styled like MS word stuff
stylingTags = ["abbr", "b", "big", "acronym", "dfn", "em", "font", "i", "mark", "q", "small", "strong"]

-- | Just gives the inners 
stylingElem :: Stream s m Char => ParsecT s u m String 
stylingElem = do
  (e,_) <- parseOpeningTag (Just stylingTags) []
  char '>'
  fmap fst $ manyTill_ anyChar (endTag e)
  
-- f :: ([a], [b], [c]) -> Elem' a
-- f (x,y,z) = f' x y z

-- f' el attrs = Elem' el attrs

    

--   case length $ matches itr of
--     0 ->
--       case innerSpec of
--         Nothing -> return $ Elem' elem' attrs' (matches itr) (innerText itr)
--         _ -> parserZero
--     _ -> return $ Elem' elem' attrs' (matches itr) (innerText itr)

   
-- -- this should be a success but does not allow 


-- baseInnerParser :: Stream s m Char =>
--                    Maybe (ParsecT s u m a)
--                 -> ParsecT s u m String
--                 -> ParsecT s u m (InnerTextResult a)
-- baseInnerParser innerPat endParse = do
--       _ <- char '>'

--       x :: [Inner a] <- manyTill_ (Match $ match <|> El <$> sameElTag <|> NonMatch anyChar) endParse

--       foldHtmlMatcher x
      
      
      -- (pre, patternFound) <- manyTill_ (try sameElTag <|> p) (fromMaybe anyChar innerPat)
      -- (post, _) <- manyTill_  (try sameElTag <|> p) endParse

      -- return $ InnerTextResult { match = patternFound
      --                          , fullInner = mconcat pre <> patternFound <> mconcat post }



-- -- | Gets own subsets (eg div if outer = div)
-- -- | Monoid may need to be implemented so that we can have mempty to help generalize
-- parseInnerHTMLAndEndTag2 :: (ToHTML a, Stream s m Char) => Elem -> Maybe (ParsecT s u m a) -> ParsecT s u m (InnerTextResult a)
-- parseInnerHTMLAndEndTag2 elem innerPattern = do
--   let
--     p :: Stream s m Char => ParsecT s u m (Inner a) 
--     p = do
--       a <- anyChar
--       return (NonMatch $ a : [])
      
--     -- what does anyTagInner do?
--     -- should it instead be 
--     anyTagInner :: Stream s m Char => Maybe (ParsecT s u m String) -> ParsecT s u m (InnerTextResult a)
--     anyTagInner innerP = baseParser innerP (try (char '<'
--                                                   >> (optional (char '/'))
--                                                   >> MParsec.some anyChar -- end tag 
--                                                   >> (string " " <|> string ">")))

--     normal :: Stream s m Char => Maybe (ParsecT s u m String) -> ParsecT s u m (InnerTextResult a)
--     normal innerP = baseParser innerP (try (string ("</" <> elem <> ">")))
      
--   x <- Match <$> normal <|> NonMatch <$> 

--   baseInnerParser
--   _ <- char '>'
--   (pre, patternFound) <- manyTill_ (try (sameElTag elem) <|> p) (fromMaybe anyChar innerPattern)
--   (post, _) <- manyTill_  (try sameElTag <|> p) endParse

--   return $ InnerTextResult { match = patternFound
--                            , fullInner = mconcat pre <> patternFound <> mconcat post }

  
  
--   x <- MParsec.eitherP (try (string "/>")) (normal innerPattern <|> anyTagInner innerPattern)
--   case x of
--      Left  a -> -- was "/>" 
--        case innerPattern of
--          Just a -> parserZero
--          Nothing ->
--            pure InnerTextResult { match = "", fullInner = "" }

--      --was not "/>"
--        -- but i believe could still be
--          -- > implicit self-closing tag
--          -- > fail 
         
--      Right b -> return b

-- | Does not get subsets, gets most inner (Elem <-> Match) combo
-- | Monoid may need to be implemented so that we can have mempty to help generalize
{-# DEPRECATED parseInnerHTMLAndEndTag "use new elem parser directly" #-}
parseInnerHTMLAndEndTag :: (Stream s m Char) =>
                           Elem
                        -> Maybe (ParsecT s u m String)
                        -> ParsecT s u m (InnerTextResult String)
parseInnerHTMLAndEndTag elem innerPattern = do

  let f :: Stream s m Char => Maybe (ParsecT s u m String) -> ParsecT s u m String
      f x = case x of
              Just pat -> pat
              Nothing -> string ""

      sameElTag :: Stream s m Char => ParsecT s u m String
      sameElTag = do
        el <- elemParserOld (Just [elem]) Nothing []
        return $ showH el

      p :: Stream s m Char => ParsecT s u m String 
      p = do
        a <- anyToken
        return (a : [])

      baseParser :: Stream s m Char => Maybe (ParsecT s u m String) -> ParsecT s u m String -> ParsecT s u m (InnerTextResult String)
      baseParser innerPat endParse = do
        _ <- char '>'
        (pre, patternFound) <- manyTill_ (try sameElTag <|> p) (f innerPat)
        (post, _) <- manyTill_  (try sameElTag <|> p) endParse

        return $ InnerTextResult { _matchesITR = [patternFound]
                                 , _fullInner = mconcat pre <> patternFound <> mconcat post }
  
      anyTagInner :: Stream s m Char => Maybe (ParsecT s u m String) -> ParsecT s u m (InnerTextResult String)
      anyTagInner innerP = baseParser innerP (try (char '<'
                                                   >> (optional (char '/'))
----------------------------------------------------DOES THIS ACTUALLY WORK BELOW?--------------------
                                                   >> some anyChar -- end tag 
                                                   >> (string " " <|> string ">")))

      normal :: Stream s m Char => Maybe (ParsecT s u m String) -> ParsecT s u m (InnerTextResult String)
      normal innerP = baseParser innerP (try (string ("</" <> elem <> ">")))
      

  x <- eitherP (try (string "/>")) (normal innerPattern <|> anyTagInner innerPattern)
  case x of
     Left  a ->
       case innerPattern of
         Just a -> parserZero
         Nothing ->
           pure InnerTextResult { _matchesITR = [], _fullInner = "" }
         
     Right b -> return b
     
  -- Note: we can parse these better with eitherP
  -- eitherP :: Alternative m => m a -> m b -> m (Either a b)

      -- then use case statement to deal with case (A: sub-elem | B: anychar)
        --if sub-elem -> put in list --then--> 
  
  -- (pre, patternFound) <- manyTill_ (try sameElTag <|> p) (f innerPattern)
  -- (post, _) <- manyTill_  (try sameElTag <|> p) (try (string ("</" <> elem <> ">")))

  -- return $ InnerTextResult { match = patternFound
  --                          , fullInner = mconcat pre <> patternFound <> mconcat post }


-- | Note: In case of Nothing for innerSpec, the parser should be : optional anyChar == () 

-- Note: this can be passed to findAll func in megaparsec as is
-- |          attrs (Attr | AnyAttr)   maybe discr elem
{-# DEPRECATED elemParserOld "use elemParser" #-}
elemParserOld :: (Stream s m Char) =>
              Maybe [Elem]
           -> Maybe (ParsecT s u m String)
           -> [(String, Maybe String)]
           -> ParsecT s u m (Elem' String)
elemParserOld elemList innerSpec attrs = do
  (elem', attrs') <- parseOpeningTag elemList attrs
  --note that at this point, there is a set elem' to match  
  inner <- parseInnerHTMLAndEndTag elem' innerSpec
  return $ Elem' elem' attrs' (_matchesITR inner) (_fullInner inner)
-- | Attrs should really be returned as a map

-- | note that this could even be used for mining text on page
-- | eg. search with innerSpec as "the" or other common articles 

-- either begin parsing the element or href and find that it is what youre looking for
-- OR -> skip to start of next element
 
-- | Note: how do we get a computer to recognize, Boolean-ly if some representation is x?
-- | where x is some statement?

  --  As humans we do this visually : is this object red? yes or no

  --  A computer must do the same with "local vision" that theoretically must be as parsing algorithm
  --  aka Bool `followedby` Bool2 `followedBy` Bool3 ... Bool_n
  
    --  where some Bools are of set-wise funcs and some are singleton-options AKA / therfore some
    --        sequential combo of (Set 1 of Any <-> Set (totalCount) of Any)

    --  Therefore -> this logic generalizes processing to both humans and computers,; the full set of
    --  higher level processors 





