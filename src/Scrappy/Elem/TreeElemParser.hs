{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Scrappy.Elem.TreeElemParser where

import Control.Monad.IO.Class

import Scrappy.Elem.ElemHeadParse (parseOpeningTag, hrefParser', parseOpeningTagDesc, mkAttrsDesc, attrParser)
import Scrappy.Elem.Types (Elem, Attrs, ElemHead, TreeHTML(TreeHTML), HTMLMatcher (IText, Element, Match)
                  , InnerTextHTMLTree(InnerTextHTMLTree), innerTree, innerText, matches, GroupHtml
                  , Elem', TreeIndex, attrs, elTag, ShowHTML, showH, _innerTree', matches'
                  , ElementRep, mkGH, innerText', _innerText, _matches, foldFuncTrup
                  , UrlPagination(..), enoughMatchesTree, selfClosingTextful, endTag)

import Scrappy.Elem.ChainHTML (someHtml, manyHtml, nl)
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Find (findNaive)

import Control.Monad (when)
import Control.Applicative (Alternative, liftA2, many, (<|>), some)
import Text.Parsec (Stream, ParsecT, anyChar, try, parserZero, parserFail, string, parse
                   , char, noneOf, option, space, alphaNum, notFollowedBy, (<?>), optional, manyTill)
import qualified Data.Map as Map (Map, toList, fromList, adjust) 
import Data.Graph (Tree (Node), Forest)
import Data.Tree (rootLabel)
import Text.URI as URI
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, fromJust)
import Data.List
import Data.Text (Text, splitOn)


skipManyTill :: Alternative m => m a -> m end -> m end
skipManyTill p end = go
  where
    go = end <|> (p *> go)
{-# INLINE skipManyTill #-}

-- | Note for research on Parsers/Scrapers + AI research -> if a scraper does provide only
-- | a slow method for processing (Picture -> *) that we might be able to solve this issue with
-- | either Quantum (same as current AI) or with attentive methodologies to "gamble" on what to first pay
-- | attention to , which could further be based on if Video like (frame -> frame) or single picture


manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go


data Many a = Many a | One a deriving Show

treeLookupIdx :: TreeIndex -> Forest a -> a
treeLookupIdx = undefined


-------------------------------------------------------------------------------------------------------------------
----------------------Top Level Functions-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  
-- | Like elemParser, this matches on an html element but also represents the innerHTML
-- | as a Tree ElemHead so that we can match this structure in elements further down in the DOM
-- | see groupHtml and treeElemParserSpecific 
treeElemParser :: (Stream s m Char, ShowHTML a) =>
                   Maybe [Elem]
                -> Maybe (ParsecT s u m a)
                -> [(String, Maybe String)]
                -> ParsecT s u m (TreeHTML a)
treeElemParser elemOpts matchh attrsSubset = do
  e <- treeElemParser' elemOpts matchh attrsSubset
  when (length (matches' e) < (case matchh of { Nothing -> 0; _ -> 1 })) (parserFail "not enough matches")
  return e


selfClosing :: [String]
selfClosing = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]


-- | Used by treeElemParser, is not an interface, use treeElemParser
treeElemParser' :: (Stream s m Char, ShowHTML a) =>
                  Maybe [Elem]
               -> Maybe (ParsecT s u m a)
               -> [(String, Maybe String)]
               -> ParsecT s u m (TreeHTML a)
treeElemParser'  elemOpts matchh attrsSubset = do
 (elem', attrs') <- parseOpeningTag elemOpts attrsSubset
 case elem elem' selfClosing of
   True -> do
      (try (string ">") <|> string "/>")
      case matchh of
        Nothing ->  return $ TreeHTML elem' attrs' mempty mempty []
        Just _ -> parserZero

     -- ((try string ">") <|> string "/>") >> return TreeHTML elem' attrs' mempty mempty mempty
   False -> do
     -- (inText, matchBook, treees) <- inerTreeElemParser
     rawMatchers <- (try (string "/>") >> return [])
                    <|> (try $ innerElemParser2 elem' matchh)
                    <|> (selfClosingTextful matchh)
     let (inText, matchBook, _treees) = foldr foldFuncTrup mempty rawMatchers
     return $ TreeHTML elem' attrs' matchBook inText rawMatchers

-------------------------------------------------------------------------------------------------------------------

-- | The real difference between (htmlGroup _ _ _) and specificRepetitiveForest is a matter of if we accept the next
-- | piece to be a new discovery to match on or if we are in that process of matching what we just found


-- digitEq "ere3" "ere4"
innerTreeElemParser :: (ShowHTML a, Stream s m Char) =>
                       Elem
                    -> Maybe (ParsecT s u m a)
                    -> ParsecT s u m (String, [a], [Tree ElemHead])
innerTreeElemParser elem' matchh = do
  fmap (foldr foldFuncTrup mempty)
    $ (try (string "/>") >> return [])
    <|> (try $ innerElemParser2 elem' matchh)
    <|> (selfClosingTextful matchh)
-- then i apply this to all leaves of the Map

-- !!!!!!!!!!!!!!!!!!!
-- | NOTE: In future: create function that simplifies all numbers that will be compared to their number of digits
-- 1426674 -> 1234567
-- 1834324 -> 1234567 (==) -> True 




-- htmlGroup --calls--> treeElemParser >>= (many treeSpecific --calls--> specificRepForest) 

-- | Ideal case is that we can do (Many a) struturing even if interspersed with text which would solve issues like
-- | many search terms highlighted, we then wouldnt need to know what the search term is
-- | AND!! we have already seen that it could for exmaple be: <a><b class="hiddenText">Hockey</b></a> 


-- treeElemParserSpecific' :: -> HTMLMatcher TreeHTML String
-- treeElemParserSpecific' = do
--   x <- treeElemParserSpecific
--   case innerText' x == searchTerm of
--     True -> IText (innerText' x)
--     False -> Element x

type SubTree a = [Tree a]
-- | Note: unlike other Element parsers, it does not call itself but innerParserSpecific instead loops with
-- | 
treeElemParserSpecific :: (Stream s m Char, ShowHTML a) =>
                          Maybe (ParsecT s u m a)
                       -> Elem
                       -> [(String, String)]
                       -> SubTree ElemHead
                       -> ParsecT s u m (TreeHTML a)
treeElemParserSpecific match elem' attrs' subTree = do
  (tag, attrsOut) <- parseOpeningTagDesc (Just [elem']) attrs'
  char '>'
  (matchBook, inText, _treees) <- innerParserSpecific match tag subTree
  return $ TreeHTML tag attrsOut matchBook inText []  -- TODO: propagate rawInner through innerParserSpecific

validateGPR :: [Many (Tree ElemHead)] -> ParsecT s u m [HTMLMatcher TreeHTML a]
validateGPR manyElHeads =
  if length (filter (\case {One a -> True; _ -> False}) manyElHeads) == 0 then return []
  else parserFail "promised elements not found"

-- We allow attrs to be any then check, but also avoid unnceccessarily parsing attrs
--  following needs to be inside many
-- innerParser will be from case elem tag selfClosing and on 
--  Is able to repeat / execute any pattern that returns multiple elements of same type
-- (see manyTreeElemHeadParser)


-- | Uses HTMLMatcher to collect cases of html while parsing inside of a certain element
htmlGenParserRepeat' :: (Stream s m Char, ShowHTML a) =>
                       String 
                    -> Maybe (ParsecT s u m a)
                    -> [Many (Tree ElemHead)]
                    -> ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserRepeat' elemTag match manyElHeads = {-parsesTreeHs-}
  ((endTag elemTag) >> validateGPR manyElHeads)
  <|> liftA2 (:) (fmap Match $ try (fromMaybe parserZero match)) (htmlGenParserRepeat elemTag match manyElHeads)
  <|> liftA2 (:) (fmap IText $ try stylingElem) (htmlGenParserRepeat elemTag match manyElHeads) 
  <|> try ((treeElemParserSpecificContinuous match manyElHeads)
       >>= (\(sM, a) -> fmap ((Element a):) (htmlGenParserRepeat elemTag match sM)))
  <|> liftA2 (:) ((IText . (:[])) <$> anyChar) (htmlGenParserRepeat elemTag match manyElHeads) 



-- Note: even tho this is done / works (below) we could allow for any element the entire time
-- | BUT! we need to check off our list of demanded elements so that when we parse the end tag, we can see if
-- | the elements (ordered and parsed only in order) were all found before the end tag
-- for htmlGenParserRepeat it can just change the passed state of [Many (Tree ElemHead)]


htmlGenParserRepeat :: (Stream s m Char, ShowHTML a) =>
                       String 
                    -> Maybe (ParsecT s u m a)
                    -> [Many (Tree ElemHead)]
                    -> ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserRepeat elemTag match manyElHeadss = {-parsesTreeHs-}
  case manyElHeadss of
    -- [] -> fmap ((flip (:) []) . IText . fst) (manyTill_ (htmlGenParserFlex match) (endTag elemTag))

    [] -> do
      (htMMers, _) <-  manyTill_ (htmlGenParserFlex match) (endTag elemTag)
      if length (filter (\case {Element _ -> True; _ -> False}) htMMers) == 0 -- AND loose == False 
        then
        return htMMers
        else
        parserFail "extra elements"
    --  New idea: parse this structure up until the endTag and invalidate if number of elems exceeds 0 
    
      -- fmap ((flip (:) []) . IText . (:[])) specificITextParser
      -- fmap ((flip (:) []) . IText . fst)  (manyTill_ anyChar (endTag elemTag))
    manyElHeads ->
      -- For strict matching: try expected elements first, fail on unexpected elements
      liftA2 (:) (fmap Match $ try (fromMaybe parserZero match)) (htmlGenParserRepeat elemTag match manyElHeads)
      <|> try ((treeElemParserSpecificContinuous match manyElHeads)
               >>= (\(sM, a) -> fmap ((Element a):) (htmlGenParserRepeat elemTag match sM)))
      -- Only allow text content that's not part of an opening tag
      <|> liftA2 (:) (fmap (IText . (:[])) (specificChar' elemTag)) (htmlGenParserRepeat elemTag match manyElHeads) 
      -- case parseOpeningTag 

      
      -- <|> liftA2 (:) ((IText . (:[])) <$> anyChar) (htmlGenParserRepeat elemTag match manyElHeads) 


-- | NEW IDEA!!

  -- Continue if first tree is present in second tree AND! allow for new branches
    -- NewBranch -> Optional (ParsecT s u m a) in data sig of (Many a)


specificChar :: Stream s m Char => ParsecT s u m Char
specificChar = do
  notFollowedBy (parseOpeningTag Nothing [] >> char '>') <?> "error on specificChar (tag found)"
  anyChar 


  --  first <- anyChar
  -- if first == '<'
  --   then
  --   do 
  --     e <- option "false" (some alphaNum)
  --     attrs <- option "false" (some attrsParser)
  --     close <- option "false" (char '>')

  --     if e == "false" && e == attrs && attrs = close
  --       then return (first : [])  -- no elemHead return 
  --   -- attrs <- many attrsParser
  --   -- char '>'
  --   -- if length x == 0
  --   -- then
  --     -- return first
      
  --   -- do
  --     -- notFollowedBy $ (,) <$> many alphaNum <*> many attrParser <* char '>'
  --     -- return (first : [])
  --   else return (first : [])
    
  --   manyTill anyChar (char ' ') == 
  --   many alphaNum
  --   many attrsParser 
    
  --   elemTag : many attrsParser
  --   parserFail "
  --   else
    
  -- parseOpeningTag >> parserFail ""
  
  -- it can be any character except < if its not followedby  


-- (do { txt <- try stylingElem; return $ (IText txt):[] })

-- | treeElemParserSpecific is an interface to this (via innerParserSpecific)
-- | This inner function uses the Many datatype to differentiate between whether we should expect
-- | to parse a single element with the given specs or allow for multiple of the given element specs in a row
-- |
-- | 
treeElemParserSpecificContinuous :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a)
                                 -> [Many (Tree ElemHead)] -- The total innerHtml structure the current element must have in order to match
                                 -> ParsecT s u m ([Many (Tree ElemHead)], TreeHTML a)
treeElemParserSpecificContinuous match manyElHeads = do
  let
    -- If we take up until the 1st (One a) then we know that it should succeed in all valid cases or fail
    elSet :: [Many (Tree ElemHead)]
    elSet = takeTill (\case { One a -> True; _ -> False}) manyElHeads


    
  (e,attrs) <- parseOpeningTag (Just $ fmap (fst . rootLabel . fromMany) elSet) []
      
  (innerForest, outputStack) <- tryElHeads (e,attrs) elSet   
  --  at this point in the case where we have parsed an opening tag we have two possibilities
    --  manyElHeads = [] | x:xs
    --  [] -> then why do we have an opening tag? this should have been either an IText or the endTag
  
  (m, inTx, _inTr) <- innerParserSpecific match e innerForest
  let
    manyElHeads' :: [Many (Tree ElemHead)]
    manyElHeads' = drop ((length manyElHeads) - (length outputStack)) manyElHeads
  return $ (,) manyElHeads' (TreeHTML e attrs m inTx [])  -- TODO: propagate rawInner

--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------

-- | This is largely a subfunc of htmlGenParserContains 
-- | Accepts any element and if element is in the order of our checklist-of-elems, we give the tail of elems back
-- | if the tail reaches [] before we hit the end tag then we are successful 
treeElemParserContains :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a)
                                 -> [Many (Tree ElemHead)]
                                 -> ParsecT s u m ([Many (Tree ElemHead)], TreeHTML a)
treeElemParserContains match manyElHeads = do
  -- (Just $ fmap (fst . rootLabel . fromMany) elSet) [] -- <|> (fmap Left $ parseOpeningTag Nothing [])
  (e,ats) <- parseOpeningTag Nothing []
  -- char '>'
  let
    elSet = takeTill (\case { One a -> True; _ -> False}) manyElHeads
    -- forestNStack = tryElHeads' elAttrs elSet
  case tryElHeads' (e, ats) elSet of
    Right (innerForest, outputStack) -> do
      let
        manyElHeads' :: [Many (Tree ElemHead)]
        manyElHeads' = drop ((length manyElHeads) - (length outputStack)) manyElHeads
      if innerForest == []
        then -- this can be selfclosing
        do
          case elem e selfClosing of
            True -> do
              (try (string ">") <|> string "/>")
              return $ (,) manyElHeads' (TreeHTML e ats mempty mempty [])
            False -> do
              (m, inTx, _inTr) <- innerParserContains match e innerForest
              return $ (,) manyElHeads' (TreeHTML e ats m inTx [])  -- TODO: propagate rawInner
        else -- this cannot be selfClosing
        do
          (m, inTx, _inTr) <- innerParserContains match e innerForest
          return $ (,) manyElHeads' (TreeHTML e ats m inTx [])  -- TODO: propagate rawInner

      -- string "/>" >> return (mempty, mempty, mempty) <|> string ">" >> innerParserContains 

      
    Left someError -> do
      (inText, matchBook, _treees) <- innerTreeElemParser e match
      return $ (,) manyElHeads (TreeHTML e ats matchBook inText [])  -- TODO: propagate rawInner 
    
          
   -- ParsecT s u m ([a], String, [Tree ElemHead]) 

htmlGenParserContains :: (Stream s m Char, ShowHTML a) =>
                       String 
                    -> Maybe (ParsecT s u m a)
                    -> [Many (Tree ElemHead)]
                    -> ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserContains elemTag match manyElHeadss = {-parsesTreeHs-}
  case manyElHeadss of
    [] -> do
      (htMMers, _) <-  manyTill_ (htmlGenParserFlex match) (endTag elemTag)
      return htMMers
    manyElHeads -> 
      liftA2 (:) (fmap Match $ try (fromMaybe parserZero match)) (htmlGenParserContains elemTag match manyElHeads)
      <|> liftA2 (:) (fmap IText $ try stylingElem) (htmlGenParserContains elemTag match manyElHeads) 
      <|> try ((treeElemParserContains match manyElHeads)
               >>= (\(sM, a) -> fmap ((Element a):) (htmlGenParserContains elemTag match sM)))
      <|> liftA2 (:) (fmap (IText . (:[])) (try $ specificChar' elemTag)) (htmlGenParserContains elemTag match manyElHeads)
      <|> (endTag elemTag
           >> case length (filter (\case {One a -> True; _ -> False}) manyElHeadss) == 0 of
                True -> return []
                False -> parserFail $ "still havent yielded " <> show manyElHeadss) 

specificChar' :: Stream s m Char => Elem -> ParsecT s u m Char
specificChar' elemTag = do
  notFollowedBy (parseOpeningTag Nothing [] >> char '>') <?> "error on specificChar' (tag found)"
  notFollowedBy (endTag elemTag) 
  anyChar 



innerParserContains :: (Stream s m Char, ShowHTML a) =>
                       Maybe (ParsecT s u m a)
                    -> Elem
                    -> SubTree ElemHead
                    -> ParsecT s u m ([a], String, [Tree ElemHead])
innerParserContains match tag subTree =
  case elem tag selfClosing of
    True -> if not $ null subTree then undefined else do
      (try (string ">") <|> string "/>")
      return (mempty, mempty, mempty)
    False -> do
      char '>'
      x <- htmlGenParserContains tag match (reverse $ groupify subTree [])
      let
        -- | need to ensure all the trees are in order
        (inText, matchBook, treees) = foldr foldFuncTrup mempty (x)
      return (matchBook, inText, treees)

-- | Very similar to treeElemParserSpecific except that it allows for a new nodes in the HTML DOM tree
-- | to exist at random as long as when we resume parsing we still find all of the branches we found in the
-- | TreeHTML a that is given as an arg to this function
similarTreeH :: (Stream s m Char, ShowHTML a)
             => Maybe (ParsecT s u m a)
             -> TreeHTML a
             -> ParsecT s u m (TreeHTML a)
similarTreeH matchh treeH = do
  (e,at) <- parseOpeningTag (Just $ [elTag treeH]) (((fmap . fmap) Just) . Map.toList $ attrs treeH)
  char '>'
  rawMatchers <- htmlGenParserContains e matchh (groupify (_innerTree' treeH) [])
  let (inTx, m, _inTr) = foldr foldFuncTrup mempty rawMatchers
  return $ TreeHTML e at m inTx rawMatchers
  
  -- treeElemParserContains matchh (elTag treeH) (Map.toList $ attrs treeH) (_innerTree' treeH)
  
-- treeElemParserContains :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a)
                                 -- -> [Many (Tree ElemHead)]
                                 -- -> ParsecT s u m ([Many (Tree ElemHead)], TreeHTML a)

-- | Returns an entire group of highly similar elements based on their specifications such
-- | as their innerTrees, the element tag, and attributes.
-- |
-- | This can be used to autonomously determine the structure of and find search result items after you've submitted a form
-- htmlGroupSimilar :: (Stream s m Char, ShowHTML a)
--                  => Maybe [Elem]
--                  ->  Maybe (ParsecT s u m a)
--                  -> [(String, Maybe String)]
--                  -> ParsecT s u m (GroupHtml TreeHTML a)
-- htmlGroupSimilar elemOpts matchh attrsSubset = 
--   -- Not sure about the order yet tho
--   fmap mkGH $ try (treeElemParser elemOpts matchh attrsSubset
--                    >>= (\treeH -> fmap (treeH :) (some (try $ similarTreeH matchh treeH))))

-- I could rewrite groups to become more versatile by allowing optionally
-- any number of '\n' after parsing a similar tree 


-- some (try $ similarTreeH matchh treeH) <* optional (many $ char '\n')



htmlGroupSimilar :: (Stream s m Char, ShowHTML a)
                 => Maybe [Elem]
                 ->  Maybe (ParsecT s u m a)
                 -> [(String, Maybe String)]
                 -> ParsecT s u m (GroupHtml TreeHTML a)
htmlGroupSimilar elemOpts matchh attrsSubset = 
  -- Not sure about the order yet tho
  fmap mkGH $ (do
                 
                  treeH <- treeElemParser elemOpts matchh attrsSubset <* nl 
                  treeHs <- some $ try $ similarTreeH matchh treeH <* nl
                  pure $ treeH : treeHs 
              )


--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------
takeTill :: (a -> Bool) -> [a] -> [a]
takeTill _ [] = []
takeTill f (x:xs) = if f x then x:[] else x : takeTill f xs 

-- | yields how many are still worth trying
tryElHeads :: (Elem, Attrs)
           -> [Many (Tree ElemHead)]
           -> ParsecT s u m ([Tree ElemHead], [Many (Tree ElemHead)])
tryElHeads _ [] = parserFail "none of me opening tags worked laddy" 
tryElHeads tagAttrs ((Many (Node label forest)):outputStack) =
  if tagAttrs == label
  then return $ (forest, (Many (Node label forest)):outputStack) -- added back to stack 
  else tryElHeads tagAttrs outputStack -- deleted from temp stack if not found 
tryElHeads tagAttrs ((One (Node label forest)):outputStack) =
  if tagAttrs == label
  then return $ (forest, outputStack)
  else parserFail $ "missing element" <> show label 

tryElHeads' :: (Elem, Attrs)
           -> [Many (Tree ElemHead)]
           -> Either String ([Tree ElemHead], [Many (Tree ElemHead)])
tryElHeads' _ [] = Left "none of me opening tags worked laddy" 
tryElHeads' tagAttrs ((Many (Node label forest)):outputStack) =
  if tagAttrs == label
  then return $ (forest, (Many (Node label forest)):outputStack) -- added back to stack 
  else tryElHeads' tagAttrs outputStack -- deleted from temp stack if not found 
tryElHeads' tagAttrs ((One (Node label forest)):outputStack) =
  if tagAttrs == label
  then return $ (forest, outputStack)
  else Left $ "missing element"   <> show label -- I believe Left "missing element"



innerParserSpecific :: (Stream s m Char, ShowHTML a) =>
                       Maybe (ParsecT s u m a)
                    -> Elem
                    -> SubTree ElemHead
                    -> ParsecT s u m ([a], String, [Tree ElemHead]) 
innerParserSpecific match tag subTree =
  case elem tag selfClosing of
    True -> if not $ null subTree then undefined else do
      (try (string ">") <|> string "/>")
      return (mempty, mempty, mempty)
    False -> do 
      -- char '>'
      x <- htmlGenParserRepeat tag match (reverse $ groupify subTree [])
      let
        -- | need to ensure all the trees are in order 
        (inText, matchBook, treees) = foldr foldFuncTrup mempty (x)
      return (matchBook, inText, treees)





-- currentElHead :: Stream s m Char =>
--                  ParsecT s u m a
--               -> [Many (Tree ElemHead)]
--               -> ParsecT s u m [HTMLMatcher TreeHTML a]
-- currentElHead = do
--   (m, inTx, inTr) <- innerParserSpecific mElHead1 match elem 
--   (:) <$> (TreeHTML e at m inTx inTr) <*> htmlGenParserRepeat match (mElHead1:mElHead2:manyElHeads)



-- treeElemParserSpecificContinuous match (manyElHeads) = do
--   (eIns, at)  <- parseOpeningTagDesc (Just [(fst mElHead1), (fst mEleHead2)]) []
--   if attrs e == snd mElHead1 
--     then currentElHead match manyElHeads 
--     else nextElHead match manyElHeads <|> parserFail "did not match on either of the next two previous elements" 
--     if attrs e == snd mElHead2     -- should also have this or similar behaviour in case (One a) 
--     then
--       do
--         (matches, inText, inTree) <- innerParserSpecific mElHead2 match eIns
--         (:[]) <$> (TreeHTML e at matches inText inTree) <*> htmlGenParserRepeat match (mElHead2:manyElHeads)
--     else parserFail "did not match on either of the next two previous elements" 
--          -- case e1,e2,parserFail

      
      -- x <- specificRepetitiveForest (reverse $ groupify subTree []) (fromMaybe parserZero matchh)
      -- ---
      -- -- Everything here is to avoid false positives from being too
      -- -- general while developing 
      -- option "" (string "Hockey")
      -- many space
      -- option "" (string "tour case suing ")
      -- ---
      -- endTag tag
      -- --can be followed by whatever: 
      -- -- (y, _) <- manyTill_ (htmlGenParserFlex matchh) (endTag tag) 
      -- let
      --   -- | need to ensure all the trees are in order 
      --   (inText, matchBook, treees) = foldr foldFuncTrup mempty (x)
      -- return $ TreeHTML tag attrsOut matchBook (reverse inText) (reverse treees)--(_matches itr) (_innerText itr) (innerTree itr)


-- | IS THIS IN THE RIGHT ORDER OR DOES IT NEED TO BE REVERSED?
-- | Creates a simplified set of instructions for parsing a very specific Tree structure 
groupify :: Eq a => [Tree a] -> [Many (Tree a)] -> [Many (Tree a)]
groupify [] acc = acc
groupify (tree:forest) [] = groupify forest (One tree:[])
groupify ((Node elemHead subForest):forest) (mTree:acc) =
  case mTree of
    One (Node elemHeadPrev subForestPrev) ->
      if elemHead == elemHeadPrev
      -- here is maybe where I could add in checking if forests are equal ? 
      then groupify forest $ ((Many (Node elemHeadPrev subForestPrev)):acc)
      else groupify forest $ ((One (Node elemHead subForest)) : (One (Node elemHeadPrev subForestPrev)) : acc)
      --  we want to ensure then that this One constructor isn't touched again, we need to create
      --  another One constructor with the incoming `tree` that was peeled off
    Many (Node elemHeadPrev subForestPrev) ->
      if elemHead == elemHeadPrev
      then groupify forest ((Many (Node elemHeadPrev subForestPrev)):acc)
      else groupify forest ((One (Node elemHead subForest))
                            : (Many (Node elemHeadPrev subForestPrev))
                            : acc)



-------------------------------------------------------------------------------------------------------------------


-- | Returns a minimum of 2 --> almost like `same` should be function ; same :: a -> [a] to be applied to some doc/String
-- | note: not sure if this exists but here's where we could handle iterating names of attributes 
-- | Can generalize to ElementRep e
htmlGroup  :: (Stream s m Char, ShowHTML a)
               => Maybe [Elem]
               ->  Maybe (ParsecT s u m a)
               -> [(String, Maybe String)]
               -> ParsecT s u m (GroupHtml TreeHTML a)
htmlGroup elemOpts matchh attrsSubset = 
  -- Not sure about the order yet tho
  fmap mkGH $ try (treeElemParser elemOpts matchh attrsSubset
                   >>= (\treeH -> fmap (treeH :) (some (try $ sameTreeH matchh treeH))))

-- | Html table group   
table :: Stream s m Char => ParsecT s u m (GroupHtml TreeHTML String) 
table = htmlGroup (Just ["tr"]) Nothing []





-- test for

-- Previous Element of (if Many) :: Many a
-- Next element of Many a
-- Match?
-- IText String
  
  -- until endTag 


-- | Build [Many (Tree ElemHead)]
-- | Write parser that tries each case OR parses openingTag and then decides what case it fits
  -- | For set (Previous, Next) if Next is True then delete Previous parser
  -- |    Next becomes "Previous" in future equation(s)
  
-- | Fail onto plain IText (of parent element that the parser is currently in)




--------------------------------------------------------------------------------------------------------------------
---Generalizations----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Inner parser of treeElemParserSpecific 
-- specificRepetitiveForest :: (Stream s m Char, ShowHTML a)
--                          => [Many (Tree ElemHead)]
--                          -> ParsecT s u m a
--                          -> ParsecT s u m [HTMLMatcher TreeHTML a]
-- specificRepetitiveForest [] _ = return []
-- specificRepetitiveForest (mElHead1:mElHead2:manyElHeads) match = do
--   -- | ysA could be == []
--   htmlGenParserRepeat match (manyElHeads) -- NEW 
--   ysA {-maybeNext-} <- multiTreeElemHeadParser match mElHead2
--   ysB <- case ysA of
--            Just a -> specificRepetitiveForest (mElHead2:manyElHeads) match 
--            Nothing ->
--              -- WHat happens to the rest of Many ElHeads in this case? 
--              htmlGenParserRepeat match (multiTreeElemHeadParser match mElHead1)
--              htmlGenParserRepeat match (manyElHeads) -- NEW 
--   return (ysA <> ysB)
  -- let
  --   -- funcP :: (ShowHTML a, Stream s m Char) => ParsecT s u m [TreeHTML a]
  --   funcP = multiTreeElemHeadParser match mElHead1 
  -- y <- htmlGenParserRepeat match funcP -- this literally just allows for matching on multiple elems too
  -- -- Only applies to "specific" functions, prev: any case 
  -- ys <- case y of
  --   -- Discard last parsed pattern and go to next element formula on success of `y` 
  --   ((Element _):xs') -> specificRepetitiveForest manyElHeads match
  --   _                -> specificRepetitiveForest (manyElHead:manyElHeads) match
  -- -- return all results 






-- | This is all I actually need , no need for recursion here, since thats already done in top level func
{-# DEPRECATED multiTreeElemHeadParser "use specificContinuous style functions" #-} 
multiTreeElemHeadParser :: (Stream s m Char, ShowHTML a) =>
                          ParsecT s u m a
                       -> Many (Tree ElemHead)
                       -> ParsecT s u m [HTMLMatcher TreeHTML a]
multiTreeElemHeadParser match mTree = case mTree of
  Many (Node (elem, attrs) subTree) ->
    (fmap . fmap) Element (many (try $ treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree ))
  One (Node (elem, attrs) subTree) ->
    treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree >>= return . flip (:) [] . Element
                                                                        -- like return . (\x -> x :[])



-- | Is able to repeat / execute any pattern that returns multiple elements of same type
-- |(see manyTreeElemHeadParser)
-- htmlGenParserRepeat :: (Stream s m Char, ShowHTML a) =>
                       -- ParsecT s u m a
                    -- -> [Many (Tree ElemHead)]
                    -- -> ParsecT s u m [TreeHTML a] -- Can just apply multiTreeElemHeadParser (if i should) inside
                    -- -> ParsecT s u m [HTMLMatcher TreeHTML a]


-- | HTMLGenParserRepeat is in this use case always going to be exact ie these 3 elems then the end tag
-- |  ... and maybe some text in between there

-- | OF the cases we can do this:
  -- | parse and repeat function/recurse
  -- | or find end tag >> return [] which ends list 
  



  -- -- (eIns, at)  <- parseOpeningTagDesc (Just [(fst mElHead1), (fst mEleHead2)]) []
  -- let
  --   atSet :: Elem -> [Tree ElemHead] -> [Tree ElemHead]
  --   atSet = filter (\(Node (e, a)) -> if eIns == e then True else False) elSet 
      

            
  -- f' eIns at elHeads
                                     

  -- -- | Note that this is not a manyTill but should behave like any parser, f (however f is defined)
  -- -- | then once thats complete, parse endTag (with maybe some text) 

    -- f :: Stream s m Char =>
         -- Maybe (ParsecT s u m a)
      -- -> Elem
      -- -> Attrs
      -- -> [Many (Tree ElemHead)]
      -- -> ParsecT s u m ([Many (Tree ElemHead)], TreeHTML a)
    --
    -- At this point, we 100% know that the head of the list is the correct one 
    -- f match endT at elHeads = do
      -- (m, inTx, inTr) <- innerParserSpecific match endT (fromMany . fst $ elHeads)

      -- case fst elHeads of
        -- Many a -> "" --doesnt have to be there at all 
        -- One a -> "" -- has to be there 
   
   -- then get attrs 


       -- OR filter for successful el tag eg "a" was apart of allowed set, it was "a" and now you
       -- filter the allowed set for "a" to reference attrs to try

                                     --eg: ref "a" ("a", fromList [("x", "y")])
                                     

    -- This function should also handle (Many a) control flow
      -- case manyA
      --  One a -> delete elHead from state if openTag matches
      --  Many a -> delete if

      --  if (Many a) on second one then reset (call treeElePSC again with delete elHeads) 
    -- f' match eIns at manyElHeads
      --  at == (snd mElHead1) && eIns == (fst mElHead1) = f eIns at manyElHeads 
      --  at == (snd mElHead2) && eIns == (fst mElHead2) = f eIns at (tail manyElHeads) 
      --  otherwise = parserFail "does not match on elements"
  

fromMany :: Many a -> a
fromMany (One a) = a
fromMany (Many a) = a 



---OLD
  -- <|> (do
  --         -- this gets into treeElemParserSpecific territory
  --         (eIns, at)  <- parseOpeningTagDesc (Just [(fst mElHead1), (fst mEleHead2)]) []
  --         if attrs e == snd mElHead1 -- (elTag e == (fst mElHead1) && (attrs a) == (snd mElHead1)
  --           then f manyElHeads 
  --           do
  --             (m, inTx, inTr) <- innerParserSpecific mElHead1 match eIns  
  --             (:) <$> (TreeHTML e at m inTx inTr) <*> htmlGenParserRepeat match (mElHead1:mElHead2:manyElHeads)
  --           else
  --           -- should also have this or similar behaviour in case (One a) 
  --           if attrs e == snd mElHead2
  --           then
  --             do
  --               (matches, inText, inTree) <- innerParserSpecific mElHead2 match elem 
  --               (:[]) <$> (TreeHTML e at matches inText inTree) <*> htmlGenParserRepeat match (mElHead2:manyElHeads)
  --           else parserFail "did not match on either of the next two previous elements" 
  --         -- case e1,e2,parserFail
  --     )

  
-- | Note that multiTreeElemHeadParser is still not handled, all I need to do is auto delete if only one
-- | actual function of multiTreeElemHeadParser will not be used but broken up


  -- <|> ((fmap . fmap) (Element) parsesTreeHs) -- list of elements, could be single element or multiple 
  -- <|> (do { x <- anyChar; return (IText (x:[]):[]) }) -- just allows for singleton creation (x:[])



    
     -- then f manyElHeads
     -- else
       -- if at == (snd mElHead2) && eIns == (fst mElHead2)
       -- then f (tail manyElHeads)
       -- else parserFail "does not match on elements"

  
--     -- should also have this or similar behaviour in case (One a) 
--     i
--     then
--       do
--         (matches, inText, inTree) <- innerParserSpecific mElHead2 match elem 
--         (:[]) <$> (TreeHTML e at matches inText inTree) <*> htmlGenParserRepeat match (mElHead2:manyElHeads)
--     else parserFail "did not match on either of the next two previous elements" 
--   -- case e1,e2,parserFail
-- )


-- might still be:

--   htmlGenParserRepeat
--   endTag tag

-- as opposed to:

--   manyTill_ htmlGenParserRepeat (endTag tag)

-- but it could still need to be the second in order to work 

---------- where htmlGenParserRepeat =
--     <|> 
--     --- if this succeeds (we try this first) then we reset I believe with this parser as Prev 

--   (do { x <- try match;  return $ (Match x):[] })
--   <|> (do { txt <- try stylingElem; return $ (IText txt):[] }) 

  
--   -- <|> ((fmap . fmap) (if innerText' == searchTerm then Element else (IText . innerText')) parsesTreeHs) -- list of elements, could be single element or multiple
--   <|> multiTreeElemHeadParser match mElHead1
  
  
  
--   <|> ((fmap . fmap) (Element) parsesTreeHs) -- list of elements, could be single element or multiple 
--   <|> (do { x <- anyChar; return (IText (x:[]):[]) }) -- just allows for singleton creation (x:[])
-- -----------



-- Doesnt change the structure of the page at all just how text is styled like MS word stuff
stylingTags = ["abbr", "b", "big", "acronym", "dfn", "em", "font", "i", "mark", "q", "small"] -- , "strong"]

-- | Just gives the inners 
stylingElem :: Stream s m Char => ParsecT s u m String 
stylingElem = do
  (e,_) <- parseOpeningTag (Just stylingTags) []
  char '>'
  fmap fst $ manyTill_ anyChar (endTag e)
  




-- | Interface to find same element    
sameTreeH :: (Stream s m Char, ShowHTML a)
              => Maybe (ParsecT s u m a)
              -> TreeHTML a
              -> ParsecT s u m (TreeHTML a)
sameTreeH matchh treeH = treeElemParserSpecific matchh (elTag treeH) (Map.toList $ attrs treeH) (_innerTree' treeH)

-- I could do 2 things at the same time as calling findSameTreeH : use in findNaive, use in `some` 

                                      

-- this implementation would cause issues for when we want to check equality of trees
-- we would need to set the inside tree element parser + we would also need to think about how to     handle matches -->> maybe check after for matches > 0?
htmlGenParserFlex :: (Stream s m Char, ShowHTML a) =>
                     Maybe (ParsecT s u m a)
                  -> ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParserFlex a = (try (Match <$> (fromMaybe parserZero a)))
                      <|> try (Element <$> treeElemParser Nothing a [])   --(treeElemParserAnyInside a))
                      <|> ((IText . (:[])) <$> anyChar)
    







--- Not in use: ----------------------------------------------------------------------------------------------------








htmlGenParser :: (Stream s m Char, ShowHTML a)
              => ParsecT s u m a
              -> ParsecT s u m (TreeHTML a)
              -> ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParser a parseTreeH = (Match <$> try a)
                             <|> (Element <$> try parseTreeH)
                             <|> (fmap (IText . (:[])) anyChar)




{-# DEPRECATED specificForest "you likely need specificRepetitiveForest" #-}
-- | Library function for when you want an exact match, if 3 of ElemHead A then it looks for 3 Elemhead A
 -- accumMaybe' :: [HTMLMatcher] -> ParsecT s u m a
specificForest :: (Stream s m Char, ShowHTML a) =>
                  [Tree ElemHead]
               -> ParsecT s u m a
               -> ParsecT s u m [HTMLMatcher TreeHTML a]
specificForest [] _ = return [] --or could be allowing for tail text
specificForest (x:xs) match = do
  y <- htmlGenParser match (nodeToTreeElemExpr x match)
  ys <- case y of
     Element _ -> specificForest xs match
     _ -> specificForest (x:xs) match
  return (y : ys) 


  
nodeToTreeElemExpr :: (Stream s m Char, ShowHTML a) =>
                      Tree ElemHead
                   -> ParsecT s u m a
                   -> ParsecT s u m (TreeHTML a)
nodeToTreeElemExpr (Node (elem, attrs) subTree) match =
  treeElemParserSpecific (Just match) elem (Map.toList attrs) subTree



-----------------------------------------------------------------------------------------------------------------------------Main---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Used by treeElemParser' 
innerElemParser2 :: (ShowHTML a, Stream s m Char) =>
                   String
                -> Maybe (ParsecT s u m a)
                -> ParsecT s u m [HTMLMatcher TreeHTML a]
innerElemParser2 eTag innerSpec = char '>'
                                  -- >> manyTill (try (Element <$> treeElemParser Nothing innerSpec [])) (try (endTag eTag))
                                  >> manyTill (try (Match <$> (fromMaybe parserZero innerSpec))
                                               <|> (try (IText <$> stylingElem))
                                               <|> try (Element <$> treeElemParser' Nothing innerSpec [])
                                               <|> ((IText . (:[])) <$> anyChar)) (try $ endTag eTag)

--- NEXT 3 ARE NOT IN USE, useful for API?

treeElemParserAnyInside :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a) -> ParsecT s u m (TreeHTML a)
treeElemParserAnyInside match = treeElemParser Nothing match []

--------------------------------------------------------------------------------------------------------------------
-------------------------------------------Groupings--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  -- (_, treeH) <- manyTill_ (anyChar) (try $ treeElemParser elemOpts matchh attrsSubset)
  -- treeHs <- some (try $ findSameTreeH matchh treeH)
  -- return $ mkGH (treeH : treeHs) 


anyHtmlGroup :: (ShowHTML a, Stream s m Char) => ParsecT s u m (GroupHtml TreeHTML a)
anyHtmlGroup = htmlGroup Nothing Nothing [] 


-- Maybe this func should go in Find.hs
-- 0 -> Nothing
findAllSpaceMutExGroups :: (ShowHTML a, Stream s m Char) => ParsecT s u m (Maybe [GroupHtml TreeHTML a])
findAllSpaceMutExGroups = findNaive anyHtmlGroup 




findAllMutExGroups' = undefined -- prime in name until renaming errors complete
-- deals with cases where attr:selected="true" exists since there will be two subgroups that
-- require concatenations

-- find == runParserOnHtml :: ParsecT s u m (Maybe [a]) ; a ~ GroupHtml b

-- Note: If we can find all groups, then we can find all non-groups ~ tree/display functionality 


-- findSomeHtmlNaive (try findAnyHtmlGroup) htmlText 


------------------------------------------------------------------------------------------------------------------------------------------------------Numerically Flexible Pattern Matching On Specific Elements----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  
-- | What if above was of type :: [Many (Tree a)] -> ParsecT s u m [HTMLMatcher a]

-- [Node a _, Node b _, Node c _, Node d _, Node e _]  -> [Many tree1, Many tree2, One tree3]

                                   -- (((IText . (:[])) <$> anyChar) >>= return . flip (:) [])
                                   -- (Match <$> try a) >>= return . flip (:) [])

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



















-- <|> (parseOpeningTag
--        >>= (\(e,a) -> do
--                a <- parseInnerBodySpecific *> endTag e
--                if e == (fst mElHead1) && a == (snd mElHead1)
--                  then 
--                case e of
--                  (fst mElHead1) -> 
--                htmlGenParserRepeat match 
               
--                a <> b
--                where b = 
--                        case e of
--                          mElHead1 -> do
--                            do innerBody of TreeElem(Specific) 
                  
--                           htmlGenParserRepeat match (mElHead1:mEleHead2:manyElHeads)
--                           a <> b

--                 mElHead2 -> htmlGenParserRepeat match (mElHead_1_OR_Both:manyElHeads) 
--            )
--   <|> element2 


-- elemSkeleton :: Stream s m Char => ParsecT s u m (Tree ElemHead)
-- elemSkeleton tags attrsIn pat = do
--   (e, a) <- parseOpeningTag --
--   branches <- case elem e selfClosing of
--     True -> return $ Node (e, a) []
--     False -> fmap (Node (e, a)) (manyHtml (elemSkeleton (ANY _ _))) *> endTag e
--   return Node 
