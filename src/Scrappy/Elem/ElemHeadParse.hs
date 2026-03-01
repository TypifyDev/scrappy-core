{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}


module Scrappy.Elem.ElemHeadParse where

import Scrappy.Links (Link, LastUrl, CurrentUrl)
import Scrappy.Elem.Types (Elem, ElemHead, Attrs, AttrsError(IncorrectAttrs), getHrefAttrs) -- Attr)

import Control.Applicative (some)
import Text.Parsec (Stream, ParsecT, (<|>), string, try, noneOf, parserZero, char, option, space,
                   alphaNum, between, many, parserFail, optional)
import Data.Map as Map (Map, fromList, lookup, toList) 
import Data.Maybe (fromMaybe)
--import Witherable (mapMaybe)

import Scrappy.Types

-- | needs to use many for multiple links


href :: Stream s m Char => Bool -> LastUrl -> ParsecT s u m Link
href booly cUrl = ((getHrefAttrs booly cUrl) . snd) `mapMaybe` (parseOpeningTag (Just ["a"]) [])

href' :: Stream s m Char => Maybe CurrentUrl -> ParsecT s u m Link
href' = undefined --  bo






-- | Safe because it forces parse of the entire ElemHead then pulls if there
-- | Designed for use in findSomeHtml
parseAttrSafe :: Stream s m Char => String -> ParsecT s u m String
parseAttrSafe attrNm = do
  tag <- parseOpeningTag Nothing [(attrNm, Nothing)] -- i could in theory pass an expression as value
  case (Map.lookup attrNm . snd) tag of
    Nothing -> parserZero
    Just a -> return a

-- | Done like  this so that it reliably is true link and not false positive 
hrefParser :: Stream s m Char => ParsecT s u m String --Link
hrefParser = do
  tag <- parseOpeningTag Nothing [("href", Nothing)] -- i could in theory pass an expression as value
  case (Map.lookup "href" . snd) tag of
    Nothing -> parserZero
    Just a -> return a
-- snd OR fmap snd for multiple then analyze URI

parseOpeningTagF :: Stream s m Char => String -> (String -> Bool) -> ParsecT s u m ElemHead --Link
parseOpeningTagF attrib predicate = do
  (e, as) <- parseOpeningTag Nothing [(attrib, Nothing)] -- i could in theory pass an expression as value
  case Map.lookup attrib as of
    Nothing -> parserZero
    Just a -> if predicate a then return (e,as) else parserFail "couldnt find match parseOpeningTagF"

-- | TODO(galen): make this actually generic, not just for one attr
parseOpeningTagWhere :: Stream s m Char
                     => Maybe [Elem]
                     -> String 
                     -> (String -> Bool)
                     -> ParsecT s u m ElemHead --Link
parseOpeningTagWhere es attrib predicate = do
  (e, as) <- parseOpeningTag es [(attrib, Nothing)] -- i could in theory pass an expression as value
  case Map.lookup attrib as of
    Nothing -> parserZero
    Just a -> if predicate a then return (e,as) else parserFail "couldnt find match parseOpeningTagF"




-- parseOpeningTagFs :: Stream s m Char => [(String, (String -> Bool)] -> ParsecT s u m ElemHead --Link
-- parseOpeningTagFs (attrib, predicate):xs = do
  -- (e, as) <- parseOpeningTag Nothing [(attrib, Nothing)] -- i could in theory pass an expression as value
  -- case Map.lookup attrib a of
    -- Nothing -> parserZero
    -- Just a -> if predicate a then return (e,as) else parserZero 


-- | Allows parsing with high level predicate 
hrefParser' :: Stream s m Char => (String -> Bool) -> ParsecT s u m String --Link
hrefParser' predicate = do
  tag <- parseOpeningTag Nothing [("href", Nothing)] -- i could in theory pass an expression as value
  case (Map.lookup "href" . snd) tag of
    Nothing -> parserZero
    Just a -> if predicate a then return a else parserZero 
-- snd OR fmap snd for multiple then analyze URI

-- Does between have an answer to my problem of non-infinite parsers?
-- between' (parseOpeningTag) (endTag) innerParser ... or something


--also could be with '' not just ""
-- | In future should add replace of apostrophe and similar issues to corresponding html representations
attrValue :: Stream s m Char => ParsecT s u m [Char]
attrValue = (between (char '"') (char '"') (many (noneOf ['"'])))
            <|> (between (char '\'') (char '\'') (many (noneOf ['\''])))

--PAST
-- attrValue :: Stream s m Char => ParsecT s u m [Char]
-- attrValue = between (char '"' <|> char '\'') (char '"' <|> char '\'') (many (noneOf ['"', '\'']))

-- 

-- -- this could be extensible to scrapePDFLink
-- attrValue' :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
-- attrValue' parser = between (char '"') (char '"') parser

-- | Both attrValue functions mimic map functionality 

attrValuesExist :: [(String, String)] -> [(String, Maybe String)] -> Bool 
attrValuesExist _ [] = True-- Either AttrsError [(String, String)]
attrValuesExist attrsOut (nextAttr:attrsIn)
  | attrValueExists attrsOut nextAttr = True && (attrValuesExist attrsOut attrsIn)
  | otherwise = False 

attrValueExists :: [(String, String)] -> (String, Maybe String) -> Bool
attrValueExists [] _ = False 
attrValueExists (attrF:attrsOut) nextAttr-- (AttrPair nextAttrP:attrsIn)
  | fst attrF == fst nextAttr && snd nextAttr == Nothing = True
  | fst attrF == fst nextAttr && snd nextAttr == (Just (snd attrF)) = True
  | otherwise = attrValueExists attrsOut nextAttr


-- -- | Doesn't check for value of attr
-- attrNamesExist :: [(String, String)] ->  [AttrsP] -> Either AttrsError [(String, String)]
-- -- attrNamesExist attrsOut [AnyAttr] = Right attrsOut --relevant??
-- attrNamesExist attrsOut [] = Right attrsOut 
-- attrNamesExist attrsOut (Attr nextAttr:attrsIn)
  
--    elem nextAttr (fmap fst attrsOut) = attrNamesExist attrsOut attrsIn
--    otherwise = Left IncorrectAttrs 
--   -- Im not sure this will continue to the next element though

-- -- | With attrs, we must parse to a map regardless of actual value due to lack of ordering then
-- -- |  should do lookup


attrName :: Stream s m Char => ParsecT s u m String 
attrName = some (alphaNum <|> char '-' <|> char '_')
-- | Need lower|UPPER case insensitivity
           --

-- SPACE, ("), ('), (>), (/), (=)

-- | for generalization sake
attrParser :: Stream s m Char => ParsecT s u m (String, String)
attrParser = do
      _ <- many (space <|> char '\n' <|> char '\t')

      --re-implement anyAttr
        --needs to include weird edge cases
      attrName' <- attrName
      content <- option "" (char '=' >> attrValue)
      return (attrName', content)
                                  -- [AttrsPNew]
attrsParser :: Stream s m Char =>
               [(String, Maybe String)] -- Maybe (ParsecT s u m String)
            -> ParsecT s u m (Either AttrsError (Map String String))
            --change to Either AttrsError (Map String String) 
attrsParser attrs = do
  -- attrPairs <- MParsec.manyTill attrParser {- this needs to also handle -} (char '/' <|> char '>')
  attrPairs <- many $ try attrParser -- (char '>' <|> char '/')
  let
    attrPairsMap = fromList attrPairs
  case isAttrsMatch attrPairsMap attrs of
    True -> return $ Right attrPairsMap
    False -> return $ Left IncorrectAttrs


isAttrsMatch' :: Map String String -> [(String, Maybe String)] -> Bool
isAttrsMatch' _ [] = True
isAttrsMatch' mapAttr ((name, maybeVal):desired)
  | Map.lookup name mapAttr == Nothing = False
  | (Map.lookup name mapAttr == maybeVal) || (maybeVal == Nothing) = isAttrsMatch mapAttr desired
  | otherwise = False 

isAttrsMatch :: Map String String -> [(String, Maybe String)] -> Bool
isAttrsMatch _ [] = True
isAttrsMatch mapAttr ((name, maybeVal): desired) = case maybeVal of
  Just val ->
    case Map.lookup name mapAttr of
      Just valFromKey -> if val /= valFromKey then False else isAttrsMatch mapAttr desired
      Nothing -> False
        
  Nothing ->
    case Map.lookup name mapAttr of
      Just _ {- we only care about the name -} -> isAttrsMatch mapAttr desired
      Nothing -> False

attrsFit :: Map String String -> [(String, (String -> Bool))] -> Bool
attrsFit _ [] = True
attrsFit mapppy ((name, test): rest) =
  (fromMaybe False $ fmap test $ Map.lookup name mapppy) && attrsFit mapppy rest

                                  -- [AttrsPNew]



attrsMatch' :: Map String String -> Map String String -> Bool
attrsMatch' a b = attrsMatch (toList a) b



attrsMatch :: [(String, String)] -> Map String String -> Bool
attrsMatch [] _ = True 
attrsMatch ((k,v):kvs) mappy = case Map.lookup k mappy of
  Just val ->
    if elem k ["title", "alt", "href"] then True && attrsMatch kvs mappy
    else digitEqFree v val && attrsMatch kvs mappy
  Nothing -> False 

                               
attrsParserDesc :: Stream s m Char =>
               [(String, String)] -- Maybe (ParsecT s u m String)
            -> ParsecT s u m (Map String String)
            --change to Either AttrsError (Map String String) 
attrsParserDesc attrs = do
  attrPairs <- many attrParser -- (char '>' <|> char '/')
  let
    attrPairsMap = fromList attrPairs 
  if attrsMatch attrs attrPairsMap  
    then return attrPairsMap
    else parserFail $ "incorrect attrs:" <> (show $ unfit attrs attrPairsMap)
    
  -- let
    -- attrPairsMap = fromList attrPairs
  -- case isAttrsMatch attrPairsMap attrs of
    -- True -> return $ Right attrPairsMap
    -- False -> return $ Left IncorrectAttrs



parseOpeningTagDesc :: Stream s m Char => Maybe [Elem] -> [(String, String)] -> ParsecT s u m (Elem, Attrs)
parseOpeningTagDesc elemOpts attrList = do
  _ <- char '<'
  el <- mkElemtagParser elemOpts
  parsedAttrs <- attrsParserDesc attrList
  return (el, parsedAttrs) 


-- | Allows for certain degrees of freedom such as 1 spot off eg 123 vs 1230 (or even (109|22))
-- | as well as any numerical digit must also be a numerical digit from 0 to 9
digitEq :: String -> String -> Bool
digitEq [] [] = True -- Only time the func gives True
digitEq [] (_:_) = False
digitEq (_:_) [] = False
digitEq (charA:xs) (charB:ys) =
  if charA == charB
  then True && digitEq xs ys 
  else
    if elem charA ['0'..'9'] && elem charB ['0'..'9']
    then digitEq xs ys 
    else
      -- allow for one more digit 
      saveDigitEq (charA:xs) (charB:ys)  

-- A; 12340red
-- B; 1234red

saveDigitEq :: String -> String -> Bool
saveDigitEq [] _ = False
saveDigitEq _ [] = False
saveDigitEq as@(a:_) bs@(b:_) =
  if elem (length as - length bs) [1,-1]
  then -- allows for one more digit
    if elem a ['0'..'9'] || elem b ['0'..'9']
    then svDigEq as bs
    else False
  else False

svDigEq :: String -> String -> Bool
svDigEq [] _ = False
svDigEq _ [] = False
svDigEq (charA:as) (charB:bs) = case (as, bs) of
  (a:restA, _) | a == charB -> digitEq restA bs
  (_, b:restB) | b == charA -> digitEq as restB
  _ -> saveDigitEq (charA:as) bs || saveDigitEq as (charB:bs)



-- OR!!!
digitEqFree :: [Char] -> [Char] -> Bool
digitEqFree [] [] = True
digitEqFree (a:rest) [] = if elem a ['0'..'9'] then digitEqFree rest [] else False
digitEqFree [] (b:rest) = if elem b ['0'..'9'] then digitEqFree [] rest else False
digitEqFree (a:restA) (b:restB) =
  if elem a ['0'..'9'] then digitEqFree restA (b:restB)
  else
    if elem b ['0'..'9'] then digitEqFree (a:restA) restB
    else
      if a == b then digitEqFree restA restB
      else False 
      


unfit :: [(String, String)] -> Map String String -> [(String, String)]
unfit [] _ = []
unfit ((n,v):ns) mappy = case Map.lookup n mappy of
  Nothing -> (n, "no attr") : unfit ns mappy
  Just val -> if elem n ["href", "alt", "title"] then unfit ns mappy
              else if digitEq v val
                   then unfit ns mappy
                   else (n<>":"<>"("<>val<>"|"<>v<>")", "failed test") : unfit ns mappy 


mkAttrsDesc :: [(String, String)] -> [(String, (String -> Bool))]
mkAttrsDesc atrs = (fmap . fmap) digitEqFree atrs

-- htmlGroup = do
--   (e,a) <- treeElemParser
--   treeElSpec e a
--     where 
--       treeElSpec e a = do
--         parseOpeningTagDesc e (mkAttrsDesc a)
--         ...innerElem + endTag 

-- attrsFit mapAttr ((name, maybeVal): desired) = case maybeVal of
--   Just val ->
--     case Map.lookup name mapAttr of
--       Just valFromKey -> if val /= valFromKey then False else isAttrsMatch mapAttr desired
--       Nothing -> False
        
--   Nothing ->
--     case Map.lookup name mapAttr of
--       Just irrValFromKey {- we only care about the name -} -> isAttrsMatch mapAttr desired
--       Nothing -> False


  
-- | NOTES
-- if href="#" on form -> just means scroll to top

-- | May rename parseOpeningTag to elemHeadParser
  -- |  -> Case of input tag: <input ...."> DONE ie no innerhtml or end tag
  -- |     then this would be more efficient or even maybe we should add an option via
  -- |     a  datatype: InnerTextOpts a = DoesntExist --efficient parser | AnyText | ParserText a
parseOpeningTag :: Stream s m Char => Maybe [Elem] -> [(String, Maybe String)] -> ParsecT s u m (Elem, Attrs)
parseOpeningTag elemOpts attrsSubset = do
  -- _ <- MParsec.manyTill anyToken (char '<' >> elemOpts >> attrsParser attrsSubset) -- the buildElemsOpts [Elem]
  _ <- char '<'
  el <- mkElemtagParser elemOpts
  parsedAttrs <- attrsParser attrsSubset
  _ <- optional (many space)
  case parsedAttrs of
    Left IncorrectAttrs -> parserZero
    Right whateva -> return (el, whateva)

-- | For elemsOpts, will either be
-- | Parser: (anyChar)
-- | Parser: (buildElemsOpts elems)

-- parseOpeningTagTutorial :: ParsecT s u m (String, [(String, String)])
-- parseOpeningTagTutorial = do
--   skipChar '<'
--   elem <- some letter
--   attrs <- manyTill attrParser 

-- attrParser :: Stream s m Char => ParsecT s u m (String, String)
-- attrParser = do
--   _ <- space
--   attrName' <- attrName
--   content <- option "" (char '=' >> attrValue)
--   return (attrName', content)


mkElemtagParser :: Stream s m Char => Maybe [Elem] -> ParsecT s u m String
mkElemtagParser x = case x of
                   -- Nothing -> MParsec.some (noneOf [' ', '>'])
                      --commented out in case below is wrong
                      Nothing -> some (alphaNum <|> char '-')
                      Just elemsOpts -> buildElemsOpts elemsOpts


-- | FUTURE USE CASES: buildElemsOpts :: [ParsecT s u m a] -> ParsecT s u m a -- using <|>
buildElemsOpts :: Stream s m Char => [Elem] -> ParsecT s u m String
-- buildElemsOpts [] = <----- i dont think i need this
buildElemsOpts [] = parserZero
buildElemsOpts (x:elemsAllow) = try (string x) <|> (buildElemsOpts elemsAllow)


























-- [("alt:(Link to external site, this site will open in a new window|Abstract/Details from ABI/INFORM Global and other databases)","failed test"),("class:( /abicomplete/ExternalImage/G@o662VDDpZRdYIFMpLL4twEDUtNbkgCEUrRr@NuLI2KJgLR3sPk50yOcu09+kEL|addFlashPageParameterformat_abstract  )","failed test"),("href:(https://www-proquest-com.proxy1.lib.uwo.ca/results.displayresultsitem.contentitemlinks:outboundevent/https:$2f$2focul-uwo.primo.exlibrisgroup.com$2fopenurl$2f01OCUL_UWO$2f01OCUL_UWO:UWO_DEFAULT$3f$3furl_ver$3dZ39.88-2004$26rft_val_fmt$3dinfo:ofi$2ffmt:kev:mtx:journal$26genre$3darticle$26sid$3dProQ:ProQ$253Aabiglobal$26atitle$3dSALARY$2bINEQUALITY$252C$2bTEAM$2bSUCCESS$252C$2bLEAGUE$2bPOLICIES$252C$2bAND$2bTHE$2bSUPERSTAR$2bEFFECT$26title$3dContemporary$2bEconomic$2bPolicy$26issn$3d10743529$26date$3d2018-01-01$26volume$3d36$26issue$3d1$26spage$3d200$26au$3dCyrenne$252C$2bPhilippe$26isbn$3d$26jtitle$3dContemporary$2bEconomic$2bPolicy$26btitle$3d$26rft_id$3dinfo:eric$2f$26rft_id$3dinfo:doi$2f10.1111$252Fcoep.12217/1967324598/318806?site=abicomplete&amp;t:ac=85FEBC108EAA4132PQ/1|https://www-proquest-com.proxy1.lib.uwo.ca/abicomplete/docview/2213130775/abstract/85FEBC108EAA4132PQ/1?accountid=15115)","failed test"),("id:(linkResolverLink|addFlashPageParameterformat_abstract)","failed test"),("linktitle","no attr"),("onclick","no attr"),("title:(Link to external site, this site will open in a new window|Abstract/Details from ABI/INFORM Global and other databases)","failed test")]






