{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}


-- | This will eventually be a beautiful interface between NLP and scrappy

module Scrappy.Elem.ITextElemParser where


import Scrappy.Elem.TreeElemParser (treeElemParser)
import Scrappy.Elem.SimpleElemParser (elemParser)
import Scrappy.Elem.ElemHeadParse (parseOpeningTag, buildElemsOpts, attrsParser)
import Scrappy.Elem.Types (HTMLMatcher(..), Elem'(..), Elem, Attrs, ShowHTML(..)
                  , TreeHTML(..), endTag
                  , selfClosingTextful, noPat, innerText', _innerTree'
                  , attrs, elTag, coerceAttrs
                  )


import Text.Parsec (parse, ParsecT, Stream, string, (<|>), anyChar, char
                   , optional, try, manyTill, many
                   , parserZero, alphaNum, oneOf
                   , digit
                   , option
                   , letter
                   , space
                   )
import Control.Monad (when, void)
import Control.Applicative (some)
import Control.Applicative.Combinators (eitherP)
import Data.Either (fromRight, isRight)
import Data.List (intercalate)

import Scrappy.Elem.ChainHTML (nl, manyTill_)



-- | comment is to crash nix as reminder to move somewhere sensible
-- data OpenStruct a = OpenStruct (Parser a)
-- type CloseStruct a = OpenStruct a -> ClosePiece a
  -- could be even \_ -> f , when the Close struct is independent of Open and I dont think this
  -- would affect speed
-- data ClosePiece a = ClosePiece (Parser a)  

-- | paired with maybeUsefulNewUrls this would allow us to scrape an entire
-- | site for a singular pattern
-- | and just by virtue of basic haskell types, there's zero reason we cant
-- | have some simple type:
-- | data Scrapeable = Case1 A | Case2 B ... 
-- fanExistential :: Url -> (Url -> Bool) -> MaybeT m a -> MaybeT m [a]
-- fanExistential url = do
--   html <- getHtmlST sv url 
--   links <- flip successesM html $ hoistMaybe $ scrape (hrefParser' cond)
--   fanExistential links

  -- but actually this would fail due to circularity ; the site is a graph of links

-- writersAbstractSimple = do
--   abstract
--   (_, g) <- manyTill_ anyChar $ paragElemGroup
  


-- paragElemGroup but instead use treeElemParser and ensure that
-- tree == mempty and that you try to match for a proper paragraph


emptyTree :: (ShowHTML a, Stream s m Char) =>
             Maybe [Elem]
          -> Maybe (ParsecT s u m a)
          -> [(String, Maybe String)]
          -> ParsecT s u m (TreeHTML a)
emptyTree elemOpts match attrsSpec = do
  e <- treeElemParser elemOpts match attrsSpec 
  when (not $ null $ _innerTree' e) $ parserZero
  pure e


preface :: Stream s m Char =>
           ParsecT s u m pre
        -> ParsecT s u m a
        -> ParsecT s u m a
preface _pre p = p >> (fmap snd $ manyTill_ anyChar p)


-- | Returns a minimum of 2 --> almost like `same` should be function ; same :: a -> [a] to be applied to some doc/String
-- | note: not sure if this exists but here's where we could handle iterating names of attributes 
-- | Can generalize to ElementRep e

-- instance Show a => ShowHTML Parag where
--   showH = show


class Zero a where
  consumeZero :: a -> b -> b 
  -- functions which will always work with a null value

class Singleton a where
  consumeSingleton :: a -> b
  -- functions which will always work with a singular value
  -- most functions in haskell would support this

class Multiple a where
  consumeMultiple :: a -> b 
  -- functions which are guranteed to work on 2 or more but not
  -- necessarily less

class (Zero a, Singleton a, Multiple a) => Existential a where
  consumeExists :: a -> b
  -- functions that will always work for any case of existentiality 

-- | Only matches if no innerTrees
-- | This doesn't behave exactly like a "group" function
-- | because it allows matching on one element
-- | but this will also never be empty 
emptyTreeGroup :: (ShowHTML a, Stream s m Char) =>
                  Maybe [Elem]
               -> Maybe (ParsecT s u m a)
               -> [(String, Maybe String)]
               -> ParsecT s u m [TreeHTML a]
emptyTreeGroup elemOpts match attrsSubset = do
  e <- emptyTree elemOpts match attrsSubset
  let
    same = emptyTree (Just [elTag e]) match (coerceAttrs $ attrs e)
  (:) <$> pure e <*> (many $ same <* nl)
  


-- emptyTreeGroup  :: (Stream s m Char)
--                 => Maybe [Elem]
--                 -> Maybe (ParsecT s u m a)
--                 -> [(String, Maybe String)]
--                 -> ParsecT s u m [a]
-- emptyTreeGroup elemOpts match attrsSubset = do
--   treeH <- treeElemParser elemOpts match attrsSubset
--   when (not $ null $ _innerTree' treeH) $ parserZero
  
--   let
--     sameTreeHMatches tree = do
--       t <- sameTreeH (Just paragraph) tree 
--       when (not $ null $ _innerTree' t) $ parserZero
--       pure $ _matches' t
--   many_treeHMatches <- some $ try $ sameTreeHMatches treeH
--   pure $ mconcat $ (_matches' treeH) : many_treeHMatches
  
-- >>= (\treeH -> fmap (treeH :) (some (try $ sameTreeH matchh treeH))))

-- fmap (_matches) list :: mconcat $ [match] : [[match]] 


-- myEl :: Stream s m Char => ParsecT s u m (TreeHTML Paragraph)
-- myEl = emptyTree Nothing (Just paragraph) [] 


-- abstractPattern >> skipManyTIll $ htmlGroupEmptyTree `eachContain` paragraph 


-- paragElemGroup = do
--   let
--     e1 =  do
--       e <- elemParser Nothing (Just paragraph) []
--       if exists anyEndTag $ innerText' e then parserZero else return e
--     innerTextIfNoEndTags e =
--       if exists anyEndTag $ innerText' e then parserZero else return e 
--     eN = many $ fmap innerTextIfNoEndTags
--          $ elemParser (Just . (:[]) . elem $ e1) (Just paragraph) []
--   (:) <$> (innerText' <$> e1) <*> eN 



-- writersAbstract :: Stream s m Char =>
--                    ParsecT s u m (Maybe [Either String Paragraph]) 
-- writersAbstract = do
--   elemParser Nothing (Just $ abstractWord) [] `contains`
--     findNaive (some $ elemAny paragraphOrAbstractWord)

elemAny :: Stream s m Char => ParsecT s u m (Elem' String) 
elemAny = elemParser Nothing noPat [] 

-- Start Pattern
-- pattern
-- notPattern / falsePattern


--     I might have a function in chainHTML that allows this

-- somePrevPat >> dropManyTill startPattern >> findFirst pattern

-- which I think is reasonably safe given our pattern should be:

--       some' $ elAny `contains` paragraph
--       where
--            some' = all tags determined by first tag if 'p' then might
--                    be 6 'p' tags in a row 

--   so we can have multiple but it must be same tag

-- and I could still use contain just not findNaive 
              

              
-- abstractWord :: Stream s m Char => ParsecT s u m String
-- abstractWord = string "Abstract" <|> string "abstract" <|> string "Nonlinear"

-- paragraphOrAbstractWord :: Stream s m Char => ParsecT s u m (Either String Paragraph)
-- paragraphOrAbstractWord = Left <$> abstractWord <|> Right <$> paragraph


-- revalation:
--   scrape x --> [entry1, entry2, entry3 ..]    so every item found is in order meaning we could add patterns that influence the consumption of this

-- for example:
--   lets say we want to scrape an abstract ~~~ so thus we want to scrape a body of text titled abstract
--   design principles make it fairly predictable that such will look like:

-- Abstract
-- blah blahblahblabla

-- so:
--   if p in scrape p html is == (Abstract <$> abstractPattern <|> Paragraph p) -- abstractPattern = string "abstract"

-- then we get something like:

--     [Paragraph txt, Paragraph txt2, Abstract "abstract", Paragraph txt3]

-- then reasonably we can infer that txt3 is a valid writers abstract Paragraph but since the first 2 would be rendered above, these 2 are not

-- further this could be a general pattern

-- scrapeWithSeparatingPattern :: ParsecT s u m a -> ParsecT s u m sep -> Maybe (Seperated sep a)
-- scrapeWithSeparatingPattern = undefined

-- type Seperated e a = [Either e a] 





-- i need to test if this module would work for extracting text out of research papers even though they are pdfs

--- ***** if we read a research paper backwards we can be waaaaaaay more confident about name and author, may even mix the
-- two up a lot at least its there and discernable to the user even maybe a dumb user, because names are very `discrete` in a sense

--       ---> might even make parsing citations easier

-- I need to do some investigation on the feasibility of reading papers with my parsers by determining if PDFs can just be converted from UTF8 char sequences
-- and read by scraping

-- this could also be an interface ; scrape :: Scrapeable t => ScraperT t a -> t -> Maybe [a] 


-- justPlaintext :: Int -> ParsecT s u m [String]
-- justPlaintext atLeast = 
--   fmap (filter (\x -> length x > atLeast)) {--} (find $ many (letter <|> number <|> char ' ' <|> char '.'))

-- just for testing 
type ResearchResult = String 

-- type Paragraph = [[String]]
-- | TODO(galen): these should build off each other
data Paragraph = Paragraph { unParagraph :: [Sentence] } 

data Sentence = Sentence { unSentence :: [WrittenWord] }

data WrittenWord = WW { unWord :: String }

instance Show Paragraph where
  show (Paragraph sentences) = intercalate " " $ show <$> sentences

instance Show Sentence where
  show (Sentence ws) = (intercalate " " $ show <$> ws) <> "."

instance Show WrittenWord where
  show (WW s) = s

  

instance Semigroup WrittenWord where
  (WW w1) <> (WW w2) = WW $ w1 <> " " <> w2

-- | Technically this shouldnt exist ever 
instance Monoid WrittenWord where
  mempty = WW ""

instance Semigroup Sentence where
  (Sentence s1) <> (Sentence s2) = Sentence $ s1 <> s2

instance Monoid Sentence where
  mempty = Sentence []

instance Semigroup Paragraph where
  (Paragraph p) <> (Paragraph p2) = Paragraph $ p <> p2

instance Monoid Paragraph where
  mempty = Paragraph []

instance ShowHTML Paragraph where
  showH (Paragraph s) = mconcat $ fmap showH s 

instance ShowHTML Sentence where
  showH (Sentence ws) = intercalate "" (fmap unWord ws) <> "."
  --where

punctuation :: Stream s m Char => ParsecT s u m Char
punctuation = oneOf [';', ':', '(', ')', '\"', '\'', '-', ','] -- dk if last one works

-- | Word also means bits but I mean written specifically
-- | This can definitely be expanded upon to increase its reach
-- | while maintaining validity
writtenWord :: Stream s m Char => ParsecT s u m WrittenWord
writtenWord = WW <$> (some $ alphaNum <|> punctuation) <* optional (char ' ')







wordSeparator, comma, colon, semiColon ::  Stream s m Char => ParsecT s u m String
wordSeparator = ((:[]) <$> space) <|> comma <|> colon <|> semiColon 
comma = do
  c <- char ','
  s <- option "" $ (:[]) <$> space
  pure $ c : s  
colon = do 
  c <- char ':'
  s <- option "" $ (:[]) <$> space
  pure $ c : s  
semiColon = do
  c <- char ';'
  s <- option "" $ (:[]) <$> space
  pure $ c : s  

word' :: Stream s m Char => ParsecT s u m String
word' = a_ <|> else'
  where
    a_ = do
      head_ <- char 'a'
      tail_ <- many $ letter <|> (char '\'') <|> (char '-')
      pure $ head_ : tail_
    
    else' = some (letter <|> (char '\'') <|> (char '-'))

capitalizedWord :: Stream s m Char => ParsecT s u m String
capitalizedWord = try ia <|> else'
  where
    ia = do
      head_ <- oneOf ['I', 'A']
      tail_ <- many $ letter <|> (char '\'') <|> (char '-')
      pure $ head_ : tail_
    else' = do
      head_ <- oneOf $ ['B'..'H'] <> ['J'..'Z']
      tail_ <- some (letter <|> (char '\'') <|> (char '-'))
      pure $ head_ : tail_ 

number :: Stream s m Char => ParsecT s u m String
number = do
  whole <- some digit
  dec <- option "" $ do
    (:) <$> char '.' <*> some digit
  pure $ whole <> dec

sentence :: Stream s m Char => ParsecT s u m Sentence
sentence = sentenceWhere (const True)
  
sentenceWhere :: Stream s m Char => ([WrittenWord] -> Bool) ->  ParsecT s u m Sentence
sentenceWhere testFn = do
  tokens <- eitherP capitalizedWord number >>= \case
    Left word -> do
      eitherP wordSeparator (char '.') >>= \case
        Right _period -> pure [WW word]
        Left separator -> (WW (word <> separator) :) <$> sentenceTail False
    Right num -> do
      eitherP wordSeparator (char '.') >>= \case
        Right _period -> pure [WW num]
        Left separator -> (WW (num <> separator) :) <$> sentenceTail True

  toSentence testFn tokens
  where
    toSentence :: Stream s m Char => ([WrittenWord] -> Bool) -> [WrittenWord] -> ParsecT s u m Sentence
    toSentence fn ws = case fn ws of
      False -> parserZero
      True -> pure $ Sentence ws

sentenceTail :: Stream s m Char => Bool -> ParsecT s u m [WrittenWord]
sentenceTail previousWasNumber = do
  token <- case previousWasNumber of
    True -> Left <$> word'
    False -> eitherP word' number 
  eitherP wordSeparator (char '.') >>= \case
    Left separator -> do
      tokens <- sentenceTail $ isRight token
      pure $ WW (either id id token <> separator) : tokens
    Right _period -> pure $ [WW $ either id id token]


-- sentenceTail = \case
--   True -> do
--     -- was number
--     some (word'  >> 
  

  
--   a <|> b
--   where
--     a = parseMaybe (number <* optional space) >> some word' >> (number <* optional space) >> (endSentence <|> recSentence)  
      


             

-- | For the sake of chaining these parsers, this optionally consumes
-- | a space at the end. This is the key char diff between one and many
-- | sentences
-- sentence :: Stream s m Char => ParsecT s u m Sentence
-- sentence = do
--   words <- some writtenWord
--   if length words < 4 then parserZero else return () 
--   period <- char '.'
--   optional (char ' ')
--   pure . mkSentence $ words 
 -- pure words

-- *** for research: new concept: reliable generalizations of thinking
-- --> perhaps a point on intelligence eg. math dude vs a dummy

-- | To my understanding this should not affect how we parse; it is
-- | only for sure a given that the result of our low level read is really
-- | just words and so the parsers should focus on setting up the next
-- | parser 
  
-- paragraph :: Stream s m Char => ParsecT s u m Paragraph
-- paragraph = fmap mkParagraph $ some $ try sentence

-- | This is built in a way that allows the idea of a sentence
-- | to be as internally valid as possible; the sentence controls
-- | the period 
-- mkParagraph :: [Sentence] -> Paragraph
-- mkParagraph ss = Paragraph . mkParagraph' $ ss
--   where
--     mkParagraph' :: [Sentence] -> String
--     mkParagraph' ((Sentence s):[]) = s <> ('\n':[])
--     mkParagraph' ((Sentence s):ss) = s <> " " <> (mkParagraph' ss)

-- mkSentence :: [WrittenWord] -> Sentence
-- mkSentence words = Sentence . mkSentence' $ words
--   where 
--     mkSentence' :: [WrittenWord] -> String 
--     mkSentence' ((WW lastWord):[]) = lastWord <> "." 
--     mkSentence' ((WW word):words) = word <> " " <> (mkSentence' words)

-- paragraph' :: Stream s m Char => ParsecT s u m String
-- paragraph' = fmap ((intercalate " ") . mconcat) $ some $ try sentence

-- | Note: will need more complex accumulator for case where an elem has two distinct text segements broken up
-- | by an element, (rare case)


-- at a low level I would need to create a new elemHeadParser that succeeds for
-- whenever the case is not in the input set of tag and maybe attributes, ie
-- doesnt share any commonalities with the description

-- for now ill forget about attributes

-- -- | Is parseOpeningTag except elem tags are a fail if they match
-- -- | "blacklisted" so to speak
-- notElemHeadParser :: [Elem] -> ParsecT s u m ElemHead
-- notElemHeadParser = do
--   _ <- char '<'
--   elem <- mkNegElemtagParser 

styleTags :: [String]
styleTags =  ["b", "strong", "i", "em", "mark", "small", "ins", "sub", "sup"]  

-- | Will only match elements not specified 
negParseOpeningTag :: Stream s m Char => [Elem] -> ParsecT s u m (Elem, Attrs)
negParseOpeningTag elemOpts = do
  -- _ <- MParsec.manyTill anyToken (char '<' >> elemOpts >> attrsParser attrsSubset) -- the buildElemsOpts [Elem]
  _ <- char '<'
  tag <- some alphaNum -- mkElemtagParser $ Just elemOpts
  when (tag `Prelude.elem` elemOpts) parserZero
  parsedAttrs <- attrsParser []
  pure $ (tag, fromRight mempty parsedAttrs) 

textChunk :: Stream s m Char => ParsecT s u m String 
textChunk = fmap mconcat $ manyTill plainText (try $ openOrCloseTag)
 -- also need to trim out whitespace, \n's, and script

-- | This will match any element open or closing tag that is not a style tag
openOrCloseTag :: Stream s m Char => ParsecT s u m ()
openOrCloseTag = 
  void $ (try $ negParseOpeningTag styleTags >> char '>') <|> anyEndTag
  -- could also fit in script bit here 

anyEndTag :: Stream s m Char => ParsecT s u m Char 
anyEndTag = do
  string "</" >> anyThingbut styleTags >> char '>'

-- | Despite the fun name, this is just for textChunk use
anyThingbut :: Stream s m Char => [String] -> ParsecT s u m String
anyThingbut es = do
  txt <- some alphaNum
  when (txt `Prelude.elem` es) $ parserZero
  pure txt
    
textChunkIf :: Stream s m Char => (String -> Bool) -> ParsecT s u m String
textChunkIf f = do
  x <- textChunk
  when (not $ f x) $ parserZero
  pure x 

-- parse () "" "eeee<i>hey</i><a></a>"

plainText :: Stream s m Char => ParsecT s u m String
plainText = do
  unit <- innerText' <$> styleElem <|> (fmap (:[]) anyChar)
  pure unit 


-- plainText' = do
--   styleElemOpenOrClose
--   anyChar 

--   a style elem should be skiped while a normal elem should end

-- fmap elemAny 

styleElem :: Stream s m Char => ParsecT s u m (Elem' String)
styleElem = elemParser (Just $ styleTags) noPat []
-- closeOrOpenTag = try $ negParseOpeningTag ["i"]

-- manyTill anyChar (el "i" [] >> parserZero) <|> (elemParser Nothing Nothing []) 

-- prsr = openingTag >> manyTill (styleTagElem <|> anyChar) closeOrOpenTag

-- scrape prsr html 


-- check if styleTag --> Wrap InnerText (show styleTag) ;; safely unwraps into plain                                                         text 
--   <|> (elemParser null >> pure Fail)
--   <|> anyChar

--   }-> inside of manyTill_ :: (end, [a]) 


type Html = String
-- getPlainText :: Html -> Either ParseError [String]
-- getPlainText html = do
  -- let
    -- expr = (fmap show $ parseOpeningTag (Just styleTags) [])
            -- <|> (string "</" >> buildElemsOpts styleTags >> string ">")
    -- styleTags = ["b", "strong", "i", "em", "mark", "small", "ins", "sub", "sup"]   --"del" omitted
     
  -- divied <- parse (divideUp expr) "" html    
  -- parse onlyPlainText "" $ (mconcat . catEithers) divied

removeStyleTags :: Html -> Html
removeStyleTags html = (mconcat . catEithers) $ fromRight [] $ parse (divideUp expr) "" html
  where expr = (fmap show $ parseOpeningTag (Just styleTags) [])
               <|> (string "</" >> buildElemsOpts styleTags >> string ">")
        



-- getPlainText' :: ParsecT s u m [String]
-- getPlainText' = do
  

  -- join $ fmap (  (parse onlyPlainText "")  . mconcat . catEithers) $ 
  -- where
    -- expr = (fmap show $ parseOpeningTag (Just styleTags) [])
            -- <|> (string "</" >> buildElemsOpts styleTags >> string ">")
    -- styleTags = ["b", "strong", "i", "em", "mark", "small", "ins", "sub", "sup"]   --"del" omitted 

-- Just applies onlyPlainText to html tag
-- getDocText :: Html -> [String]
-- getDocText html = 


catEithers :: [Either e a] -> [a]
catEithers [] = []
catEithers (x:xs) = case x of
  Right a -> a : catEithers xs
  Left _ -> catEithers xs
  -- in this case, our Right case are the ones we want to eliminate

divideUp :: Stream s m Char => ParsecT s u m String -> ParsecT s u m [Either String String]
divideUp parser = many ((Right <$> parser) <|> ( (Left . (:[]) ) <$> anyChar)) 

onlyPlainText :: Stream s m Char => ParsecT s u m String
onlyPlainText = fmap (\(ACT strings) -> mconcat $ reverse strings) specialElemParser 
  where
    specialElemParser :: Stream s m Char => ParsecT s u m (AccumITextElem String)
    specialElemParser = do
      (elem', _) <- parseOpeningTag (Just ["html"]) []
      (localText, inTex) <- fmap (foldr textOnlyFoldr mempty)
                                $ (try (string "/>") >> return [])
                                <|> (try $ innerElemParser' elem')
                                <|> (selfClosingTextful Nothing) -- did not have an easily associated end tag
      return $ ACT (localText : inTex) 
      
      where innerElemParser' eTag = --htmlGenParser with specialElemParser 
              char '>'
              >> manyTill (Element <$> (try specialElemParser) 
                           <|> ((IText . (:[])) <$> anyChar)  ) (endTag eTag)

            -- selfClosingTextful = manyTill (IText . (:[]) <$> anyChar) endTagg
            -- endTagg = (try (char '<'
                            -- >> (optional (char '/'))
                            -- >> MParsec.some anyChar
                            -- >> (string " " <|> string ">")))

-- Not for getting matches 
data AccumITextElem a = ACT [String]

textOnlyFoldr :: HTMLMatcher AccumITextElem String -> (String, [String]) -> (String, [String]) 
textOnlyFoldr htmlM (itextAccum, fromElemAccum) = case htmlM of 
  IText str -> 
    (itextAccum <> str, fromElemAccum) 
  Element (ACT strList) ->
    (itextAccum, fromElemAccum <> strList)
  -- should never fire
  Match mat ->
    (itextAccum <> mat, fromElemAccum)

-- Should consider using the following data structure




-- textOnlyFoldr :: [HTMLMatcher e a] -> [String] 
-- textOnlyFoldr htmlMs = fmap f htmlMs -- . filter (\x -> case x of { IText x -> True; _ -> False }) htmlMs
--       where
--         f htmlM = case htmlM of 
--                     Match str -> str
--                     IText str -> str
--                     Element e  -> innerText' e
                      
