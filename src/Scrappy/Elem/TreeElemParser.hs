{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : Scrappy.Elem.TreeElemParser
Description : Tree-based HTML element parsers for nested DOM structure matching
Copyright   : (c) Galen Sprout 2024
License     : BSD-3-Clause
Maintainer  : galen.sprout@gmail.com
-}
module Scrappy.Elem.TreeElemParser (
    -- | @since 0.1.0.0
    skipManyTill,
    -- | @since 0.1.0.0
    manyTill_,
    -- | @since 0.1.0.0
    Many (..),
    -- | @since 0.1.0.0
    treeLookupIdx,
    -- | @since 0.1.0.0
    treeElemParser,
    -- | @since 0.1.0.0
    selfClosing,
    -- | @since 0.1.0.0
    treeElemParser',
    -- | @since 0.1.0.0
    innerTreeElemParser,
    -- | @since 0.1.0.0
    SubTree,
    -- | @since 0.1.0.0
    treeElemParserSpecific,
    -- | @since 0.1.0.0
    validateGPR,
    -- | @since 0.1.0.0
    htmlGenParserRepeat',
    -- | @since 0.1.0.0
    htmlGenParserRepeat,
    -- | @since 0.1.0.0
    specificChar,
    -- | @since 0.1.0.0
    treeElemParserSpecificContinuous,
    -- | @since 0.1.0.0
    treeElemParserContains,
    -- | @since 0.1.0.0
    specificChar',
    -- | @since 0.1.0.0
    innerParserContains,
    -- | @since 0.1.0.0
    similarTreeH,
    -- | @since 0.1.0.0
    htmlGroupSimilar,
    -- | @since 0.1.0.0
    takeTill,
    -- | @since 0.1.0.0
    tryElHeads,
    -- | @since 0.1.0.0
    tryElHeads',
    -- | @since 0.1.0.0
    innerParserSpecific,
    -- | @since 0.1.0.0
    multiTreeElemHeadParser,
    -- | @since 0.1.0.0
    fromMany,
    -- | @since 0.1.0.0
    groupify,
    -- | @since 0.1.0.0
    htmlGroup,
    -- | @since 0.1.0.0
    table,
    -- | @since 0.1.0.0
    htmlGenParserContains,
    -- | @since 0.1.0.0
    stylingTags,
    -- | @since 0.1.0.0
    stylingElem,
    -- | @since 0.1.0.0
    sameTreeH,
    -- | @since 0.1.0.0
    htmlGenParserFlex,
    -- | @since 0.1.0.0
    htmlGenParser,
    -- | @since 0.1.0.0
    specificForest,
    -- | @since 0.1.0.0
    nodeToTreeElemExpr,
    -- | @since 0.1.0.0
    innerElemParser2,
    -- | @since 0.1.0.0
    treeElemParserAnyInside,
    -- | @since 0.1.0.0
    anyHtmlGroup,
    -- | @since 0.1.0.0
    findAllSpaceMutExGroups,
    -- | @since 0.1.0.0
    findAllMutExGroups',
) where

import Control.Applicative (Alternative, liftA2, many, some, (<*), (<|>))
import Control.Monad (when)
import Data.Graph (Forest, Tree (Node))
import qualified Data.Map as Map (toList)
import Data.Maybe (fromMaybe)
import Data.Tree (rootLabel)
import Scrappy.Elem.ChainHTML (nl)
import Scrappy.Elem.ElemHeadParse (parseOpeningTag, parseOpeningTagDesc)
import Scrappy.Elem.Types (
    Attrs,
    Elem,
    ElemHead,
    GroupHtml,
    HTMLMatcher (Element, IText, Match),
    ShowHTML,
    TreeHTML (TreeHTML),
    TreeIndex,
    attrs,
    elTag,
    endTag,
    foldFuncTrup,
    matches',
    mkGH,
    selfClosingTextful,
    _innerTree',
 )
import Scrappy.Find (findNaive)
import Text.Parsec (
    ParsecT,
    Stream,
    anyChar,
    char,
    manyTill,
    notFollowedBy,
    parserFail,
    parserZero,
    string,
    try,
    (<?>),
 )
import Prelude (
    Bool (..),
    Char,
    Either (..),
    Eq (..),
    Maybe (..),
    Num (..),
    Ord (..),
    Show (..),
    String,
    drop,
    elem,
    filter,
    flip,
    fmap,
    foldr,
    fst,
    length,
    mempty,
    not,
    null,
    pure,
    return,
    reverse,
    undefined,
    ($),
    (*>),
    (.),
    (<$>),
    (<>),
    (>>),
    (>>=),
 )

{- | Skip many occurrences of @p@ until @end@ succeeds, returning the result of @end@.

@since 0.1.0.0
-}
skipManyTill :: (Alternative m) => m a -> m end -> m end
skipManyTill p end = go
  where
    go = end <|> (p *> go)
{-# INLINE skipManyTill #-}

{- | Like 'manyTill' but returns both the accumulated list and the end result.

@since 0.1.0.0
-}
manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
manyTill_ p end = go
  where
    go = (([],) <$> end) <|> liftA2 (\x (xs, y) -> (x : xs, y)) p go

{- | Wrapper distinguishing single-occurrence vs repeated elements.

@since 0.1.0.0
-}
data Many a = Many a | One a deriving (Show)

{- | Look up a tree by index in a forest. Currently unimplemented.

@since 0.1.0.0
-}
treeLookupIdx :: TreeIndex -> Forest a -> a
treeLookupIdx = undefined

{- | Like 'elemParser', this matches on an HTML element but also represents the innerHTML
as a @Tree ElemHead@ so that we can match this structure in elements further down in the DOM.
See 'htmlGroup' and 'treeElemParserSpecific'.

@since 0.1.0.0
-}
treeElemParser ::
    (Stream s m Char, ShowHTML a) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (TreeHTML a)
treeElemParser elemOpts matchh attrsSubset = do
    e <- treeElemParser' elemOpts matchh attrsSubset
    when (length (matches' e) < (case matchh of Nothing -> 0; _ -> 1)) (parserFail "not enough matches")
    return e

{- | List of self-closing HTML element tags.

@since 0.1.0.0
-}
selfClosing :: [String]
selfClosing = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

{- | Internal implementation for 'treeElemParser'. Not an interface; use 'treeElemParser'.

@since 0.1.0.0
-}
treeElemParser' ::
    (Stream s m Char, ShowHTML a) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (TreeHTML a)
treeElemParser' elemOpts matchh attrsSubset = do
    (el', attrs') <- parseOpeningTag elemOpts attrsSubset
    case el' `Prelude.elem` selfClosing of
        True -> do
            _ <- try (string ">") <|> string "/>"
            case matchh of
                Nothing -> return $ TreeHTML el' attrs' mempty []
                Just _ -> parserZero
        False -> do
            rawMatchers <-
                (try (string "/>") >> return [])
                    <|> (try $ innerElemParser2 el' matchh)
                    <|> (selfClosingTextful matchh)
            let (_inText, matchBook, _treees) = foldr foldFuncTrup mempty rawMatchers
            return $ TreeHTML el' attrs' matchBook rawMatchers

{- | Parse the inner content of a tree element, folding results into text, matches, and subtrees.

@since 0.1.0.0
-}
innerTreeElemParser ::
    (ShowHTML a, Stream s m Char) =>
    Elem ->
    Maybe (ParsecT s u m a) ->
    ParsecT s u m (String, [a], [Tree ElemHead])
innerTreeElemParser el' matchh = do
    fmap (foldr foldFuncTrup mempty) $
        (try (string "/>") >> return [])
            <|> (try $ innerElemParser2 el' matchh)
            <|> (selfClosingTextful matchh)

{- | Alias for a list of subtrees.

@since 0.1.0.0
-}
type SubTree a = [Tree a]

{- | Unlike other element parsers, this does not call itself but 'innerParserSpecific'
instead loops with the given subtree structure.

@since 0.1.0.0
-}
treeElemParserSpecific ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    Elem ->
    [(String, String)] ->
    SubTree ElemHead ->
    ParsecT s u m (TreeHTML a)
treeElemParserSpecific match el' attrs' subTree = do
    (tag, attrsOut) <- parseOpeningTagDesc (Just [el']) attrs'
    _ <- char '>'
    (matchBook, _inText, _treees) <- innerParserSpecific match tag subTree
    return $ TreeHTML tag attrsOut matchBook [] -- TODO: propagate rawInner through innerParserSpecific

{- | Validate that all 'One' elements have been consumed from the given list.

@since 0.1.0.0
-}
validateGPR :: [Many (Tree ElemHead)] -> ParsecT s u m [HTMLMatcher TreeHTML a]
validateGPR manyElHeads =
    if length (filter (\case One _ -> True; _ -> False) manyElHeads) == 0
        then return []
        else parserFail "promised elements not found"

{- | Uses 'HTMLMatcher' to collect cases of HTML while parsing inside of a certain element.

@since 0.1.0.0
-}
htmlGenParserRepeat' ::
    (Stream s m Char, ShowHTML a) =>
    String ->
    Maybe (ParsecT s u m a) ->
    [Many (Tree ElemHead)] ->
    ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserRepeat' elemTag match manyElHeads =
    ((endTag elemTag) >> validateGPR manyElHeads)
        <|> liftA2 (:) (fmap Match $ try (fromMaybe parserZero match)) (htmlGenParserRepeat elemTag match manyElHeads)
        <|> liftA2 (:) (fmap IText $ try stylingElem) (htmlGenParserRepeat elemTag match manyElHeads)
        <|> try
            ( (treeElemParserSpecificContinuous match manyElHeads)
                >>= (\(sM, a) -> fmap ((Element a) :) (htmlGenParserRepeat elemTag match sM))
            )
        <|> liftA2 (:) ((IText . (: [])) <$> anyChar) (htmlGenParserRepeat elemTag match manyElHeads)

{- | Ordered element checker: checks off demanded elements so that when we parse the end tag,
we can verify all elements (ordered, parsed only in order) were found.

@since 0.1.0.0
-}
htmlGenParserRepeat ::
    (Stream s m Char, ShowHTML a) =>
    String ->
    Maybe (ParsecT s u m a) ->
    [Many (Tree ElemHead)] ->
    ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserRepeat elemTag match manyElHeadss =
    case manyElHeadss of
        [] -> do
            (htMMers, _) <- manyTill_ (htmlGenParserFlex match) (endTag elemTag)
            if null (filter (\case Element _ -> True; _ -> False) htMMers)
                then
                    return htMMers
                else
                    parserFail "extra elements"
        manyElHeads ->
            liftA2 (:) (fmap Match $ try (fromMaybe parserZero match)) (htmlGenParserRepeat elemTag match manyElHeads)
                <|> try
                    ( (treeElemParserSpecificContinuous match manyElHeads)
                        >>= (\(sM, a) -> fmap ((Element a) :) (htmlGenParserRepeat elemTag match sM))
                    )
                <|> liftA2 (:) (fmap (IText . (: [])) (specificChar' elemTag)) (htmlGenParserRepeat elemTag match manyElHeads)

{- | Parse a single character that is not the start of an HTML tag.

@since 0.1.0.0
-}
specificChar :: (Stream s m Char) => ParsecT s u m Char
specificChar = do
    notFollowedBy (parseOpeningTag Nothing [] >> char '>') <?> "error on specificChar (tag found)"
    anyChar

{- | This inner function uses the 'Many' datatype to differentiate between whether we should expect
to parse a single element with the given specs or allow for multiple of the given element specs in a row.
Used by 'treeElemParserSpecific' via 'innerParserSpecific'.

@since 0.1.0.0
-}
treeElemParserSpecificContinuous ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    [Many (Tree ElemHead)] ->
    ParsecT s u m ([Many (Tree ElemHead)], TreeHTML a)
treeElemParserSpecificContinuous match manyElHeads = do
    let
        elSet :: [Many (Tree ElemHead)]
        elSet = takeTill (\case One _ -> True; _ -> False) manyElHeads

    (e, parsedAttrs) <- parseOpeningTag (Just $ fmap (fst . rootLabel . fromMany) elSet) []

    (innerForest, outputStack) <- tryElHeads (e, parsedAttrs) elSet

    (m, _inTx, _inTr) <- innerParserSpecific match e innerForest
    let
        manyElHeads' :: [Many (Tree ElemHead)]
        manyElHeads' = drop ((length manyElHeads) - (length outputStack)) manyElHeads
    return $ (,) manyElHeads' (TreeHTML e parsedAttrs m []) -- TODO: propagate rawInner

{- | Subfunc of 'htmlGenParserContains'.
Accepts any element and if element is in the order of our checklist-of-elems, we give the tail of elems back.
If the tail reaches @[]@ before we hit the end tag then we are successful.

@since 0.1.0.0
-}
treeElemParserContains ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    [Many (Tree ElemHead)] ->
    ParsecT s u m ([Many (Tree ElemHead)], TreeHTML a)
treeElemParserContains match manyElHeads = do
    (e, ats) <- parseOpeningTag Nothing []
    let
        elSet = takeTill (\case One _ -> True; _ -> False) manyElHeads
    case tryElHeads' (e, ats) elSet of
        Right (innerForest, outputStack) -> do
            let
                manyElHeads' :: [Many (Tree ElemHead)]
                manyElHeads' = drop ((length manyElHeads) - (length outputStack)) manyElHeads
            if innerForest == []
                then do
                    case e `Prelude.elem` selfClosing of
                        True -> do
                            _ <- (try (string ">") <|> string "/>")
                            return $ (,) manyElHeads' (TreeHTML e ats mempty [])
                        False -> do
                            (m, _inTx, _inTr) <- innerParserContains match e innerForest
                            return $ (,) manyElHeads' (TreeHTML e ats m []) -- TODO: propagate rawInner
                else do
                    (m, _inTx, _inTr) <- innerParserContains match e innerForest
                    return $ (,) manyElHeads' (TreeHTML e ats m []) -- TODO: propagate rawInner
        Left _someError -> do
            (_inText, matchBook, _treees) <- innerTreeElemParser e match
            return $ (,) manyElHeads (TreeHTML e ats matchBook []) -- TODO: propagate rawInner

{- | Parse inner HTML content in a contains-style fashion, allowing extra elements
interspersed with demanded structure.

@since 0.1.0.0
-}
htmlGenParserContains ::
    (Stream s m Char, ShowHTML a) =>
    String ->
    Maybe (ParsecT s u m a) ->
    [Many (Tree ElemHead)] ->
    ParsecT s u m [HTMLMatcher TreeHTML a]
htmlGenParserContains elemTag match manyElHeadss =
    case manyElHeadss of
        [] -> do
            (htMMers, _) <- manyTill_ (htmlGenParserFlex match) (endTag elemTag)
            return htMMers
        manyElHeads ->
            liftA2 (:) (fmap Match $ try (fromMaybe parserZero match)) (htmlGenParserContains elemTag match manyElHeads)
                <|> liftA2 (:) (fmap IText $ try stylingElem) (htmlGenParserContains elemTag match manyElHeads)
                <|> try
                    ( (treeElemParserContains match manyElHeads)
                        >>= (\(sM, a) -> fmap ((Element a) :) (htmlGenParserContains elemTag match sM))
                    )
                <|> liftA2 (:) (fmap (IText . (: [])) (try $ specificChar' elemTag)) (htmlGenParserContains elemTag match manyElHeads)
                <|> ( endTag elemTag
                        >> case length (filter (\case One _ -> True; _ -> False) manyElHeadss) == 0 of
                            True -> return []
                            False -> parserFail $ "still havent yielded " <> show manyElHeadss
                    )

{- | Parse a character that is neither an opening tag nor the end tag for the given element.

@since 0.1.0.0
-}
specificChar' :: (Stream s m Char) => Elem -> ParsecT s u m Char
specificChar' elemTag = do
    notFollowedBy (parseOpeningTag Nothing [] >> char '>') <?> "error on specificChar' (tag found)"
    notFollowedBy (endTag elemTag)
    anyChar

{- | Parse inner content in a contains-style fashion for a specific element tag and subtree.

@since 0.1.0.0
-}
innerParserContains ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    Elem ->
    SubTree ElemHead ->
    ParsecT s u m ([a], String, [Tree ElemHead])
innerParserContains match tag subTree =
    case tag `Prelude.elem` selfClosing of
        True ->
            if not $ null subTree
                then undefined
                else do
                    _ <- (try (string ">") <|> string "/>")
                    return (mempty, mempty, mempty)
        False -> do
            _ <- char '>'
            x <- htmlGenParserContains tag match (reverse $ groupify subTree [])
            let
                (inText, matchBook, treees) = foldr foldFuncTrup mempty (x)
            return (matchBook, inText, treees)

{- | Very similar to 'treeElemParserSpecific' except that it allows for new nodes in the HTML DOM tree
to exist at random as long as when we resume parsing we still find all of the branches we found in the
'TreeHTML' given as an argument to this function.

@since 0.1.0.0
-}
similarTreeH ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    TreeHTML a ->
    ParsecT s u m (TreeHTML a)
similarTreeH matchh treeH = do
    (e, at) <- parseOpeningTag (Just $ [elTag treeH]) (((fmap . fmap) Just) . Map.toList $ attrs treeH)
    _ <- char '>'
    rawMatchers <- htmlGenParserContains e matchh (groupify (_innerTree' treeH) [])
    let (_inTx, m, _inTr) = foldr foldFuncTrup mempty rawMatchers
    return $ TreeHTML e at m rawMatchers

{- | Groups similar elements found via 'similarTreeH', requiring at least two matching elements.

@since 0.1.0.0
-}
htmlGroupSimilar ::
    (Stream s m Char, ShowHTML a) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (GroupHtml TreeHTML a)
htmlGroupSimilar elemOpts matchh attrsSubset =
    fmap mkGH $
        ( do
            treeH <- treeElemParser elemOpts matchh attrsSubset <* nl
            treeHs <- some $ try $ similarTreeH matchh treeH <* nl
            pure $ treeH : treeHs
        )

{- | Take elements from a list until a predicate matches (inclusive of the matching element).

@since 0.1.0.0
-}
takeTill :: (a -> Bool) -> [a] -> [a]
takeTill _ [] = []
takeTill f (x : xs) = if f x then x : [] else x : takeTill f xs

{- | Yields how many element heads are still worth trying against the given tag and attributes.

@since 0.1.0.0
-}
tryElHeads ::
    (Elem, Attrs) ->
    [Many (Tree ElemHead)] ->
    ParsecT s u m ([Tree ElemHead], [Many (Tree ElemHead)])
tryElHeads _ [] = parserFail "none of me opening tags worked laddy"
tryElHeads tagAttrs ((Many (Node label forest)) : outputStack) =
    if tagAttrs == label
        then return $ (forest, (Many (Node label forest)) : outputStack)
        else tryElHeads tagAttrs outputStack
tryElHeads tagAttrs ((One (Node label forest)) : outputStack) =
    if tagAttrs == label
        then return $ (forest, outputStack)
        else parserFail $ "missing element" <> show label

{- | Pure version of 'tryElHeads' returning 'Either' instead of using parser failure.

@since 0.1.0.0
-}
tryElHeads' ::
    (Elem, Attrs) ->
    [Many (Tree ElemHead)] ->
    Either String ([Tree ElemHead], [Many (Tree ElemHead)])
tryElHeads' _ [] = Left "none of me opening tags worked laddy"
tryElHeads' tagAttrs ((Many (Node label forest)) : outputStack) =
    if tagAttrs == label
        then return $ (forest, (Many (Node label forest)) : outputStack)
        else tryElHeads' tagAttrs outputStack
tryElHeads' tagAttrs ((One (Node label forest)) : outputStack) =
    if tagAttrs == label
        then return $ (forest, outputStack)
        else Left $ "missing element" <> show label

{- | Parse inner content for a specific element tag and subtree structure.

@since 0.1.0.0
-}
innerParserSpecific ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    Elem ->
    SubTree ElemHead ->
    ParsecT s u m ([a], String, [Tree ElemHead])
innerParserSpecific match tag subTree =
    case tag `Prelude.elem` selfClosing of
        True ->
            if not $ null subTree
                then undefined
                else do
                    _ <- (try (string ">") <|> string "/>")
                    return (mempty, mempty, mempty)
        False -> do
            x <- htmlGenParserRepeat tag match (reverse $ groupify subTree [])
            let
                (inText, matchBook, treees) = foldr foldFuncTrup mempty (x)
            return (matchBook, inText, treees)

{- | Parse multiple tree element heads based on the 'Many' wrapper.

@since 0.1.0.0
-}
{-# DEPRECATED multiTreeElemHeadParser "use specificContinuous style functions" #-}
multiTreeElemHeadParser ::
    (Stream s m Char, ShowHTML a) =>
    ParsecT s u m a ->
    Many (Tree ElemHead) ->
    ParsecT s u m [HTMLMatcher TreeHTML a]
multiTreeElemHeadParser match mTree = case mTree of
    Many (Node (el, at) subTree) ->
        (fmap . fmap) Element (many (try $ treeElemParserSpecific (Just match) el (Map.toList at) subTree))
    One (Node (el, at) subTree) ->
        treeElemParserSpecific (Just match) el (Map.toList at) subTree >>= return . flip (:) [] . Element

{- | Unwrap a 'Many' value to its contained element.

@since 0.1.0.0
-}
fromMany :: Many a -> a
fromMany (One a) = a
fromMany (Many a) = a

{- | Creates a simplified set of instructions for parsing a very specific tree structure
by grouping consecutive equal elements into 'Many' and singletons into 'One'.

@since 0.1.0.0
-}
groupify :: (Eq a) => [Tree a] -> [Many (Tree a)] -> [Many (Tree a)]
groupify [] acc = acc
groupify (tree : forest) [] = groupify forest (One tree : [])
groupify ((Node elemHead subForest) : forest) (mTree : acc) =
    case mTree of
        One (Node elemHeadPrev subForestPrev) ->
            if elemHead == elemHeadPrev
                then groupify forest $ ((Many (Node elemHeadPrev subForestPrev)) : acc)
                else groupify forest $ ((One (Node elemHead subForest)) : (One (Node elemHeadPrev subForestPrev)) : acc)
        Many (Node elemHeadPrev subForestPrev) ->
            if elemHead == elemHeadPrev
                then groupify forest ((Many (Node elemHeadPrev subForestPrev)) : acc)
                else
                    groupify
                        forest
                        ( (One (Node elemHead subForest))
                            : (Many (Node elemHeadPrev subForestPrev))
                            : acc
                        )

{- | Returns a minimum of 2 elements. Can generalize to 'ElementRep'.

@since 0.1.0.0
-}
htmlGroup ::
    (Stream s m Char, ShowHTML a) =>
    Maybe [Elem] ->
    Maybe (ParsecT s u m a) ->
    [(String, Maybe String)] ->
    ParsecT s u m (GroupHtml TreeHTML a)
htmlGroup elemOpts matchh attrsSubset =
    fmap mkGH $
        try
            ( treeElemParser elemOpts matchh attrsSubset
                >>= (\treeH -> fmap (treeH :) (some (try $ sameTreeH matchh treeH)))
            )

{- | HTML table group parser for @\<tr\>@ elements.

@since 0.1.0.0
-}
table :: (Stream s m Char) => ParsecT s u m (GroupHtml TreeHTML String)
table = htmlGroup (Just ["tr"]) Nothing []

{- | Tags that do not change the DOM structure, only text styling (like MS Word formatting).

@since 0.1.0.0
-}
stylingTags :: [String]
stylingTags = ["abbr", "b", "big", "acronym", "dfn", "em", "font", "i", "mark", "q", "small"]

{- | Parse a styling element and return its inner text content.

@since 0.1.0.0
-}
stylingElem :: (Stream s m Char) => ParsecT s u m String
stylingElem = do
    (e, _) <- parseOpeningTag (Just stylingTags) []
    _ <- char '>'
    fmap fst $ manyTill_ anyChar (endTag e)

{- | Interface to find the same element structure as the given 'TreeHTML'.

@since 0.1.0.0
-}
sameTreeH ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    TreeHTML a ->
    ParsecT s u m (TreeHTML a)
sameTreeH matchh treeH = treeElemParserSpecific matchh (elTag treeH) (Map.toList $ attrs treeH) (_innerTree' treeH)

{- | Flexible HTML parser that tries matching, element parsing, or single character consumption.

@since 0.1.0.0
-}
htmlGenParserFlex ::
    (Stream s m Char, ShowHTML a) =>
    Maybe (ParsecT s u m a) ->
    ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParserFlex a =
    (try (Match <$> (fromMaybe parserZero a)))
        <|> try (Element <$> treeElemParser Nothing a [])
        <|> ((IText . (: [])) <$> anyChar)

{- | General HTML parser combinator for matching, element parsing, or text consumption.

@since 0.1.0.0
-}
htmlGenParser ::
    (Stream s m Char, ShowHTML a) =>
    ParsecT s u m a ->
    ParsecT s u m (TreeHTML a) ->
    ParsecT s u m (HTMLMatcher TreeHTML a)
htmlGenParser a parseTreeH =
    (Match <$> try a)
        <|> (Element <$> try parseTreeH)
        <|> (fmap (IText . (: [])) anyChar)

{- | Library function for when you want an exact match. If 3 of @ElemHead A@ then it looks for 3 @ElemHead A@.

@since 0.1.0.0
-}
{-# DEPRECATED specificForest "you likely need specificRepetitiveForest" #-}
specificForest ::
    (Stream s m Char, ShowHTML a) =>
    [Tree ElemHead] ->
    ParsecT s u m a ->
    ParsecT s u m [HTMLMatcher TreeHTML a]
specificForest [] _ = return []
specificForest (x : xs) match = do
    y <- htmlGenParser match (nodeToTreeElemExpr x match)
    ys <- case y of
        Element _ -> specificForest xs match
        _ -> specificForest (x : xs) match
    return (y : ys)

{- | Convert a tree node into a 'treeElemParserSpecific' expression.

@since 0.1.0.0
-}
nodeToTreeElemExpr ::
    (Stream s m Char, ShowHTML a) =>
    Tree ElemHead ->
    ParsecT s u m a ->
    ParsecT s u m (TreeHTML a)
nodeToTreeElemExpr (Node (el', at') subTree) match =
    treeElemParserSpecific (Just match) el' (Map.toList at') subTree

{- | Used by 'treeElemParser'' to parse inner element content.

@since 0.1.0.0
-}
innerElemParser2 ::
    (ShowHTML a, Stream s m Char) =>
    String ->
    Maybe (ParsecT s u m a) ->
    ParsecT s u m [HTMLMatcher TreeHTML a]
innerElemParser2 eTag innerSpec =
    char '>'
        >> manyTill
            ( try (Match <$> (fromMaybe parserZero innerSpec))
                <|> (try (IText <$> stylingElem))
                <|> try (Element <$> treeElemParser' Nothing innerSpec [])
                <|> ((IText . (: [])) <$> anyChar)
            )
            (try $ endTag eTag)

{- | Parse any tree element with no constraints on tag or attributes.

@since 0.1.0.0
-}
treeElemParserAnyInside :: (Stream s m Char, ShowHTML a) => Maybe (ParsecT s u m a) -> ParsecT s u m (TreeHTML a)
treeElemParserAnyInside match = treeElemParser Nothing match []

{- | Parse any HTML group with no element/attribute constraints.

@since 0.1.0.0
-}
anyHtmlGroup :: (ShowHTML a, Stream s m Char) => ParsecT s u m (GroupHtml TreeHTML a)
anyHtmlGroup = htmlGroup Nothing Nothing []

{- | Find all spatially mutually exclusive groups in the HTML.

@since 0.1.0.0
-}
findAllSpaceMutExGroups :: (ShowHTML a, Stream s m Char) => ParsecT s u m (Maybe [GroupHtml TreeHTML a])
findAllSpaceMutExGroups = findNaive anyHtmlGroup

{- | Placeholder for finding all mutually exclusive groups. Currently unimplemented.

@since 0.1.0.0
-}
findAllMutExGroups' :: a
findAllMutExGroups' = undefined
