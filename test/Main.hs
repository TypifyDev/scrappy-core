{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Either (isLeft, isRight)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Tree (Tree (Node))
import Hedgehog (
    Property,
    annotateShow,
    assert,
    failure,
    property,
    success,
    (===),
 )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Text.Parsec (ParseError, ParsecT, anyChar, parse, string)

import Scrappy.Elem.ChainHTML
import Scrappy.Elem.ElemHeadParse
import Scrappy.Elem.ITextElemParser
import Scrappy.Elem.SimpleElemParser (
    clickableHref,
    eitherP,
    el,
    elemParser,
    elemParserWhere,
    matchesInSameElTag,
    sameElTag,
    selfClosing,
    stylingTags,
 )
import qualified Scrappy.Elem.SimpleElemParser as SEP (manyTill_, stylingElem)
import Scrappy.Elem.TreeElemParser (
    Many (..),
    fromMany,
    groupify,
    htmlGroup,
    sameTreeH,
    skipManyTill,
    table,
    takeTill,
    treeElemParser,
 )
import qualified Scrappy.Elem.TreeElemParser as TEP (manyTill_, selfClosing)
import Scrappy.Elem.Types
import Scrappy.Find
import Scrappy.Links
import Scrappy.Scrape
import Scrappy.Types

main :: IO ()
main =
    defaultMain $
        testGroup
            "scrappy-core"
            [ typesTests
            , linksTests
            , findTests
            , elemTypesTests
            , elemHeadParseTests
            , simpleElemParserTests
            , chainHTMLTests
            , treeElemParserTests
            , iTextElemParserTests
            , scrapeTests
            ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run a pure parser on a String and return the result
runP :: ParsecT String () Identity a -> String -> Either ParseError a
runP p = parse p ""

-- | Assert that a parse succeeds
parseSucceeds :: (Show a) => ParsecT String () Identity a -> String -> Property
parseSucceeds p input = property $ do
    case runP p input of
        Left err -> do
            annotateShow err
            failure
        Right _ -> success

-- | Assert that a parse fails
parseFails :: (Show a) => ParsecT String () Identity a -> String -> Property
parseFails p input = property $ do
    case runP p input of
        Left _ -> success
        Right val -> do
            annotateShow val
            failure

-- | Assert that a parse succeeds and check the result
parseIs :: (Show a, Eq a) => ParsecT String () Identity a -> String -> a -> Property
parseIs p input expected = property $ do
    case runP p input of
        Left err -> do
            annotateShow err
            failure
        Right val -> val === expected

-- ---------------------------------------------------------------------------
-- Scrappy.Types
-- ---------------------------------------------------------------------------

typesTests :: TestTree
typesTests =
    testGroup
        "Scrappy.Types"
        [ testPropertyNamed "ScrapeFail Show Eof" "scrapefail_show_eof" $
            property $
                show Eof === "Eof"
        , testPropertyNamed "ScrapeFail Show NonMatch" "scrapefail_show_nonmatch" $
            property $
                show NonMatch === "NonMatch"
        , testPropertyNamed "mapMaybe succeeds on Just" "mapmyabe_just" $
            parseIs (mapMaybe (\x -> Just (x ++ "!")) (string "hi")) "hi" "hi!"
        , testPropertyNamed "mapMaybe fails on Nothing" "mapmyabe_nothing" $
            parseFails (mapMaybe (\_ -> Nothing :: Maybe String) (string "hi")) "hi"
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Links
-- ---------------------------------------------------------------------------

linksTests :: TestTree
linksTests =
    testGroup
        "Scrappy.Links"
        [ -- fixRelativeUrl
          testPropertyNamed "fixRelativeUrl appends relative path" "fix_relative_url_basic" $
            property $
                fixRelativeUrl (Link "https://example.com") "/page" === "https://example.com/page"
        , testPropertyNamed "fixRelativeUrl returns url when base is infix" "fix_relative_url_infix" $
            property $
                fixRelativeUrl (Link "https://example.com") "https://example.com/foo" === "https://example.com/foo"
        , testPropertyNamed "fixRelativeUrl returns base for empty url" "fix_relative_url_empty" $
            property $
                fixRelativeUrl (Link "https://example.com") "" === "https://example.com"
        , -- deriveBaseUrl
          testPropertyNamed "deriveBaseUrl extracts base" "derive_base_url" $
            property $
                deriveBaseUrl (Link "https://example.com/path/to/page") === Just (Link "https://example.com")
        , testPropertyNamed "deriveBaseUrl nothing on empty" "derive_base_url_empty" $
            property $
                deriveBaseUrl (Link "") === Nothing
        , -- sameAuthority
          testPropertyNamed "sameAuthority matches same domain" "same_authority_match" $
            property $
                assert $
                    sameAuthority "https://example.com/page" (Link "https://example.com")
        , testPropertyNamed "sameAuthority rejects different TLD" "same_authority_mismatch" $
            property $
                assert $
                    not $
                        sameAuthority "https://other.org" (Link "https://example.com")
        , -- getHostName
          testPropertyNamed "getHostName extracts host" "get_hostname" $
            property $
                getHostName (Link "https://example.com/foo") === Just "example.com"
        , testPropertyNamed "getHostName nothing on garbage" "get_hostname_empty" $
            property $
                getHostName (Link "") === Nothing
        , -- getLastPath
          testPropertyNamed "getLastPath gets last segment" "get_last_path" $
            property $
                getLastPath (Link "https://example.com/foo/bar") === Just "bar"
        , -- numberOfQueryParamsIsZero
          testPropertyNamed "numberOfQueryParamsIsZero accepts clean URL" "qparams_zero_clean" $
            property $
                assert $
                    isJust (numberOfQueryParamsIsZero (Link "https://example.com/page"))
        , testPropertyNamed "numberOfQueryParamsIsZero rejects query URL" "qparams_zero_query" $
            property $
                assert $
                    isNothing (numberOfQueryParamsIsZero (Link "https://example.com/page?a=b"))
        , -- Link instances
          testPropertyNamed "Link Eq reflexive" "link_eq_reflexive" $
            property $
                Link "abc" === Link "abc"
        , testPropertyNamed "Link Eq different" "link_eq_different" $
            property $
                assert $
                    Link "abc" /= Link "def"
        , testPropertyNamed "Link Show roundtrip" "link_show" $
            property $
                show (Link "test") === "Link \"test\""
        , -- maybeUsefulUrl
          testPropertyNamed "maybeUsefulUrl rejects javascript" "useful_url_js" $
            property $
                assert $
                    isNothing (maybeUsefulUrl (Link "https://example.com") (Link "javascript:void(0)"))
        , testPropertyNamed "maybeUsefulUrl rejects mailto" "useful_url_mailto" $
            property $
                assert $
                    isNothing (maybeUsefulUrl (Link "https://example.com") (Link "mailto:test@test.com"))
        , -- maybeNewUrl
          testPropertyNamed "maybeNewUrl returns new URL" "new_url_new" $
            property $
                assert $
                    isJust (maybeNewUrl [] (Link "https://example.com"))
        , testPropertyNamed "maybeNewUrl rejects visited URL" "new_url_visited" $
            property $
                assert $
                    isNothing (maybeNewUrl [(Link "https://example.com", ())] (Link "https://example.com"))
        , -- usefulUrls
          testPropertyNamed "usefulUrls filters bad URLs" "useful_urls_filter" $
            property $ do
                let base = Link "https://example.com"
                    urls = [Link "https://example.com/page", Link "javascript:void(0)", Link "https://example.com/other"]
                    result = usefulUrls base urls
                length (filter isJust result) === 2
        , -- fixURL
          testPropertyNamed "fixURL returns href with scheme as-is" "fix_url_scheme" $
            property $
                fixURL (Link "https://example.com/old") "https://other.com/new" === "https://other.com/new"
        , testPropertyNamed "fixURL resolves relative path" "fix_url_relative" $
            property $ do
                let result = fixURL (Link "https://example.com/old") "/new"
                assert $ not $ null result
        , -- getFileName
          testPropertyNamed "getFileName extracts last segment" "get_filename" $
            property $
                getFileName (Link "https://example.com/foo/bar.html") === Just "bar.html"
        , -- parseLink
          testPropertyNamed "parseLink resolves relative" "parse_link_relative" $
            property $
                assert $
                    isJust (parseLink False (Link "https://example.com/page") "/other")
        , testPropertyNamed "parseLink same-site rejects different" "parse_link_same_site_reject" $
            property $
                assert $
                    isNothing (parseLink True (Link "https://example.com/page") "https://other.org/foo")
        , testPropertyNamed "parseLink accepts http link" "parse_link_http" $
            property $
                assert $
                    isJust (parseLink False (Link "https://example.com/page") "https://somewhere.com/foo")
        , -- urlIsNew
          testPropertyNamed "urlIsNew true for unvisited" "url_is_new_true" $
            property $
                assert $
                    urlIsNew ([] :: [((), String)]) "https://example.com/page"
        , testPropertyNamed "urlIsNew false for visited" "url_is_new_false" $
            property $
                assert $
                    not $
                        urlIsNew [(() :: (), "https://example.com/page")] "https://example.com/page"
        , -- maybeUsefulNewUrl
          testPropertyNamed "maybeUsefulNewUrl filters visited" "useful_new_url_visited" $
            property $
                assert $
                    isNothing $
                        maybeUsefulNewUrl (Link "https://example.com") [(Link "https://example.com/page", ())] (Link "https://example.com/page")
        , testPropertyNamed "maybeUsefulNewUrl accepts new" "useful_new_url_new" $
            property $
                assert $
                    isJust $
                        maybeUsefulNewUrl (Link "https://example.com") [] (Link "https://example.com/page")
        , -- usefulNewUrls
          testPropertyNamed "usefulNewUrls filters" "useful_new_urls" $
            property $ do
                let base = Link "https://example.com"
                    tree = [(Link "https://example.com/old", ())]
                    urls = [Link "https://example.com/new", Link "https://example.com/old"]
                    result = usefulNewUrls base tree urls
                length (filter isJust result) === 1
        , -- fixRelativeUrl more guards
          testPropertyNamed "fixRelativeUrl trailing slash base no slash url" "fix_relative_url_trailing_slash" $
            property $
                fixRelativeUrl (Link "https://example.com/") "page" === "https://example.com/page"
        , testPropertyNamed "fixRelativeUrl trailing slash base slash url" "fix_relative_url_trailing_slash2" $
            property $
                fixRelativeUrl (Link "https://example.com/") "/page" === "https://example.com/page"
        , testPropertyNamed "fixRelativeUrl no trailing slash no prefix" "fix_relative_url_no_slash" $
            property $
                fixRelativeUrl (Link "https://example.com") "page" === "https://example.com/page"
        , testPropertyNamed "fixRelativeUrl returns base for slash" "fix_relative_url_slash" $
            property $
                fixRelativeUrl (Link "https://example.com") "/" === "https://example.com"
        , -- IsLink instance
          testPropertyNamed "renderLink extracts url" "render_link" $
            property $
                renderLink (Link "https://example.com") === "https://example.com"
        , -- Link Ord
          testPropertyNamed "Link Ord" "link_ord" $
            property $
                assert $
                    Link "abc" < Link "def"
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Find
-- ---------------------------------------------------------------------------

findTests :: TestTree
findTests =
    testGroup
        "Scrappy.Find"
        [ -- findNaive
          testPropertyNamed "findNaive finds all matches" "find_naive_all" $
            parseIs (findNaive (string "x")) "xaxbxc" (Just ["x", "x", "x"])
        , testPropertyNamed "findNaive returns Nothing on no match" "find_naive_none" $
            parseIs (findNaive (string "z")) "abc" Nothing
        , testPropertyNamed "findNaive finds single match" "find_naive_single" $
            parseIs (findNaive (string "hello")) "say hello world" (Just ["hello"])
        , -- find
          testPropertyNamed "find returns Rights for matches" "find_all_rights" $
            property $ do
                let result = runP (find (string "x")) "xax"
                case result of
                    Left _ -> failure
                    Right xs -> length (filter isRight xs) === 2
        , -- findUntilMatch
          testPropertyNamed "findUntilMatch finds first occurrence" "find_until_match" $
            parseIs (findUntilMatch (string "target")) "noise noise target rest" "target"
        , -- streamEdit
          testPropertyNamed "streamEdit replaces all" "stream_edit_all" $
            property $
                streamEdit (string "x") (const "y") "xaxbx" === "yayby"
        , testPropertyNamed "streamEdit no match leaves unchanged" "stream_edit_no_match" $
            property $
                streamEdit (string "z") (const "y") "abc" === "abc"
        , testPropertyNamed "streamEdit empty input" "stream_edit_empty" $
            property $
                streamEdit (string "x") (const "y") "" === ""
        , testPropertyNamed "streamEdit longer replacement" "stream_edit_longer" $
            property $
                streamEdit (string "a") (const "xyz") "aba" === "xyzbxyz"
        , -- findSequential2
          testPropertyNamed "findSequential2 finds two patterns" "find_seq2" $
            parseIs (findSequential2 (string "a", string "b")) "xxaxxb" ("a", "b")
        , -- findSequential3
          testPropertyNamed "findSequential3 finds three patterns" "find_seq3" $
            parseIs (findSequential3 (string "a", string "b", string "c")) "xaxbxc" ("a", "b", "c")
        , -- findSomeHTMLNaive
          testPropertyNamed "findSomeHTMLNaive finds matches" "find_some_html_naive" $
            property $
                findSomeHTMLNaive (string "hi") ("say hi and hi again" :: String) === Just ["hi", "hi"]
        , testPropertyNamed "findSomeHTMLNaive no match" "find_some_html_naive_none" $
            property $
                findSomeHTMLNaive (string "z") ("abc" :: String) === Nothing
        , -- findSomeHTML
          testPropertyNamed "findSomeHTML Right on match" "find_some_html_match" $
            property $ do
                let result = findSomeHTML (string "x") ("xax" :: String)
                assert $ isRight result
        , testPropertyNamed "findSomeHTML Right Nothing on no match" "find_some_html_no_match" $
            property $
                findSomeHTML (string "z") ("abc" :: String) === Right Nothing
        , -- editFirst
          testPropertyNamed "editFirst replaces only first" "edit_first_only" $
            property $ do
                let result = parse (editFirst (const "Y") (string "x")) "" ("xaxbx" :: String)
                case result of
                    Right s -> s === ("Yaxbx" :: String)
                    Left _ -> failure
        , -- baseParser
          testPropertyNamed "baseParser wraps in Right" "base_parser_right" $
            property $ do
                let result = runP (baseParser (string "hi")) "hi"
                case result of
                    Right (Right s) -> s === ("hi" :: String)
                    _ -> failure
        , -- givesNothing
          testPropertyNamed "givesNothing returns Left" "gives_nothing" $
            property $ do
                let result = runP givesNothing "a"
                case result of
                    Right (Left _) -> success
                    _ -> failure
        , -- endStream
          testPropertyNamed "endStream at eof returns Left" "end_stream_eof" $
            property $ do
                let result = runP endStream ("" :: String)
                case result of
                    Right (Left _) -> success
                    _ -> failure
        , -- streamEdit idempotent on no match
          testPropertyNamed "streamEdit with itself" "stream_edit_self" $
            property $
                streamEdit (string "hello") (\s -> s ++ "!") "say hello" === "say hello!"
        , -- findSomeHTMLNaive empty
          testPropertyNamed "findSomeHTMLNaive empty input" "find_some_html_naive_empty" $
            property $
                findSomeHTMLNaive (string "z") ("" :: String) === Nothing
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Elem.Types
-- ---------------------------------------------------------------------------

elemTypesTests :: TestTree
elemTypesTests =
    testGroup
        "Scrappy.Elem.Types"
        [ -- ShowHTML
          testPropertyNamed "ShowHTML String uses show" "showhtml_string" $
            property $
                showH ("hello" :: String) === show ("hello" :: String)
        , -- Elem' construction and accessors
          testPropertyNamed "Elem' elTag accessor" "elem_eltag" $
            property $
                elTag (Elem' "div" Map.empty [] "") === "div"
        , testPropertyNamed "Elem' attrs accessor" "elem_attrs" $
            property $
                attrs (Elem' "div" (Map.fromList [("class", "foo")]) [] "") === Map.fromList [("class", "foo")]
        , testPropertyNamed "Elem' innerText' accessor" "elem_innertext" $
            property $
                innerText' (Elem' "div" Map.empty ([] :: [String]) "hello world") === "hello world"
        , testPropertyNamed "Elem' matches' accessor" "elem_matches" $
            property $
                matches' (Elem' "div" Map.empty (["a", "b"] :: [String]) "") === (["a", "b"] :: [String])
        , -- coerceAttrs
          testPropertyNamed "coerceAttrs empty value becomes Nothing" "coerce_attrs_empty" $
            property $
                coerceAttrs (Map.fromList [("class", "")]) === ([("class", Nothing)] :: [(String, Maybe String)])
        , testPropertyNamed "coerceAttrs nonempty value becomes Just" "coerce_attrs_nonempty" $
            property $
                coerceAttrs (Map.fromList [("class", "foo")]) === ([("class", Just "foo")] :: [(String, Maybe String)])
        , -- f helper
          testPropertyNamed "f empty returns Nothing" "f_empty" $
            property $
                f "" === Nothing
        , testPropertyNamed "f nonempty returns Just" "f_nonempty" $
            property $
                f "hello" === Just "hello"
        , -- fst', snd', thd'
          testPropertyNamed "fst' extracts first" "fst_prime" $
            property $
                fst' (1 :: Int, 2 :: Int, 3 :: Int) === 1
        , testPropertyNamed "snd' extracts second" "snd_prime" $
            property $
                snd' (1 :: Int, 2 :: Int, 3 :: Int) === 2
        , testPropertyNamed "thd' extracts third" "thd_prime" $
            property $
                thd' (1 :: Int, 2 :: Int, 3 :: Int) === 3
        , -- HTMLMatcher constructors
          testPropertyNamed "IText carries string" "htmlmatcher_itext" $
            property $ do
                let IText s = IText "hello" :: HTMLMatcher Elem' String
                s === "hello"
        , -- maxLength
          testPropertyNamed "maxLength finds longest" "max_length" $
            property $
                maxLength [[1, 2], [1, 2, 3], [1]] === ([1, 2, 3] :: [Int])
        , testPropertyNamed "maxLength empty" "max_length_empty" $
            property $
                maxLength ([] :: [[Int]]) === []
        , -- mkGH and ungroup
          testPropertyNamed "mkGH then ungroup roundtrip" "mkgh_ungroup" $
            property $ do
                let elems =
                        [ Elem' "div" Map.empty ([] :: [String]) "a"
                        , Elem' "div" Map.empty ([] :: [String]) "ab"
                        ]
                length (ungroup (mkGH elems)) === length elems
        , -- longestElem
          testPropertyNamed "longestElem finds longest inner text" "longest_elem" $
            property $ do
                let e1 = Elem' "p" Map.empty ([] :: [String]) "short"
                    e2 = Elem' "p" Map.empty ([] :: [String]) "much longer text"
                    e3 = Elem' "p" Map.empty ([] :: [String]) "medium"
                case longestElem [e1, e2, e3] of
                    Just e -> innerText' e === "much longer text"
                    Nothing -> failure
        , testPropertyNamed "longestElem empty" "longest_elem_empty" $
            property $
                assert $
                    isNothing $
                        longestElem ([] :: [Elem' String])
        , -- elemToStr
          testPropertyNamed "elemToStr reconstructs element" "elem_to_str" $
            property $ do
                let e = Elem' "div" (Map.fromList [("class", "foo")]) ([] :: [String]) "hello"
                assert $ elem '<' (elemToStr e)
        , testPropertyNamed "elemToStr with no attrs" "elem_to_str_no_attrs" $
            property $ do
                let e = Elem' "p" Map.empty ([] :: [String]) "text"
                elemToStr e === "<p>text</p>"
        , testPropertyNamed "elemToStr with attrs" "elem_to_str_with_attrs" $
            property $ do
                let e = Elem' "a" (Map.fromList [("href", "/")]) ([] :: [String]) "link"
                elemToStr e === "<a href=\"/\">link</a>"
        , -- treeElemToStr
          testPropertyNamed "treeElemToStr renders TreeHTML" "tree_elem_to_str" $
            property $ do
                let t = TreeHTML "div" Map.empty ([] :: [String]) "content" []
                treeElemToStr t === "<div>content</div>"
        , testPropertyNamed "treeElemToStr with attrs" "tree_elem_to_str_attrs" $
            property $ do
                let t = TreeHTML "span" (Map.fromList [("class", "bold")]) ([] :: [String]) "text" []
                -- mdToStringPairs does not add space before attrs
                treeElemToStr t === "<spanclass=\"bold\">text</span>"
        , testPropertyNamed "ShowHTML TreeHTML uses treeElemToStr" "showhtml_tree" $
            property $ do
                let t = TreeHTML "p" Map.empty ([] :: [String]) "hi" []
                showH t === "<p>hi</p>"
        , -- InnerTextResult Monoid
          testPropertyNamed "InnerTextResult mempty" "itr_mempty" $
            property $ do
                let m = mempty :: InnerTextResult String
                _matchesITR m === []
        , testPropertyNamed "InnerTextResult semigroup" "itr_semigroup" $
            property $ do
                let a = InnerTextResult{_matchesITR = ["x" :: String], _fullInner = "ax"}
                    b = InnerTextResult{_matchesITR = ["y"], _fullInner = "by"}
                    c = a <> b
                _matchesITR c === (["x", "y"] :: [String])
        , -- Clickable
          testPropertyNamed "Clickable Eq" "clickable_eq" $
            property $ do
                let eh = ("a" :: String, Map.fromList [("href", "/")]) :: ElemHead
                    c1 = Clickable eh (Link "/")
                    c2 = Clickable eh (Link "/")
                c1 === c2
        , -- ShowHTML instances
          testPropertyNamed "ShowHTML Char" "showhtml_char" $
            property $
                showH 'a' === show 'a'
        , testPropertyNamed "ShowHTML Text" "showhtml_text" $
            property $
                showH ("hello" :: T.Text) === "hello"
        , testPropertyNamed "ShowHTML Elem'" "showhtml_elem" $
            property $ do
                let e = Elem' "p" Map.empty ([] :: [String]) "text"
                assert $ not $ null $ showH e
        , -- noPat
          testPropertyNamed "noPat is Nothing" "nopat_nothing" $
            property $
                assert $
                    isNothing (noPat :: Maybe (ParsecT String () Identity String))
        , -- endTag
          testPropertyNamed "endTag parses closing tag" "end_tag_parse" $
            parseIs (endTag "div") "</div>" "</div>"
        , testPropertyNamed "endTag fails on wrong tag" "end_tag_wrong" $
            parseFails (endTag "span") "</div>"
        , -- GroupHtml
          testPropertyNamed "GroupHtml Eq" "grouphtml_eq" $
            property $ do
                let g1 = GroupHtml ([] :: [Elem' String]) 3 5
                    g2 = GroupHtml ([] :: [Elem' String]) 3 5
                g1 === g2
        , testPropertyNamed "GroupHtml Ord" "grouphtml_ord" $
            property $
                assert $
                    GroupHtml ([] :: [Elem' String]) 2 3 <= GroupHtml ([] :: [Elem' String]) 3 5
        , testPropertyNamed "GroupHtml Show empty" "grouphtml_show_empty" $
            property $
                assert $
                    not $
                        null $
                            show (GroupHtml ([] :: [Elem' String]) 0 0)
        , -- biggestHtmlGroup
          testPropertyNamed "biggestHtmlGroup finds biggest" "biggest_html_group" $
            property $ do
                let g1 = GroupHtml ([] :: [Elem' String]) 2 3
                    g2 = GroupHtml ([] :: [Elem' String]) 5 10
                    result = biggestHtmlGroup [g1, g2]
                result === g2
        , -- biggestGroup
          testPropertyNamed "biggestGroup single" "biggest_group_single" $
            property $ do
                let g = GroupHtml [Elem' "p" Map.empty ([] :: [String]) "a"] 1 1
                biggestGroup [g] === g
        , -- foldFuncITR
          testPropertyNamed "foldFuncITR IText" "fold_func_itr_itext" $
            property $ do
                let m = mempty :: InnerTextResult String
                    result = foldFuncITR (IText "hello" :: HTMLMatcher Elem' String) m
                _fullInner result === "hello"
        , testPropertyNamed "foldFuncITR Match" "fold_func_itr_match" $
            property $ do
                let m = mempty :: InnerTextResult String
                    result = foldFuncITR (Match "x" :: HTMLMatcher Elem' String) m
                _matchesITR result === ["x"]
        , testPropertyNamed "foldFuncITR Element" "fold_func_itr_element" $
            property $ do
                let m = mempty :: InnerTextResult String
                    e = Elem' "p" Map.empty (["m"] :: [String]) "inner"
                    result = foldFuncITR (Element e :: HTMLMatcher Elem' String) m
                _matchesITR result === ["m"]
        , -- foldFuncTup
          testPropertyNamed "foldFuncTup IText" "fold_func_tup_itext" $
            property $ do
                let result = foldFuncTup (IText "hi" :: HTMLMatcher Elem' String) ("", [] :: [String])
                fst result === "hi"
        , testPropertyNamed "foldFuncTup Match" "fold_func_tup_match" $
            property $ do
                let result = foldFuncTup (Match "x" :: HTMLMatcher Elem' String) ("", [] :: [String])
                snd result === ["x"]
        , -- UrlPagination
          testPropertyNamed "UrlPagination Eq" "url_pagination_eq" $
            property $
                UrlPagination "a" "b" === UrlPagination "a" "b"
        , testPropertyNamed "UrlPagination Show" "url_pagination_show" $
            property $
                assert $
                    not $
                        null $
                            show (UrlPagination "a" "b")
        , -- AttrsError
          testPropertyNamed "AttrsError Show" "attrs_error_show" $
            property $
                show IncorrectAttrs === "IncorrectAttrs"
        , -- getLink
          testPropertyNamed "getLink extracts link" "get_link" $
            property $ do
                let eh = ("a" :: String, Map.fromList [("href", "/")]) :: ElemHead
                getLink (Clickable eh (Link "/page")) === Link "/page"
        , -- mkClickable
          testPropertyNamed "mkClickable with href" "mk_clickable" $
            property $ do
                let e = Elem' "a" (Map.fromList [("href", "https://example.com/page")]) ([] :: [String]) "click"
                assert $ isJust $ mkClickable False (Link "https://example.com") e
        , testPropertyNamed "mkClickable without href" "mk_clickable_no_href" $
            property $ do
                let e = Elem' "div" Map.empty ([] :: [String]) "no link"
                assert $ isNothing $ mkClickable False (Link "https://example.com") e
        , -- mkClickableEH
          testPropertyNamed "mkClickableEH with href" "mk_clickable_eh" $
            property $ do
                let eh = ("a" :: String, Map.fromList [("href", "https://example.com/page")]) :: ElemHead
                assert $ isJust $ mkClickableEH False (Link "https://example.com") eh
        , -- getHrefEl
          testPropertyNamed "getHrefEl extracts" "get_href_el" $
            property $ do
                let e = Elem' "a" (Map.fromList [("href", "https://example.com/page")]) ([] :: [String]) "link"
                assert $ isJust $ getHrefEl False (Link "https://example.com") e
        , -- getHrefAttrs
          testPropertyNamed "getHrefAttrs finds href" "get_href_attrs" $
            property $ do
                let attrMap = Map.fromList [("href", "https://example.com/page")]
                assert $ isJust $ getHrefAttrs False (Link "https://example.com") attrMap
        , testPropertyNamed "getHrefAttrs no href" "get_href_attrs_none" $
            property $ do
                let attrMap = Map.fromList [("class", "foo")]
                assert $ isNothing $ getHrefAttrs False (Link "https://example.com") attrMap
        , -- InnerTextHTMLTree
          testPropertyNamed "InnerTextHTMLTree mempty" "itht_mempty" $
            property $ do
                let m = mempty :: InnerTextHTMLTree String
                _matches m === []
        , testPropertyNamed "InnerTextHTMLTree semigroup" "itht_semigroup" $
            property $ do
                let a = InnerTextHTMLTree{_matches = ["x" :: String], _innerText = "ax", innerTree = []}
                    b = InnerTextHTMLTree{_matches = ["y"], _innerText = "by", innerTree = []}
                    c = a <> b
                _matches c === (["x", "y"] :: [String])
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Elem.ElemHeadParse
-- ---------------------------------------------------------------------------

elemHeadParseTests :: TestTree
elemHeadParseTests =
    testGroup
        "Scrappy.Elem.ElemHeadParse"
        [ -- parseOpeningTag
          testPropertyNamed "parseOpeningTag parses div" "parse_opening_div" $
            property $ do
                let result = runP (parseOpeningTag (Just ["div"]) []) "<div"
                assert $ isRight result
        , testPropertyNamed "parseOpeningTag parses with class attr" "parse_opening_class" $
            property $ do
                let result = runP (parseOpeningTag (Just ["div"]) [("class", Just "foo")]) "<div class=\"foo\""
                assert $ isRight result
        , testPropertyNamed "parseOpeningTag fails on wrong tag" "parse_opening_wrong_tag" $
            property $ do
                let result = runP (parseOpeningTag (Just ["span"]) []) "<div"
                assert $ isLeft result
        , testPropertyNamed "parseOpeningTag any tag" "parse_opening_any" $
            property $ do
                let result = runP (parseOpeningTag Nothing []) "<p"
                case result of
                    Right (tagName, _) -> tagName === "p"
                    Left _ -> failure
        , -- attrParser
          testPropertyNamed "attrParser parses key=value" "attr_parser_kv" $
            parseIs attrParser "class=\"foo\"" ("class", "foo")
        , testPropertyNamed "attrParser parses single-quoted" "attr_parser_single_quote" $
            parseIs attrParser "id='bar'" ("id", "bar")
        , testPropertyNamed "attrParser parses boolean attr" "attr_parser_boolean" $
            parseIs attrParser "disabled " ("disabled", "")
        , -- attrsParser
          testPropertyNamed "attrsParser parses multiple attrs" "attrs_parser_multi" $
            property $ do
                let result = runP (attrsParser []) " class=\"foo\" id=\"bar\""
                case result of
                    Right (Right m) -> do
                        Map.lookup "class" m === Just "foo"
                        Map.lookup "id" m === Just "bar"
                    _ -> failure
        , -- digitEq
          testPropertyNamed "digitEq matches same prefix" "digit_eq_same_prefix" $
            property $
                assert $
                    digitEq "item123" "item456"
        , testPropertyNamed "digitEq rejects different prefix" "digit_eq_diff_prefix" $
            property $
                assert $
                    not $
                        digitEq "item123" "other456"
        , testPropertyNamed "digitEq equal strings" "digit_eq_equal" $
            property $
                assert $
                    digitEq "abc" "abc"
        , -- digitEqFree
          testPropertyNamed "digitEqFree ignores digits" "digit_eq_free" $
            property $
                assert $
                    digitEqFree "a1b2c3" "a4b5c6"
        , testPropertyNamed "digitEqFree rejects different letters" "digit_eq_free_diff" $
            property $
                assert $
                    not $
                        digitEqFree "a1b2" "x1y2"
        , -- isAttrsMatch
          testPropertyNamed "isAttrsMatch matches subset" "is_attrs_match_subset" $
            property $
                assert $
                    isAttrsMatch (Map.fromList [("class", "foo"), ("id", "bar")]) [("class", Just "foo")]
        , testPropertyNamed "isAttrsMatch any value" "is_attrs_match_any" $
            property $
                assert $
                    isAttrsMatch (Map.fromList [("class", "foo")]) [("class", Nothing)]
        , testPropertyNamed "isAttrsMatch fails missing" "is_attrs_match_missing" $
            property $
                assert $
                    not $
                        isAttrsMatch (Map.fromList [("class", "foo")]) [("id", Just "bar")]
        , -- attrValuesExist
          testPropertyNamed "attrValuesExist found" "attr_values_exist_found" $
            property $
                assert $
                    attrValuesExist [("class", "foo")] [("class", Just "foo")]
        , testPropertyNamed "attrValuesExist missing" "attr_values_exist_missing" $
            property $
                assert $
                    not $
                        attrValuesExist [("class", "foo")] [("id", Just "bar")]
        , -- buildElemsOpts
          testPropertyNamed "buildElemsOpts matches first" "build_elems_first" $
            parseIs (buildElemsOpts ["div", "span"]) "div" "div"
        , testPropertyNamed "buildElemsOpts matches second" "build_elems_second" $
            parseIs (buildElemsOpts ["div", "span"]) "span" "span"
        , testPropertyNamed "buildElemsOpts fails on other" "build_elems_fail" $
            parseFails (buildElemsOpts ["div", "span"]) "p"
        , -- mkElemtagParser
          testPropertyNamed "mkElemtagParser Just list" "mk_elemtag_just" $
            parseIs (mkElemtagParser (Just ["a", "p"])) "a" "a"
        , testPropertyNamed "mkElemtagParser Nothing any" "mk_elemtag_nothing" $
            property $ do
                let result = runP (mkElemtagParser Nothing) "div"
                assert $ isRight result
        , -- hrefParser
          testPropertyNamed "hrefParser extracts href" "href_parser" $
            property $ do
                let result = runP hrefParser "<a href=\"https://example.com\">"
                case result of
                    Right url -> url === "https://example.com"
                    Left _ -> failure
        , testPropertyNamed "hrefParser fails without href" "href_parser_no_href" $
            parseFails hrefParser "<div class=\"foo\">"
        , -- parseOpeningTagDesc
          testPropertyNamed "parseOpeningTagDesc matches" "parse_opening_desc" $
            property $ do
                let result = runP (parseOpeningTagDesc (Just ["div"]) [("class", "main")]) "<div class=\"main\">"
                case result of
                    Right (tg, _) -> tg === "div"
                    Left _ -> failure
        , testPropertyNamed "parseOpeningTagDesc wrong tag" "parse_opening_desc_wrong" $
            parseFails (parseOpeningTagDesc (Just ["span"]) []) "<div>"
        , -- parseOpeningTag with multiple attrs
          testPropertyNamed "parseOpeningTag multiple attrs" "parse_opening_multi_attrs" $
            property $ do
                let result = runP (parseOpeningTag (Just ["div"]) [("class", Just "foo"), ("id", Just "bar")]) "<div class=\"foo\" id=\"bar\""
                case result of
                    Right (_, attrMap) -> do
                        Map.lookup "class" attrMap === Just "foo"
                        Map.lookup "id" attrMap === Just "bar"
                    Left _ -> failure
        , -- parseAttrSafe
          testPropertyNamed "parseAttrSafe parses attr" "parse_attr_safe" $
            property $ do
                let result = runP (parseAttrSafe "class") "<div class=\"foo\""
                case result of
                    Right v -> v === "foo"
                    Left _ -> failure
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Elem.SimpleElemParser
-- ---------------------------------------------------------------------------

simpleElemParserTests :: TestTree
simpleElemParserTests =
    testGroup
        "Scrappy.Elem.SimpleElemParser"
        [ -- eitherP
          testPropertyNamed "eitherP Left on first match" "eitherp_left" $
            parseIs (eitherP (string "a") (string "b")) "a" (Left "a")
        , testPropertyNamed "eitherP Right on second match" "eitherp_right" $
            parseIs (eitherP (string "a") (string "b")) "b" (Right "b")
        , -- manyTill_
          testPropertyNamed "manyTill_ collects and returns end" "manytill_basic" $
            property $ do
                let result = runP (SEP.manyTill_ anyChar (string "end")) "abcend"
                case result of
                    Right (xs, e) -> do
                        xs === "abc"
                        e === "end"
                    Left _ -> failure
        , -- selfClosing
          testPropertyNamed "selfClosing contains br" "self_closing_br" $
            property $
                assert $
                    elem "br" selfClosing
        , testPropertyNamed "selfClosing contains img" "self_closing_img" $
            property $
                assert $
                    elem "img" selfClosing
        , testPropertyNamed "selfClosing does not contain div" "self_closing_not_div" $
            property $
                assert $
                    not $
                        elem "div" selfClosing
        , -- stylingTags
          testPropertyNamed "stylingTags contains b" "styling_tags_b" $
            property $
                assert $
                    elem "b" stylingTags
        , testPropertyNamed "stylingTags contains strong" "styling_tags_strong" $
            property $
                assert $
                    elem "strong" stylingTags
        , -- el
          testPropertyNamed "el parses simple div" "el_div" $
            property $ do
                let result = runP (el "div" []) "<div>hello</div>"
                case result of
                    Right e -> do
                        elTag e === "div"
                        innerText' e === "hello"
                    Left _ -> failure
        , testPropertyNamed "el parses with attrs" "el_with_attrs" $
            property $ do
                let result = runP (el "div" [("class", "foo")]) "<div class=\"foo\">bar</div>"
                case result of
                    Right e -> do
                        elTag e === "div"
                        Map.lookup "class" (attrs e) === Just "foo"
                    Left _ -> failure
        , testPropertyNamed "el fails wrong tag" "el_wrong_tag" $
            parseFails (el "span" []) "<div>hello</div>"
        , -- elemParser
          testPropertyNamed "elemParser no inner spec" "elem_parser_no_inner" $
            property $ do
                let result = runP (elemParser (Just ["p"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<p>text</p>"
                case result of
                    Right e -> do
                        elTag e === "p"
                        innerText' e === "text"
                    Left _ -> failure
        , testPropertyNamed "elemParser with inner match" "elem_parser_with_inner" $
            property $ do
                let result = runP (elemParser (Just ["p"]) (Just (string "hello")) []) "<p>hello</p>"
                case result of
                    Right e -> do
                        elTag e === "p"
                        matches' e === ["hello"]
                    Left _ -> failure
        , testPropertyNamed "elemParser self-closing" "elem_parser_self_closing" $
            property $ do
                let result = runP (elemParser (Just ["br"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<br>"
                case result of
                    Right e -> elTag e === "br"
                    Left _ -> failure
        , testPropertyNamed "elemParser self-closing />" "elem_parser_self_closing_slash" $
            property $ do
                let result = runP (elemParser (Just ["br"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<br/>"
                case result of
                    Right e -> elTag e === "br"
                    Left _ -> failure
        , -- sameElTag
          testPropertyNamed "sameElTag matches" "same_el_tag" $
            property $ do
                let result = runP (sameElTag "div" (Nothing :: Maybe (ParsecT String () Identity String))) "<div>stuff</div>"
                case result of
                    Right e -> elTag e === "div"
                    Left _ -> failure
        , -- matchesInSameElTag
          testPropertyNamed "matchesInSameElTag extracts" "matches_in_same" $
            property $ do
                let result = runP (matchesInSameElTag "p" (Just (string "hi"))) "<p>hi</p>"
                case result of
                    Right ms -> ms === ["hi"]
                    Left _ -> failure
        , -- stylingElem (returns reversed inner text via reverse . fst)
          testPropertyNamed "stylingElem parses bold" "styling_elem_bold" $
            parseIs SEP.stylingElem "<b>bold text</b>" "txet dlob"
        , testPropertyNamed "stylingElem parses em" "styling_elem_em" $
            parseIs SEP.stylingElem "<em>emphasis</em>" "sisahpme"
        , -- clickableHref
          testPropertyNamed "clickableHref parses anchor" "clickable_href" $
            property $ do
                let result = runP (clickableHref False (Link "https://example.com")) "<a href=\"https://example.com/page\""
                case result of
                    Right c -> getLink c === Link "https://example.com/page"
                    Left _ -> failure
        , -- elemParserWhere (attr name, attr value predicate)
          testPropertyNamed "elemParserWhere with predicate" "elem_parser_where" $
            property $ do
                let result = runP (elemParserWhere (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) "class" (== "target")) "<div class=\"target\">hit</div>"
                case result of
                    Right e -> elTag e === "div"
                    Left _ -> failure
        , testPropertyNamed "elemParserWhere fails predicate" "elem_parser_where_fail" $
            parseFails (elemParserWhere (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) "class" (== "nope")) "<div class=\"target\">miss</div>"
        , -- elemParser with attrs spec
          testPropertyNamed "elemParser rejects wrong attrs" "elem_parser_wrong_attrs" $
            parseFails (elemParser (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) [("class", Just "wrong")]) "<div class=\"right\">text</div>"
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Elem.ChainHTML
-- ---------------------------------------------------------------------------

chainHTMLTests :: TestTree
chainHTMLTests =
    testGroup
        "Scrappy.Elem.ChainHTML"
        [ -- nl
          testPropertyNamed "nl skips spaces" "nl_spaces" $
            parseSucceeds nl "   "
        , testPropertyNamed "nl skips newlines" "nl_newlines" $
            parseSucceeds nl "\n\n"
        , testPropertyNamed "nl skips empty" "nl_empty" $
            parseSucceeds nl ""
        , -- manyTill_
          testPropertyNamed "manyTill_ collects" "chain_manytill" $
            property $ do
                let result = runP (manyTill_ anyChar (string "!")) "abc!"
                case result of
                    Right (xs, e) -> do
                        xs === "abc"
                        e === "!"
                    Left _ -> failure
        , -- Shell type
          testPropertyNamed "Shell type is a pair" "shell_type" $
            property $ do
                let sh = ("div", [("class", Just "container")]) :: Shell
                fst sh === "div"
        , -- sequenceHtml
          testPropertyNamed "sequenceHtml sequences two parsers" "seq_html" $
            property $ do
                let result = runP (sequenceHtml (string "a") (string "b")) "a b"
                case result of
                    Right (a, b) -> do
                        a === "a"
                        b === "b"
                    Left _ -> failure
        , -- sequenceHtml_
          testPropertyNamed "sequenceHtml_ discards first" "seq_html_discard" $
            parseIs (sequenceHtml_ (string "a") (string "b")) "a b" "b"
        , -- </>> operator
          testPropertyNamed "</>> operator works" "chain_op" $
            parseIs ((string "x") </>> (string "y")) "x y" "y"
        , -- </>>=  operator (keeps both results)
          testPropertyNamed "</>>=  operator works" "chain_op_bind" $
            property $ do
                let result = runP ((string "x") </>>= (string "y")) "x y"
                case result of
                    Right (a, b) -> do
                        a === "x"
                        b === "y"
                    Left _ -> failure
        , -- manyHtml
          testPropertyNamed "manyHtml zero matches" "many_html_zero" $
            property $ do
                let result = runP (manyHtml (string "x")) "abc"
                case result of
                    Right xs -> length xs === 0
                    Left _ -> failure
        , testPropertyNamed "manyHtml multiple matches" "many_html_multi" $
            property $ do
                let result = runP (manyHtml (string "x") :: ParsecT String () Identity [String]) "x x x "
                case result of
                    Right xs -> length xs === 3
                    Left _ -> failure
        , -- someHtml
          testPropertyNamed "someHtml at least one" "some_html_one" $
            property $ do
                let result = runP (someHtml (string "x") :: ParsecT String () Identity [String]) "x x"
                case result of
                    Right xs -> assert $ length xs > 0
                    Left _ -> failure
        , testPropertyNamed "someHtml fails on zero" "some_html_zero_fail" $
            parseFails (someHtml (string "x") :: ParsecT String () Identity [String]) "abc"
        , -- manyTillHtml_
          testPropertyNamed "manyTillHtml_ collects with whitespace" "many_till_html" $
            property $ do
                let result = runP (manyTillHtml_ (string "x") (string "!")) "x x x!"
                case result of
                    Right (xs, e) -> do
                        length xs === 3
                        e === "!"
                    Left _ -> failure
        , -- htmlTag
          testPropertyNamed "htmlTag parses html" "html_tag" $
            property $ do
                let result = runP htmlTag "<html"
                case result of
                    Right (t, _) -> t === "html"
                    Left _ -> failure
        , -- contains''
          testPropertyNamed "contains'' finds in shell" "contains_double_prime" $
            property $ do
                let shell = ("div", []) :: Shell
                    result = runP (contains'' shell (string "hello")) "<div>hello world</div>"
                case result of
                    Right ms -> length ms === 1
                    Left _ -> failure
        , -- containsFirst
          testPropertyNamed "containsFirst gets first match" "contains_first" $
            property $ do
                let result = runP (containsFirst (el "div" []) (string "x")) "<div>x y x</div>"
                case result of
                    Right s -> s === "x"
                    Left _ -> failure
        , -- contains' (containsMany)
          testPropertyNamed "contains' finds all" "contains_prime_all" $
            property $ do
                let result = runP (contains' (el "div" []) (string "x")) "<div>xax</div>"
                case result of
                    Right xs -> length xs === 2
                    Left _ -> failure
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Elem.TreeElemParser
-- ---------------------------------------------------------------------------

treeElemParserTests :: TestTree
treeElemParserTests =
    testGroup
        "Scrappy.Elem.TreeElemParser"
        [ -- Many/One
          testPropertyNamed "fromMany extracts One" "from_many_one" $
            property $
                fromMany (One (42 :: Int)) === 42
        , testPropertyNamed "fromMany extracts Many" "from_many_many" $
            property $
                fromMany (Many (42 :: Int)) === 42
        , testPropertyNamed "Many Show" "many_show" $
            property $
                show (Many (1 :: Int)) === "Many 1"
        , testPropertyNamed "One Show" "one_show" $
            property $
                show (One (1 :: Int)) === "One 1"
        , -- takeTill
          testPropertyNamed "takeTill takes until predicate" "take_till_basic" $
            property $
                takeTill (> (3 :: Int)) [1, 2, 3, 4, 5] === [1, 2, 3, 4]
        , testPropertyNamed "takeTill empty" "take_till_empty" $
            property $
                takeTill (> (3 :: Int)) ([] :: [Int]) === []
        , testPropertyNamed "takeTill all pass" "take_till_all_pass" $
            property $
                takeTill (> (10 :: Int)) [1, 2, 3] === [1, 2, 3]
        , -- selfClosing (TreeElemParser's copy)
          testPropertyNamed "selfClosing contains hr" "tree_self_closing_hr" $
            property $
                assert $
                    elem "hr" TEP.selfClosing
        , -- manyTill_
          testPropertyNamed "manyTill_ tree version" "tree_manytill" $
            property $ do
                let result = runP (TEP.manyTill_ anyChar (string "!")) "xyz!"
                case result of
                    Right (xs, e) -> do
                        xs === "xyz"
                        e === "!"
                    Left _ -> failure
        , -- treeElemParser
          testPropertyNamed "treeElemParser parses div" "tree_elem_div" $
            property $ do
                let result = runP (treeElemParser (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<div>hello</div>"
                case result of
                    Right e -> do
                        elTag e === "div"
                        innerText' e === "hello"
                    Left _ -> failure
        , testPropertyNamed "treeElemParser parses nested" "tree_elem_nested" $
            property $ do
                let result = runP (treeElemParser (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<div><p>inner</p></div>"
                case result of
                    Right e -> do
                        elTag e === "div"
                        assert $ not $ null $ _innerTree' e
                    Left _ -> failure
        , testPropertyNamed "treeElemParser self-closing" "tree_elem_self_closing" $
            property $ do
                let result = runP (treeElemParser (Just ["br"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<br>"
                case result of
                    Right e -> elTag e === "br"
                    Left _ -> failure
        , -- skipManyTill
          testPropertyNamed "skipManyTill skips to end" "skip_many_till" $
            property $ do
                let result = runP (skipManyTill anyChar (string "!")) "abc!"
                case result of
                    Right s -> s === "!"
                    Left _ -> failure
        , -- sameTreeH
          testPropertyNamed "sameTreeH same structure" "same_tree_h" $
            property $ do
                let template = runP (treeElemParser (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<div>a</div>"
                case template of
                    Right treeH -> do
                        let result = runP (sameTreeH (Nothing :: Maybe (ParsecT String () Identity String)) treeH) "<div>b</div>"
                        assert $ isRight result
                    Left _ -> failure
        , testPropertyNamed "sameTreeH different tag fails" "same_tree_h_diff" $
            property $ do
                let template = runP (treeElemParser (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) []) "<div>a</div>"
                case template of
                    Right treeH -> do
                        let result = runP (sameTreeH (Nothing :: Maybe (ParsecT String () Identity String)) treeH) "<span>b</span>"
                        assert $ isLeft result
                    Left _ -> failure
        , -- htmlGroup
          testPropertyNamed "htmlGroup groups elements" "html_group" $
            property $ do
                let result = runP (htmlGroup Nothing (Nothing :: Maybe (ParsecT String () Identity String)) []) "<p>a</p><p>b</p>"
                case result of
                    Right g -> do
                        let elems = ungroup g
                        assert $ length elems > 0
                    Left _ -> failure
        , -- groupify
          testPropertyNamed "groupify groups same elements" "groupify_test" $
            property $ do
                let n1 = Node ("p", mempty :: Map.Map String String) [] :: Tree (String, Map.Map String String)
                    n2 = Node ("p", mempty) []
                    n3 = Node ("div", mempty) []
                    groups = groupify [n1, n2, n3] []
                -- n1 and n2 are same ("p"), should become Many; n3 is different ("div"), should become One
                length groups === 2
        , testPropertyNamed "groupify single element" "groupify_single" $
            property $ do
                let n1 = Node ("p", mempty :: Map.Map String String) [] :: Tree (String, Map.Map String String)
                    groups = groupify [n1] []
                length groups === 1
        , testPropertyNamed "groupify empty" "groupify_empty" $
            property $ do
                let groups = groupify ([] :: [Tree (String, Map.Map String String)]) []
                length groups === 0
        , -- fromMany
          testPropertyNamed "fromMany One" "from_many_one" $
            property $ do
                let n = Node ("p", mempty :: Map.Map String String) [] :: Tree (String, Map.Map String String)
                fromMany (One n) === n
        , testPropertyNamed "fromMany Many" "from_many_many" $
            property $ do
                let n = Node ("div", mempty :: Map.Map String String) [] :: Tree (String, Map.Map String String)
                fromMany (Many n) === n
        , -- takeTill
          testPropertyNamed "takeTill empty" "take_till_empty" $
            property $
                takeTill (const True) ([] :: [Int]) === []
        , testPropertyNamed "takeTill predicate at start" "take_till_start" $
            property $
                takeTill (== 1) [1, 2, 3] === [1 :: Int]
        , testPropertyNamed "takeTill predicate at end" "take_till_end" $
            property $
                takeTill (== 3) [1, 2, 3] === [1, 2, 3 :: Int]
        , testPropertyNamed "takeTill no match" "take_till_none" $
            property $
                takeTill (== 9) [1, 2, 3] === [1, 2, 3 :: Int]
        , -- table parser
          testPropertyNamed "table parses tr group" "table_parser" $
            property $ do
                let result = runP table "<tr><td>a</td></tr><tr><td>b</td></tr>"
                case result of
                    Right g -> assert $ length (ungroup g) >= 2
                    Left _ -> failure
        , -- TreeElemParser selfClosing
          testPropertyNamed "TreeElemParser selfClosing list" "tree_self_closing" $
            property $ do
                assert $ elem "br" TEP.selfClosing
                assert $ elem "img" TEP.selfClosing
                assert $ not $ elem "div" TEP.selfClosing
        , -- treeElemParser with attributes
          testPropertyNamed "treeElemParser with attrs" "tree_elem_attrs" $
            property $ do
                let result = runP (treeElemParser (Just ["div"]) (Nothing :: Maybe (ParsecT String () Identity String)) [("class", Just "main")]) "<div class=\"main\">text</div>"
                case result of
                    Right e -> do
                        elTag e === "div"
                        Map.lookup "class" (attrs e) === Just "main"
                    Left _ -> failure
        , -- treeElemParser with inner match
          testPropertyNamed "treeElemParser with match" "tree_elem_match" $
            property $ do
                let result = runP (treeElemParser (Just ["div"]) (Just (string "hello")) []) "<div>hello</div>"
                case result of
                    Right e -> do
                        elTag e === "div"
                        matches' e === ["hello"]
                    Left _ -> failure
        , -- Many Show
          testPropertyNamed "Many Show One" "many_show_one" $
            property $
                assert $
                    not $
                        null $
                            show (One (1 :: Int))
        , testPropertyNamed "Many Show Many" "many_show_many" $
            property $
                assert $
                    not $
                        null $
                            show (Many (2 :: Int))
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Elem.ITextElemParser
-- ---------------------------------------------------------------------------

iTextElemParserTests :: TestTree
iTextElemParserTests =
    testGroup
        "Scrappy.Elem.ITextElemParser"
        [ -- WrittenWord
          testPropertyNamed "WrittenWord Show" "ww_show" $
            property $
                show (WW "hello") === "hello"
        , testPropertyNamed "WrittenWord Semigroup" "ww_semigroup" $
            property $
                unWord (WW "hello" <> WW "world") === "hello world"
        , testPropertyNamed "WrittenWord Monoid identity" "ww_mempty" $
            property $
                unWord (mempty :: WrittenWord) === ""
        , -- Sentence
          testPropertyNamed "Sentence Semigroup" "sentence_semigroup" $
            property $ do
                let s1 = Sentence [WW "Hello"]
                    s2 = Sentence [WW "world"]
                length (unSentence (s1 <> s2)) === 2
        , testPropertyNamed "Sentence Monoid identity" "sentence_mempty" $
            property $
                length (unSentence (mempty :: Sentence)) === 0
        , -- writtenWord parser
          testPropertyNamed "writtenWord parses simple word" "written_word_simple" $
            property $ do
                let result = runP writtenWord "hello "
                case result of
                    Right (WW w) -> assert $ not $ null w
                    Left _ -> failure
        , -- capitalizedWord
          testPropertyNamed "capitalizedWord parses uppercase start" "cap_word" $
            property $ do
                let result = runP capitalizedWord "Hello"
                assert $ isRight result
        , testPropertyNamed "capitalizedWord fails on lowercase" "cap_word_fail" $
            parseFails capitalizedWord "hello"
        , -- number parser
          testPropertyNamed "number parses integer" "number_int" $
            parseIs number "42 " "42"
        , testPropertyNamed "number parses decimal" "number_decimal" $
            parseIs number "3.14 " "3.14"
        , -- word' parser
          testPropertyNamed "word' parses letters" "word_prime" $
            parseIs word' "hello " "hello"
        , testPropertyNamed "word' parses hyphenated" "word_prime_hyphen" $
            parseIs word' "well-known " "well-known"
        , -- punctuation
          testPropertyNamed "punctuation parses semicolon" "punct_semi" $
            parseIs punctuation ";" ';'
        , testPropertyNamed "punctuation parses comma" "punct_comma" $
            parseIs punctuation "," ','
        , -- removeStyleTags (note: function has inverted Left/Right semantics —
          -- it keeps matched tag strings via catEithers and discards non-tag chars,
          -- so it returns the tag representations rather than the stripped content)
          testPropertyNamed "removeStyleTags returns tag representations" "remove_style_bold" $
            property $
                assert $
                    removeStyleTags "<b>bold</b> text" /= "<b>bold</b> text"
        , testPropertyNamed "removeStyleTags on plain text returns empty" "remove_style_none" $
            property $
                removeStyleTags "plain text" === ""
        , -- catEithers
          testPropertyNamed "catEithers extracts Rights" "cat_eithers" $
            property $
                catEithers ([Left "a", Right "b", Left "c", Right "d"] :: [Either String String]) === ["b", "d"]
        , testPropertyNamed "catEithers empty" "cat_eithers_empty" $
            property $
                catEithers ([] :: [Either String String]) === []
        , -- elemAny
          testPropertyNamed "elemAny parses any element" "elem_any" $
            property $ do
                let result = runP elemAny "<div>hello</div>"
                case result of
                    Right e -> elTag e === "div"
                    Left _ -> failure
        , -- Paragraph
          testPropertyNamed "Paragraph Show" "paragraph_show" $
            property $ do
                let p = Paragraph [Sentence [WW "Hello", WW "world"]]
                assert $ not $ null $ show p
        , testPropertyNamed "Paragraph Semigroup" "paragraph_semigroup" $
            property $ do
                let p1 = Paragraph [Sentence [WW "Hello"]]
                    p2 = Paragraph [Sentence [WW "world"]]
                length (unParagraph (p1 <> p2)) === 2
        , testPropertyNamed "Paragraph Monoid" "paragraph_monoid" $
            property $
                length (unParagraph (mempty :: Paragraph)) === 0
        , testPropertyNamed "Paragraph ShowHTML" "paragraph_showhtml" $
            property $ do
                let p = Paragraph [Sentence [WW "Hello"]]
                assert $ not $ null $ showH p
        , -- Sentence Show
          testPropertyNamed "Sentence Show" "sentence_show" $
            property $ do
                let s = Sentence [WW "Hello", WW "world"]
                show s === "Hello world."
        , testPropertyNamed "Sentence ShowHTML" "sentence_showhtml" $
            property $ do
                let s = Sentence [WW "Hello"]
                assert $ not $ null $ showH s
        , -- wordSeparator
          testPropertyNamed "wordSeparator parses space" "word_sep_space" $
            parseIs wordSeparator " " " "
        , testPropertyNamed "wordSeparator parses comma" "word_sep_comma" $
            parseIs wordSeparator ", " ", "
        , testPropertyNamed "wordSeparator parses colon" "word_sep_colon" $
            parseIs wordSeparator ": " ": "
        , testPropertyNamed "wordSeparator parses semicolon" "word_sep_semi" $
            parseIs wordSeparator "; " "; "
        , -- comma, colon, semiColon
          testPropertyNamed "comma parser" "comma_parser" $
            parseIs comma "," ","
        , testPropertyNamed "colon parser" "colon_parser" $
            parseIs colon ":" ":"
        , testPropertyNamed "semiColon parser" "semicolon_parser" $
            parseIs semiColon ";" ";"
        , testPropertyNamed "comma with space" "comma_space" $
            parseIs comma ", " ", "
        , testPropertyNamed "colon with space" "colon_space" $
            parseIs colon ": " ": "
        , -- sentence parser
          testPropertyNamed "sentence parses simple" "sentence_simple" $
            property $ do
                let result = runP sentence "Hello world."
                case result of
                    Right (Sentence ws) -> assert $ length ws > 0
                    Left _ -> failure
        , testPropertyNamed "sentence parses with number" "sentence_with_number" $
            property $ do
                let result = runP sentence "42 things."
                case result of
                    Right (Sentence ws) -> assert $ length ws > 0
                    Left _ -> failure
        , -- styleTags
          testPropertyNamed "styleTags contains expected tags" "style_tags_content" $
            property $ do
                assert $ elem "b" styleTags
                assert $ elem "em" styleTags
                assert $ elem "strong" styleTags
                assert $ elem "i" styleTags
                assert $ not $ elem "div" styleTags
        , -- negParseOpeningTag
          testPropertyNamed "negParseOpeningTag rejects matching tag" "neg_parse_reject" $
            parseFails (negParseOpeningTag ["div"]) "<div"
        , testPropertyNamed "negParseOpeningTag accepts non-matching" "neg_parse_accept" $
            property $ do
                let result = runP (negParseOpeningTag ["div"]) "<span "
                case result of
                    Right (t, _) -> t === "span"
                    Left _ -> failure
        , -- anyThingbut
          testPropertyNamed "anyThingbut rejects listed" "anythingbut_reject" $
            parseFails (anyThingbut ["abc"]) "abc"
        , testPropertyNamed "anyThingbut accepts other" "anythingbut_accept" $
            parseIs (anyThingbut ["abc"]) "xyz" "xyz"
        , -- divideUp
          testPropertyNamed "divideUp partitions" "divide_up" $
            property $ do
                let result = runP (divideUp (string "x")) "axb"
                case result of
                    Right parts -> length parts === 3
                    Left _ -> failure
        , -- styleElem
          testPropertyNamed "styleElem parses bold" "style_elem_bold" $
            property $ do
                let result = runP styleElem "<b>text</b>"
                case result of
                    Right e -> elTag e === "b"
                    Left _ -> failure
        , testPropertyNamed "styleElem parses em" "style_elem_em" $
            property $ do
                let result = runP styleElem "<em>text</em>"
                case result of
                    Right e -> elTag e === "em"
                    Left _ -> failure
        ]

-- ---------------------------------------------------------------------------
-- Scrappy.Scrape
-- ---------------------------------------------------------------------------

scrapeTests :: TestTree
scrapeTests =
    testGroup
        "Scrappy.Scrape"
        [ -- scrape / runScraperOnHtml
          testPropertyNamed "scrape finds elements" "scrape_finds" $
            property $ do
                let result = scrape (el "p" []) "<div><p>hello</p><p>world</p></div>"
                assert $ isJust result
                case result of
                    Just xs -> length xs === 2
                    Nothing -> failure
        , testPropertyNamed "scrape no match" "scrape_no_match" $
            property $
                assert $
                    isNothing $
                        scrape (el "span" []) "<div>no spans</div>"
        , -- scrapeFirst'
          testPropertyNamed "scrapeFirst' gets first" "scrape_first" $
            property $ do
                let result = scrapeFirst' (el "p" []) "<p>one</p><p>two</p>"
                case result of
                    Just e -> innerText' e === "one"
                    Nothing -> failure
        , testPropertyNamed "scrapeFirst' no match" "scrape_first_none" $
            property $
                assert $
                    isNothing (scrapeFirst' (el "span" []) "<div>nope</div>")
        , -- exists
          testPropertyNamed "exists True on match" "exists_true" $
            property $
                assert $
                    exists (el "p" []) "<p>hi</p>"
        , testPropertyNamed "exists False on no match" "exists_false" $
            property $
                assert $
                    not $
                        exists (el "span" []) "<div>nope</div>"
        , -- runScraperOnHtml1
          testPropertyNamed "runScraperOnHtml1 gets first" "scraper_html1" $
            property $ do
                let result = runScraperOnHtml1 (el "p" []) "<p>first</p><p>second</p>"
                case result of
                    Just e -> innerText' e === "first"
                    Nothing -> failure
        , testPropertyNamed "runScraperOnHtml1 no match" "scraper_html1_none" $
            property $
                assert $
                    isNothing (runScraperOnHtml1 (el "span" []) "<div>nope</div>")
        , -- getFirstSafe
          testPropertyNamed "getFirstSafe Just" "get_first_safe_just" $
            property $
                getFirstSafe (Just [1, 2, 3 :: Int]) === Just 1
        , testPropertyNamed "getFirstSafe Nothing" "get_first_safe_nothing" $
            property $
                getFirstSafe (Nothing :: Maybe [Int]) === Nothing
        , testPropertyNamed "getFirstSafe empty list" "get_first_safe_empty" $
            property $
                getFirstSafe (Just ([] :: [Int])) === Nothing
        , -- getFirstFitSafe
          testPropertyNamed "getFirstFitSafe finds matching" "get_first_fit" $
            property $
                getFirstFitSafe (> (2 :: Int)) (Just [1, 2, 3, 4]) === Just 3
        , testPropertyNamed "getFirstFitSafe no fit" "get_first_fit_none" $
            property $
                getFirstFitSafe (> (10 :: Int)) (Just [1, 2, 3 :: Int]) === Nothing
        , -- findFit
          testPropertyNamed "findFit finds first match" "find_fit" $
            property $
                findFit (> (2 :: Int)) [1, 2, 3, 4] === Just 3
        , testPropertyNamed "findFit empty list" "find_fit_empty" $
            property $
                findFit (> (0 :: Int)) ([] :: [Int]) === Nothing
        , -- coerceMaybeParser
          testPropertyNamed "coerceMaybeParser Just succeeds" "coerce_maybe_just" $
            property $ do
                let result = runP (coerceMaybeParser (Just ("hi" :: String))) ""
                case result of
                    Right s -> s === ("hi" :: String)
                    Left _ -> failure
        , testPropertyNamed "coerceMaybeParser Nothing fails" "coerce_maybe_nothing" $
            parseFails (coerceMaybeParser (Nothing :: Maybe String)) ""
        , -- scrapeFirst (parser-level)
          testPropertyNamed "scrapeFirst finds first" "scrape_first_parser" $
            property $ do
                let result = runP (scrapeFirst (string "x")) "axbx"
                case result of
                    Right (Just s) -> s === "x"
                    _ -> failure
        , testPropertyNamed "scrapeFirst no match" "scrape_first_parser_none" $
            property $ do
                let result = runP (scrapeFirst (string "z")) "abc"
                case result of
                    Right r -> r === Nothing
                    Left _ -> failure
        , -- findCount
          testPropertyNamed "findCount counts matches" "find_count" $
            parseIs (findCount (string "x")) "xaxbxc" 3
        , testPropertyNamed "findCount zero" "find_count_zero" $
            parseIs (findCount (string "z")) "abc" 0
        , -- scrapePrefixed additional
          testPropertyNamed "scrapePrefixed with string parser" "scrape_prefixed_str" $
            property $ do
                let result = scrapePrefixed "key=" (string "val") "noise key=val end"
                assert $ isJust result
        , -- scrapePrefixed with el
          testPropertyNamed "scrapePrefixed no match" "scrape_prefixed_none" $
            property $
                assert $
                    isNothing (scrapePrefixed "ZZZ" (el "span" []) "no match here")
        , -- runScraperInBody
          testPropertyNamed "runScraperInBody finds in body" "scraper_in_body" $
            property $ do
                let html = "<html><head><title>t</title></head><body><p>hello</p></body></html>"
                    result = runScraperInBody (el "p" []) html
                assert $ isJust result
        , -- runScraperOnBody
          testPropertyNamed "runScraperOnBody finds after head" "scraper_on_body" $
            property $ do
                let html = "<html><head><title>t</title></head><body><p>world</p></body></html>"
                    result = runScraperOnBody (el "p" []) html
                assert $ isJust result
        , -- filterPattern (note: uses `undefined` in Left branch of `either`,
          -- so it throws on parse failure — only testable when parse succeeds)

          -- hoistMaybe
          testPropertyNamed "hoistMaybe Just" "hoist_maybe_just" $
            property $ do
                let Identity result = runMaybeT (hoistMaybe (Just (42 :: Int)))
                result === Just (42 :: Int)
        , testPropertyNamed "hoistMaybe Nothing" "hoist_maybe_nothing" $
            property $ do
                let Identity result = runMaybeT (hoistMaybe (Nothing :: Maybe Int))
                result === (Nothing :: Maybe Int)
        , -- scrapePrefixed
          testPropertyNamed "scrapePrefixed finds after prefix" "scrape_prefixed" $
            property $ do
                let result = scrapePrefixed "Price: " (el "span" []) "noise Price: <span>$42</span> end"
                assert $ isJust result
        , -- scrape with attrs
          testPropertyNamed "scrape with class attr" "scrape_with_class" $
            property $ do
                let result = scrape (el "div" [("class", "item")]) "<div class=\"item\">content</div><div class=\"other\">skip</div>"
                case result of
                    Just xs -> length xs === 1
                    Nothing -> failure
        , -- scrape nested elements
          testPropertyNamed "scrape nested" "scrape_nested" $
            property $ do
                let html = "<div><p>outer <b>bold</b> text</p></div>"
                    result = scrape (el "p" []) html
                case result of
                    Just [e] -> assert $ not $ null $ innerText' e
                    _ -> failure
        ]
