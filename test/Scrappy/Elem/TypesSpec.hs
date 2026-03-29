{-# LANGUAGE ScopedTypeVariables #-}

module Scrappy.Elem.TypesSpec (spec) where

import Data.Functor.Identity (Identity)
import Data.Graph (Tree (Node))
import qualified Data.Map as Map
import qualified Data.Text as T
import Test.Hspec
import Text.Parsec (ParsecT, parse)

import Scrappy.Elem.Types
import Scrappy.Links (Link (..))
import TestUtils (shouldBeUndefined)

-- Helper: make a simple Elem'
mkElem :: String -> [(String, String)] -> String -> Elem' String
mkElem tg attrList inner =
    Elem' tg (Map.fromList attrList) [] inner

-- Helper: make a simple TreeHTML
mkTree :: String -> [(String, String)] -> [HTMLMatcher TreeHTML String] -> TreeHTML String
mkTree tg attrList rawInner =
    TreeHTML tg (Map.fromList attrList) [] rawInner

spec :: Spec
spec = do
    describe "ShowHTML instances" $ do
        it "showH Char" $
            showH 'a' `shouldBe` "'a'"

        it "showH [Char] (String)" $
            showH ("hello" :: String) `shouldBe` "\"hello\""

        it "showH Text" $
            showH (T.pack "hello") `shouldBe` "hello"

    describe "ScrapeFail-adjacent: AttrsError" $ do
        it "shows IncorrectAttrs" $
            show IncorrectAttrs `shouldBe` "IncorrectAttrs"

    describe "ElementRep Elem'" $ do
        it "elTag returns tag" $
            elTag (mkElem "div" [] "content") `shouldBe` "div"

        it "attrs returns attributes" $
            attrs (mkElem "a" [("href", "/link")] "") `shouldBe` Map.fromList [("href", "/link")]

        it "innerText' returns inner HTML" $
            innerText' (mkElem "p" [] "hello world") `shouldBe` "hello world"

        it "matches' returns empty matches" $
            matches' (mkElem "div" [] "x") `shouldBe` ([] :: [String])

    describe "ElementRep TreeHTML" $ do
        it "elTag returns tag" $
            elTag (mkTree "span" [] []) `shouldBe` "span"

        it "attrs returns attributes" $
            attrs (mkTree "div" [("class", "test")] []) `shouldBe` Map.fromList [("class", "test")]

        it "_innerText' on empty tree" $
            _innerText' (mkTree "div" [] []) `shouldBe` ""

        it "_innerText' with IText" $
            _innerText' (mkTree "div" [] [IText "hello"]) `shouldBe` "hello"

        it "_innerText' with nested Element" $ do
            let inner = mkTree "span" [] [IText "nested"]
            _innerText' (mkTree "div" [] [IText "before ", Element inner, IText " after"])
                `shouldContain` "before"

    describe "deriveForest" $ do
        it "empty matchers yield empty forest" $
            deriveForest ([] :: [HTMLMatcher TreeHTML String]) `shouldBe` []

        it "IText matchers are skipped" $
            deriveForest [IText "hello" :: HTMLMatcher TreeHTML String] `shouldBe` []

        it "Element matchers produce forest nodes" $ do
            let inner = mkTree "span" [("id", "x")] []
            let forest = deriveForest [Element inner]
            length forest `shouldBe` 1
            case forest of
                [Node (tg, _ats) _] -> tg `shouldBe` "span"
                _ -> expectationFailure "unexpected forest structure"

    describe "_innerTree'" $ do
        it "returns empty for tree with no child elements" $
            _innerTree' (mkTree "div" [] [IText "just text"]) `shouldBe` []

        it "returns forest for tree with child elements" $ do
            let child = mkTree "p" [] [IText "paragraph"]
            let tree = mkTree "div" [] [Element child]
            length (_innerTree' tree) `shouldBe` 1

    describe "noPat" $ do
        it "returns Nothing" $
            case (noPat :: Maybe (ParsecT String () Identity String)) of
                Nothing -> pure ()
                Just _ -> expectationFailure "expected Nothing"

    describe "coerceAttrs" $ do
        it "converts non-empty values to Just" $
            coerceAttrs (Map.fromList [("href", "/page")]) `shouldBe` [("href", Just "/page")]

        it "converts empty values to Nothing" $
            coerceAttrs (Map.fromList [("disabled", "")]) `shouldBe` [("disabled", Nothing)]

        it "handles mixed values" $ do
            let result = coerceAttrs (Map.fromList [("class", "foo"), ("hidden", "")])
            result `shouldContain` [("class", Just "foo")]
            result `shouldContain` [("hidden", Nothing)]

    describe "f (String -> Maybe String)" $ do
        it "returns Nothing for empty string" $
            f "" `shouldBe` Nothing

        it "returns Just for non-empty string" $
            f "hello" `shouldBe` Just "hello"

    describe "GroupHtml" $ do
        it "mkGH empty list" $ do
            let gh = mkGH ([] :: [Elem' String])
            length (ungroup gh) `shouldBe` 0

        it "mkGH non-empty list" $ do
            let e = mkElem "div" [] "content"
            let gh = mkGH [e, e]
            length (ungroup gh) `shouldBe` 2

        it "ungroup extracts elements" $ do
            let e = mkElem "p" [] "text"
            let gh = GroupHtml [e] 1 4 :: GroupHtml Elem' String
            length (ungroup gh) `shouldBe` 1

        it "Eq compares by product of count and maxLength" $ do
            let g1 = GroupHtml [] 3 4 :: GroupHtml Elem' String
            let g2 = GroupHtml [] 2 6 :: GroupHtml Elem' String
            (g1 == g2) `shouldBe` True

        it "Ord compares by product" $ do
            let g1 = GroupHtml [] 2 3 :: GroupHtml Elem' String
            let g2 = GroupHtml [] 3 3 :: GroupHtml Elem' String
            (g1 <= g2) `shouldBe` True

        it "Show empty GroupHtml" $ do
            let gh = GroupHtml [] 0 0 :: GroupHtml Elem' String
            show gh `shouldContain` "count ="

        it "Show non-empty GroupHtml" $ do
            let e = mkElem "div" [] "x"
            let gh = GroupHtml [e] 1 1 :: GroupHtml Elem' String
            show gh `shouldContain` "elemStructure="

    describe "longestElem" $ do
        it "returns Nothing for empty list" $
            case longestElem ([] :: [Elem' String]) of
                Nothing -> pure ()
                Just _ -> expectationFailure "expected Nothing"

        it "returns Just for single element" $ do
            let e = mkElem "div" [] "hi"
            case longestElem [e] of
                Just el -> innerText' el `shouldBe` "hi"
                Nothing -> expectationFailure "expected Just"

        it "returns longest for multiple elements" $ do
            let short = mkElem "div" [] "hi"
            let long = mkElem "div" [] "hello world"
            case longestElem [short, long] of
                Just e -> innerText' e `shouldBe` "hello world"
                Nothing -> expectationFailure "expected Just"

    describe "maxLength" $ do
        it "returns empty for empty list" $
            maxLength ([] :: [[Int]]) `shouldBe` []

        it "returns single list" $
            maxLength ([[1, 2, 3]] :: [[Int]]) `shouldBe` [1, 2, 3]

        it "returns longest list" $
            maxLength ([[1], [1, 2, 3], [1, 2]] :: [[Int]]) `shouldBe` [1, 2, 3]

    describe "biggestHtmlGroup" $ do
        it "returns empty group for empty input" $ do
            let result = biggestHtmlGroup ([] :: [GroupHtml Elem' String])
            length (ungroup result) `shouldBe` 0

        it "returns biggest by product" $ do
            let g1 = GroupHtml [] 2 5 :: GroupHtml Elem' String
            let g2 = GroupHtml [] 3 4 :: GroupHtml Elem' String
            let result = biggestHtmlGroup [g1, g2]
            -- g2 wins: 3*4=12 > 2*5=10
            result `shouldBe` g2

    describe "biggestGroup" $ do
        it "returns empty for empty list" $ do
            let result = biggestGroup ([] :: [GroupHtml Elem' String])
            length (ungroup result) `shouldBe` 0

        it "returns single group" $ do
            let g = GroupHtml [] 5 5 :: GroupHtml Elem' String
            biggestGroup [g] `shouldBe` g

        it "returns biggest of multiple" $ do
            let g1 = GroupHtml [] 10 1 :: GroupHtml Elem' String
            let g2 = GroupHtml [] 2 10 :: GroupHtml Elem' String
            let result = biggestGroup [g1, g2]
            result `shouldBe` g2

    describe "getHrefAttrs and getHrefEl" $ do
        it "extracts href when present" $ do
            let ats = Map.fromList [("href", "https://example.com")]
            let cUrl = Link "https://example.com"
            getHrefAttrs False cUrl ats `shouldBe` Just (Link "https://example.com")

        it "returns Nothing when no href" $ do
            let ats = Map.fromList [("class", "btn")]
            let cUrl = Link "https://example.com"
            getHrefAttrs False cUrl ats `shouldBe` Nothing

        it "getHrefEl delegates to getHrefAttrs" $ do
            let e = mkElem "a" [("href", "https://example.com")] "link"
            let cUrl = Link "https://example.com"
            getHrefEl False cUrl e `shouldBe` Just (Link "https://example.com")

    describe "elemToStr" $ do
        it "renders element with no attributes" $
            elemToStr (mkElem "div" [] "content") `shouldBe` "<div>content</div>"

        it "renders element with attributes" $
            elemToStr (mkElem "a" [("href", "/page")] "link")
                `shouldBe` "<a href=\"/page\">link</a>"

    describe "treeElemToStr" $ do
        it "renders simple tree element" $ do
            let tree = mkTree "div" [] [IText "content"]
            treeElemToStr tree `shouldBe` "<div>content</div>"

        it "renders self-closing element" $ do
            let tree = mkTree "br" [] []
            treeElemToStr tree `shouldBe` "<br>"

        it "renders tree with attributes" $ do
            let tree = mkTree "div" [("class", "test")] [IText "x"]
            treeElemToStr tree `shouldContain` "class="

    describe "treeElemToStrNoMatch" $ do
        it "renders without ShowHTML constraint" $ do
            let tree = mkTree "p" [] [IText "text"]
            treeElemToStrNoMatch tree `shouldBe` "<p>text</p>"

        it "renders self-closing" $ do
            let tree = mkTree "hr" [] []
            treeElemToStrNoMatch tree `shouldBe` "<hr>"

    describe "selfClosingElems" $ do
        it "contains br" $ "br" `elem` selfClosingElems `shouldBe` True
        it "contains img" $ "img" `elem` selfClosingElems `shouldBe` True
        it "does not contain div" $ "div" `elem` selfClosingElems `shouldBe` False

    describe "endTag" $ do
        it "parses closing tag" $ do
            case parse (endTag "div") "" "</div>" of
                Right r -> r `shouldBe` "</div>"
                Left err -> expectationFailure $ show err

        it "fails on wrong tag" $
            case parse (endTag "div") "" "</span>" of
                Right _ -> expectationFailure "should fail"
                Left _ -> pure ()

    describe "enoughMatches" $ do
        it "succeeds when enough matches" $ do
            let result = parse (enoughMatches 1 "div" Map.empty ("text", ["match1", "match2"])) "" ""
            case result of
                Right e -> elTag e `shouldBe` "div"
                Left err -> expectationFailure $ show err

        it "fails when not enough matches" $ do
            let result = parse (enoughMatches 5 "div" Map.empty ("text", ["match1"])) "" ""
            case result of
                Right _ -> expectationFailure "should fail"
                Left _ -> pure ()

    describe "enoughMatchesTree" $ do
        it "succeeds when enough matches" $ do
            let result = parse (enoughMatchesTree 1 "div" Map.empty ("text", ["m1", "m2"], [])) "" ""
            case result of
                Right t -> _topEl t `shouldBe` "div"
                Left err -> expectationFailure $ show err

        it "fails when not enough matches" $ do
            let result = parse (enoughMatchesTree 5 "div" Map.empty ("text", [], [])) "" ""
            case result of
                Right _ -> expectationFailure "should fail"
                Left _ -> pure ()

    describe "foldFuncTup" $ do
        it "handles IText" $ do
            let result = foldFuncTup (IText "hello" :: HTMLMatcher Elem' String) ("", [])
            fst result `shouldBe` "hello"

        it "handles Match" $ do
            let result = foldFuncTup (Match "m" :: HTMLMatcher Elem' String) ("", [])
            snd result `shouldBe` ["m"]

        it "handles Element" $ do
            let e = mkElem "div" [] "inner"
            let result = foldFuncTup (Element e :: HTMLMatcher Elem' String) ("", [])
            fst result `shouldContain` "div"

    describe "foldFuncTrup" $ do
        it "handles IText" $ do
            let result = foldFuncTrup (IText "hi" :: HTMLMatcher TreeHTML String) ("", [], [])
            fst' result `shouldBe` "hi"

        it "handles Match" $ do
            let result = foldFuncTrup (Match "m" :: HTMLMatcher TreeHTML String) ("", [], [])
            snd' result `shouldBe` ["m"]

        it "handles Element" $ do
            let e = mkTree "span" [] [IText "x"]
            let result = foldFuncTrup (Element e :: HTMLMatcher TreeHTML String) ("", [], [])
            length (thd' result) `shouldBe` 1

    describe "foldFuncITR" $ do
        it "handles IText" $ do
            let result = foldFuncITR (IText "text" :: HTMLMatcher Elem' String) mempty
            _fullInner result `shouldBe` "text"

        it "handles Match" $ do
            let result = foldFuncITR (Match "m" :: HTMLMatcher Elem' String) mempty
            _matchesITR result `shouldBe` ["m"]

        it "handles Element" $ do
            let e = mkElem "b" [] "bold"
            let result = foldFuncITR (Element e :: HTMLMatcher Elem' String) mempty
            _fullInner result `shouldContain` "bold"

    describe "fHM_c" $ do
        it "handles IText" $ do
            let result = fHM_c (IText "hi" :: HTMLMatcher TreeHTML String) mempty
            _innerText result `shouldBe` "hi"

        it "handles Match" $ do
            let result = fHM_c (Match "m" :: HTMLMatcher TreeHTML String) mempty
            _matches result `shouldBe` ["m"]

        it "handles Element" $ do
            let e = mkTree "em" [] [IText "italic"]
            let result = fHM_c (Element e :: HTMLMatcher TreeHTML String) mempty
            _innerText result `shouldContain` "em"

    describe "makeBranch" $ do
        it "creates a tree node" $ do
            let tree = mkTree "div" [("id", "x")] []
            let branch = makeBranch tree
            case branch of
                Node (tg, _ats) _ -> tg `shouldBe` "div"

    describe "Clickable" $ do
        it "mkClickableEH extracts href" $ do
            let eh = ("a", Map.fromList [("href", "https://example.com")])
            let cUrl = Link "https://example.com"
            case mkClickableEH False cUrl eh of
                Just (Clickable _ link) -> link `shouldBe` Link "https://example.com"
                Nothing -> expectationFailure "expected Clickable"

        it "mkClickableEH returns Nothing without href" $ do
            let eh = ("div", Map.empty)
            let cUrl = Link "https://example.com"
            case mkClickableEH False cUrl eh of
                Nothing -> pure ()
                Just _ -> expectationFailure "expected Nothing"

        it "mkClickable from Elem'" $ do
            let e = mkElem "a" [("href", "https://example.com")] "link"
            let cUrl = Link "https://example.com"
            case mkClickable False cUrl e of
                Just (Clickable _ link) -> link `shouldBe` Link "https://example.com"
                Nothing -> expectationFailure "expected Clickable"

        it "getLink extracts link" $ do
            let c = Clickable ("a", Map.empty) (Link "https://example.com")
            getLink c `shouldBe` Link "https://example.com"

    describe "UrlPagination" $ do
        it "can be constructed and compared" $ do
            let up = UrlPagination "base" "page=2"
            up `shouldBe` UrlPagination "base" "page=2"

    describe "tuple helpers" $ do
        it "fst' extracts first" $ fst' (1 :: Int, 2 :: Int, 3 :: Int) `shouldBe` 1
        it "snd' extracts second" $ snd' (1 :: Int, 2 :: Int, 3 :: Int) `shouldBe` 2
        it "thd' extracts third" $ thd' (1 :: Int, 2 :: Int, 3 :: Int) `shouldBe` 3

    describe "HTMLMatcher constructors" $ do
        it "IText" $ do
            let m = IText "text" :: HTMLMatcher Elem' String
            case m of
                IText s -> s `shouldBe` "text"
                _ -> expectationFailure "wrong constructor"

        it "Element" $ do
            let e = mkElem "div" [] "x"
            let m = Element e :: HTMLMatcher Elem' String
            case m of
                Element el -> elTag el `shouldBe` "div"
                _ -> expectationFailure "wrong constructor"

        it "Match" $ do
            let m = Match "found" :: HTMLMatcher Elem' String
            case m of
                Match s -> s `shouldBe` "found"
                _ -> expectationFailure "wrong constructor"

    describe "InnerTextHTMLTree Monoid" $ do
        it "mempty has empty fields" $ do
            let m = mempty :: InnerTextHTMLTree String
            _matches m `shouldBe` []
            _innerText m `shouldBe` ""
            innerTree m `shouldBe` []

        it "semigroup combines fields" $ do
            let a = InnerTextHTMLTree ["x"] "hello" [] :: InnerTextHTMLTree String
            let b = InnerTextHTMLTree ["y"] " world" [] :: InnerTextHTMLTree String
            let c = a <> b
            _matches c `shouldBe` ["x", "y"]
            _innerText c `shouldBe` "hello world"

    describe "InnerTextResult Monoid" $ do
        it "mempty has empty fields" $ do
            let m = mempty :: InnerTextResult String
            _matchesITR m `shouldBe` []
            _fullInner m `shouldBe` ""

        it "semigroup combines fields" $ do
            let a = InnerTextResult ["x"] "a" :: InnerTextResult String
            let b = InnerTextResult ["y"] "b" :: InnerTextResult String
            let c = a <> b
            _matchesITR c `shouldBe` ["x", "y"]
            _fullInner c `shouldBe` "ab"

    describe "HTMLBare" $ do
        it "constructs with fields" $ do
            let bare = HTMLBare "div" Map.empty [] :: HTMLBare Elem' String
            tag bare `shouldBe` "div"
            attrsss bare `shouldBe` Map.empty
            length (htmlM bare) `shouldBe` 0

    describe "MessyTree constructors" $ do
        it "Noise" $ do
            let t = Noise "noise" :: MessyTree String String
            case t of
                Noise n -> n `shouldBe` "noise"
                _ -> expectationFailure "wrong"

        it "Nodee" $ do
            let t = Nodee "x" [Noise "y"] :: MessyTree String String
            case t of
                Nodee x children -> do
                    x `shouldBe` "x"
                    length children `shouldBe` 1
                _ -> expectationFailure "wrong"

    describe "MessyTreeMatch constructors" $ do
        it "Noise'" $ do
            let t = Noise' "n" :: MessyTreeMatch String String String
            case t of
                Noise' n -> n `shouldBe` "n"
                _ -> expectationFailure "wrong"

        it "Match'" $ do
            let t = Match' "m" :: MessyTreeMatch String String String
            case t of
                Match' m -> m `shouldBe` "m"
                _ -> expectationFailure "wrong"

        it "Node'" $ do
            let t = Node' "c" [] :: MessyTreeMatch String String String
            case t of
                Node' c children -> do
                    c `shouldBe` "c"
                    length children `shouldBe` 0
                _ -> expectationFailure "wrong"

    describe "ShowHTML instances for Elem' and TreeHTML" $ do
        it "showH Elem'" $ do
            let e = mkElem "p" [] "text"
            showH e `shouldBe` "<p>text</p>"

        it "showH TreeHTML" $ do
            let t = mkTree "div" [] [IText "inner"]
            showH t `shouldBe` "<div>inner</div>"

    describe "selfClosingTextful" $ do
        it "parses self-closing with text content" $ do
            let p :: ParsecT String () Identity [HTMLMatcher Elem' String]
                p = selfClosingTextful Nothing
            case parse p "" ">some text<div" of
                Right matchers -> length matchers `shouldSatisfy` (> 0)
                Left err -> expectationFailure $ show err

    describe "undefined functions" $ do
        it "foldFuncMatchlist is undefined" $
            shouldBeUndefined (foldFuncMatchlist (IText "x" :: HTMLMatcher Elem' String) (mempty :: InnerTextResult String))

        it "getSrc is undefined" $
            shouldBeUndefined (getSrc :: String)
