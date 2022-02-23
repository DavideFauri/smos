{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Smos.Report.FilterSpec
  ( spec,
  )
where

import Control.Arrow (left)
import Control.Monad
import Cursor.Forest.Gen ()
import Cursor.Simple.Forest
import Data.Functor.Identity
import Data.Maybe
import qualified Data.Text as T
import Path
import Smos.Data
import Smos.Report.Comparison
import Smos.Report.Filter
import Smos.Report.Filter.Gen ()
import Smos.Report.Time hiding (P)
import Test.QuickCheck as QC
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Text.Parsec

spec :: Spec
spec = do
  describe "Tokenisation" $ do
    genValidSpec @Part
    genValidSpec @Parts
    describe "renderParts" $ it "produces valid texts" $ producesValid renderParts
    describe "partP" $ do
      parseSuccessSpec partP ":" PartColumn
      parseSuccessSpec partP "file" (PartPiece (Piece "file"))
      parseSuccessSpec partP "(" (PartParen OpenParen)
      parseSuccessSpec partP ")" (PartParen ClosedParen)
      parseSuccessSpec partP " " PartSpace
      parseSuccessSpec partP "and" (PartBinOp AndOp)
      parseSuccessSpec partP "or" (PartBinOp OrOp)
      parseSuccessSpec partP "a" (PartPiece (Piece "a"))
      parseSuccessSpec partP "o" (PartPiece (Piece "o"))
      parsesValidSpec partP
      it "parses back whatever 'renderPart' renders" $
        forAllValid $
          \p ->
            let t = renderPart p
             in case parsePart t of
                  Left err -> expectationFailure $ show err
                  Right p' -> p' `shouldBe` p
    describe "partsP" $ do
      parseSuccessSpec
        partsP
        "file:side"
        (Parts [PartPiece (Piece "file"), PartColumn, PartPiece (Piece "side")])
      parseSuccessSpec
        partsP
        "(file:side and level:3)"
        ( Parts
            [ PartParen OpenParen,
              PartPiece (Piece "file"),
              PartColumn,
              PartPiece (Piece "side"),
              PartSpace,
              PartBinOp AndOp,
              PartSpace,
              PartPiece (Piece "level"),
              PartColumn,
              PartPiece (Piece "3"),
              PartParen ClosedParen
            ]
        )
      parsesValidSpec partsP
      it "parses back whatever 'renderParts' renders" $
        forAllValid $
          \parts ->
            let t = renderParts parts
             in case parseParts t of
                  Left err -> expectationFailure $ show err
                  Right parts' -> parts' `shouldBe` parts
  describe "Parsing" $ do
    genValidSpec @Ast
    describe "astP" $ do
      parseSuccessSpec
        astP
        [PartPiece (Piece "file"), PartColumn, PartPiece (Piece "side")]
        (AstUnOp (Piece "file") (AstPiece (Piece "side")))
      parseSuccessSpec
        astP
        [ PartParen OpenParen,
          PartPiece (Piece "file"),
          PartColumn,
          PartPiece (Piece "side"),
          PartSpace,
          PartBinOp AndOp,
          PartSpace,
          PartPiece (Piece "level"),
          PartColumn,
          PartPiece (Piece "3"),
          PartParen ClosedParen
        ]
        ( AstBinOp
            (AstUnOp (Piece "file") (AstPiece (Piece "side")))
            AndOp
            (AstUnOp (Piece "level") (AstPiece (Piece "3")))
        )
      parsesValidSpec astP
      it "parses back whatever 'renderAst' renders" $
        forAllValid $ \ast ->
          let t = renderAstParts ast
           in context (show t) $ case parseAstParts t of
                Left err -> expectationFailure $ show err
                Right ast' -> ast' `shouldBe` ast
  describe "Type-checking" $ do
    filterArgumentSpec @Time
    filterArgumentSpec @Tag
    filterArgumentSpec @Header
    filterArgumentSpec @TodoState
    filterArgumentSpec @PropertyName
    filterArgumentSpec @PropertyValue
    filterArgumentSpec @TimestampName
    filterArgumentSpec @Timestamp
    filterArgumentSpec @(Path Rel File)
    genValidSpec @Time
    genValidSpec @(Filter Time)
    eqSpec @(Filter Time)
    eqSpec @(Filter Tag)
    genValidSpec @(Filter Tag)
    eqSpec @(Filter Header)
    genValidSpec @(Filter Header)
    eqSpec @(Filter TodoState)
    genValidSpec @(Filter TodoState)
    eqSpec @(Filter PropertyValue)
    genValidSpec @(Filter PropertyValue)
    eqSpec @EntryFilter
    genValidSpec @EntryFilter
    jsonSpec @EntryFilter
    describe "tcWithTopLevelBranches" $ do
      tcSpec
        (tcWithTopLevelBranches tcSub)
        (AstUnOp (Piece "sub") (AstPiece (Piece "header")))
        (FilterSub (fromJust $ header "header"))
      tcSpec
        (tcWithTopLevelBranches tcSub)
        (AstUnOp (Piece "not") (AstUnOp (Piece "sub") (AstPiece (Piece "header"))))
        (FilterNot (FilterSub (fromJust $ header "header")))
      tcSpec
        (tcWithTopLevelBranches tcSub)
        ( AstBinOp
            (AstUnOp (Piece "sub") (AstPiece (Piece "header1")))
            AndOp
            (AstUnOp (Piece "sub") (AstPiece (Piece "header2")))
        )
        ( FilterAnd
            (FilterSub (fromJust $ header "header1"))
            (FilterSub (fromJust $ header "header2"))
        )
      tcSpec
        (tcWithTopLevelBranches tcSub)
        ( AstBinOp
            (AstUnOp (Piece "sub") (AstPiece (Piece "header1")))
            OrOp
            (AstUnOp (Piece "sub") (AstPiece (Piece "header2")))
        )
        ( FilterOr
            (FilterSub (fromJust $ header "header1"))
            (FilterSub (fromJust $ header "header2"))
        )
    describe "tcFilePathFilter" $
      tcSpec
        tcFilePathFilter
        (AstUnOp (Piece "file") (AstPiece (Piece "side")))
        (FilterFile [relfile|side|])
    describe "tcSub" $ do
      tcSpec
        tcSub
        (AstUnOp (Piece "sub") (AstPiece (Piece "toast")))
        (FilterSub (fromJust $ tag "toast"))
      tcSpec tcSub (AstPiece (Piece "toast")) (FilterSub (fromJust $ tag "toast"))
      tcSpec
        tcSub
        (AstUnOp (Piece "sub") (AstPiece (Piece "header")))
        (FilterSub (fromJust $ header "header"))
      tcSpec tcSub (AstPiece (Piece "header")) (FilterSub (fromJust $ header "header"))
      tcSpec
        tcSub
        (AstUnOp (Piece "sub") (AstPiece (Piece "TODO")))
        (FilterSub (fromJust $ todoState "TODO"))
      tcSpec tcSub (AstPiece (Piece "TODO")) (FilterSub (fromJust $ todoState "TODO"))
    describe "tcOrd" $ do
      tcSpec
        tcOrd
        (AstUnOp (Piece "ord") (AstUnOp (Piece "lt") (AstPiece (Piece "5m"))))
        (FilterOrd LTC (Minutes 5))
      tcSpec
        tcOrd
        (AstUnOp (Piece "ord") (AstUnOp (Piece "gt") (AstPiece (Piece "6h"))))
        (FilterOrd GTC (Hours 6))
    describe "tcTimeFilter" $
      tcSpec
        tcTimeFilter
        (AstUnOp (Piece "ord") (AstUnOp (Piece "gt") (AstPiece (Piece "6h"))))
        (FilterOrd GTC (Hours 6))
    describe "tcTagFilter" $ do
      tcSpec
        tcTagFilter
        (AstUnOp (Piece "sub") (AstPiece (Piece "toast")))
        (FilterSub (fromJust $ tag "toast"))
      tcSpec tcTagFilter (AstPiece (Piece "toast")) (FilterSub (fromJust $ tag "toast"))
    describe "tcHeaderFilter" $ do
      tcSpec
        tcHeaderFilter
        (AstUnOp (Piece "sub") (AstPiece (Piece "header")))
        (FilterSub (fromJust $ header "header"))
      tcSpec tcHeaderFilter (AstPiece (Piece "header")) (FilterSub (fromJust $ header "header"))
    describe "tcTodoStateFilter" $ do
      tcSpec
        tcTodoStateFilter
        (AstUnOp (Piece "sub") (AstPiece (Piece "TODO")))
        (FilterSub (fromJust $ todoState "TODO"))
      tcSpec tcTodoStateFilter (AstPiece (Piece "TODO")) (FilterSub (fromJust $ todoState "TODO"))
    describe "tcMaybeFilter" $
      tcSpec
        (tcMaybeFilter tcTimeFilter)
        ( AstUnOp
            (Piece "maybe")
            ( AstUnOp
                (Piece "false")
                (AstUnOp (Piece "ord") (AstUnOp (Piece "gt") (AstPiece (Piece "7s"))))
            )
        )
        (FilterMaybe False (FilterOrd GTC (Seconds 7)))
    describe "tcPropertyValueFilter" $ do
      tcSpec
        tcPropertyValueFilter
        (AstUnOp (Piece "sub") (AstPiece (Piece "propertyValue")))
        (FilterSub (fromJust $ propertyValue "propertyValue"))
      tcSpec
        tcPropertyValueFilter
        (AstPiece (Piece "propertyValue"))
        (FilterSub (fromJust $ propertyValue "propertyValue"))
      tcSpec
        tcPropertyValueFilter
        ( AstUnOp
            (Piece "time")
            ( AstUnOp
                (Piece "maybe")
                ( AstUnOp
                    (Piece "false")
                    (AstUnOp (Piece "ord") (AstUnOp (Piece "gt") (AstPiece (Piece "7s"))))
                )
            )
        )
        (FilterPropertyTime (FilterMaybe False (FilterOrd GTC (Seconds 7))))
    describe "tcMapFilter" $
      tcSpec
        (tcMapFilter tcPropertyValueFilter)
        ( AstUnOp
            (Piece "val")
            ( AstUnOp
                (Piece "client")
                ( AstUnOp
                    (Piece "maybe")
                    (AstUnOp (Piece "false") (AstUnOp (Piece "sub") (AstPiece (Piece "cssyd"))))
                )
            )
        )
        ( FilterMapVal
            (fromJust $ propertyName "client")
            (FilterMaybe False (FilterSub (fromJust $ propertyValue "cssyd")))
        )
    describe "tcPropertiesFilter" $ do
      tcSpec
        tcPropertiesFilter
        ( AstUnOp
            (Piece "val")
            ( AstUnOp
                (Piece "client")
                ( AstUnOp
                    (Piece "maybe")
                    (AstUnOp (Piece "false") (AstUnOp (Piece "sub") (AstPiece (Piece "cssyd"))))
                )
            )
        )
        ( FilterMapVal
            (fromJust $ propertyName "client")
            (FilterMaybe False (FilterSub (fromJust $ propertyValue "cssyd")))
        )
      tcSpec
        tcPropertiesFilter
        ( AstUnOp
            "timewindow"
            ( AstUnOp
                "time"
                ( AstUnOp
                    "ord"
                    ( AstUnOp
                        "lt"
                        (AstPiece "2h")
                    )
                )
            )
        )
        ( FilterMapVal "timewindow" $
            FilterMaybe False $
              FilterPropertyTime $
                FilterMaybe False $
                  FilterOrd LTC $
                    Hours 2
        )
      tcSpec
        tcPropertiesFilter
        ( AstUnOp
            "snd"
            ( AstUnOp
                "cursor"
                ( AstUnOp
                    "properties"
                    ( AstUnOp
                        "val"
                        ( AstUnOp
                            "timewindow"
                            ( AstUnOp
                                "maybe"
                                ( AstUnOp
                                    "False"
                                    ( AstUnOp
                                        "time"
                                        ( AstUnOp
                                            "maybe"
                                            ( AstUnOp
                                                "False"
                                                ( AstUnOp
                                                    "ord"
                                                    ( AstUnOp
                                                        "lt"
                                                        (AstPiece "2h")
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
        ( FilterMapVal "timewindow" $
            FilterMaybe False $
              FilterPropertyTime $
                FilterMaybe False $
                  FilterOrd LTC $
                    Hours 2
        )

    describe "tcSetFilter" $ do
      tcSpec
        (tcSetFilter tcTagFilter)
        (AstUnOp (Piece "any") (AstUnOp (Piece "sub") (AstPiece (Piece "toast"))))
        (FilterAny (FilterSub "toast"))
      tcSpec
        (tcSetFilter tcTagFilter)
        (AstUnOp (Piece "all") (AstUnOp (Piece "sub") (AstPiece (Piece "a"))))
        (FilterAll (FilterSub "a"))
    describe "tcTagsFilter" $ do
      tcSpec
        tcTagsFilter
        (AstUnOp (Piece "any") (AstUnOp (Piece "sub") (AstPiece (Piece "toast"))))
        (FilterAny (FilterSub "toast"))
      tcSpec
        tcTagsFilter
        (AstUnOp (Piece "all") (AstUnOp (Piece "sub") (AstPiece (Piece "a"))))
        (FilterAll (FilterSub "a"))
    describe "tcEntryFilter" $ do
      tcSpec
        tcEntryFilter
        (AstUnOp (Piece "header") (AstUnOp (Piece "sub") (AstPiece (Piece "header"))))
        (FilterEntryHeader (FilterSub "header"))
      tcSpec
        tcEntryFilter
        ( AstUnOp
            (Piece "state")
            ( AstUnOp
                (Piece "maybe")
                (AstUnOp (Piece "false") (AstUnOp (Piece "sub") (AstPiece (Piece "TODO"))))
            )
        )
        (FilterEntryTodoState (FilterMaybe False (FilterSub "TODO")))
      tcSpec
        tcEntryFilter
        ( AstUnOp
            (Piece "tags")
            (AstUnOp (Piece "all") (AstUnOp (Piece "sub") (AstPiece (Piece "a"))))
        )
        (FilterEntryTags (FilterAll (FilterSub "a")))
      tcSpec
        tcEntryFilter
        ( AstUnOp
            (Piece "properties")
            ( AstUnOp
                (Piece "val")
                ( AstUnOp
                    (Piece "client")
                    ( AstUnOp
                        (Piece "maybe")
                        (AstUnOp (Piece "false") (AstUnOp (Piece "sub") (AstPiece (Piece "cssyd"))))
                    )
                )
            )
        )
        ( FilterEntryProperties
            ( FilterMapVal
                "client"
                (FilterMaybe False (FilterSub "cssyd"))
            )
        )
    describe "tcForestCursorFilter" $ do
      tcSpec
        (tcForestCursorFilter tcEntryFilter)
        (AstUnOp (Piece "level") (AstPiece (Piece "1")))
        (FilterLevel 1)
      tcSpec
        (tcForestCursorFilter tcSub)
        ( AstUnOp
            (Piece "parent")
            (AstUnOp (Piece "cursor") (AstUnOp (Piece "sub") (AstPiece (Piece "header"))))
        )
        (FilterParent (FilterWithinCursor (FilterSub ("header" :: Header))))
    describe "tcTupleFilter" $ do
      tcSpec
        (tcTupleFilter tcFilePathFilter tcEntryFilter)
        (AstUnOp (Piece "fst") (AstUnOp (Piece "file") (AstPiece (Piece "side"))))
        (FilterFst (FilterFile [relfile|side|]))
      tcSpec
        (tcTupleFilter tcEntryFilter tcSub)
        (AstUnOp (Piece "snd") (AstUnOp (Piece "sub") (AstPiece (Piece "header"))))
        (FilterSnd (FilterSub ("header" :: Header)))
    describe "renderFilterAst" $ do
      it "produces valid asts for header filters" $ producesValid (renderFilterAst @Header)
      it "produces valid asts for file filters" $
        producesValid (renderFilterAst @(Path Rel File))
      it "produces valid asts for entryFilters" $
        producesValid (renderFilterAst @(Path Rel File, ForestCursor Entry))
    describe "parseEntryFilterAst" $
      it "parses back whatever 'renderFilterAst' renders" $
        forAllValid $
          \f ->
            let t = renderFilterAst f
             in case parseEntryFilterAst t of
                  Left err ->
                    expectationFailure $
                      unlines
                        [ "Original filter:",
                          ppShow f,
                          "rendered ast:",
                          ppShow t,
                          "parse failure:",
                          show err
                        ]
                  Right f' ->
                    let ctx =
                          unlines
                            [ "Original filter:",
                              ppShow f,
                              "rendered ast:",
                              ppShow t,
                              "parsed filter:",
                              ppShow f'
                            ]
                     in context ctx $ f' `shouldBe` f
    describe "renderFilter" $
      it "produces valid text" $
        producesValid (renderFilter @(Path Rel File, ForestCursor Entry))
    describe "parseEntryFilter" $
      it "parses back whatever 'renderFilter' renders" $
        forAllValid $ \f -> do
          let t = renderFilter f
          case parseEntryFilter t of
            Left err ->
              expectationFailure $
                unlines
                  [ "Original filter:",
                    ppShow f,
                    "rendered text:",
                    show t,
                    "parse failure:",
                    show err
                  ]
            Right f' ->
              let ctx =
                    unlines
                      [ "Original filter:",
                        ppShow f,
                        "rendered text:",
                        show t,
                        "parsed filter:",
                        ppShow f'
                      ]
               in context ctx $ f' `shouldBe` f
  describe "foldFilterAnd" $
    it "produces valid results" $
      producesValid (foldFilterAnd @Header)
  describe "filterPredicate" $
    it "produces valid results" $
      producesValid2 (filterPredicate @(Path Rel File, ForestCursor Entry))
  describe "parseEntryFilter" $ do
    let pe input expected =
          it (unwords ["succesfully parses", show input, "into", show expected]) $
            case parseEntryFilter input of
              Left err -> expectationFailure $ T.unpack $ prettyFilterParseError err
              Right actual -> actual `shouldBe` expected
        pee input expected = pe input (FilterSnd $ FilterWithinCursor expected)

    let fileFilter = FilterFst $ FilterFile [relfile|side|]
    pe "fst:file:side" fileFilter
    pe "file:side" fileFilter

    let headerFilter = FilterEntryHeader $ FilterSub "head"
    pee "header:head" headerFilter
    pee "snd:header:head" headerFilter
    pee "header:sub:head" headerFilter
    pee "snd:cursor:header:sub:head" headerFilter

    let tagToastfilter = FilterEntryTags $ FilterAny $ FilterSub "toast"
    pee "tag:toast" tagToastfilter
    pee "cursor:tag:toast" tagToastfilter
    pee "snd:tag:toast" tagToastfilter
    pee "snd:cursor:tag:toast" tagToastfilter
    pee "tags:toast" tagToastfilter
    pee "snd:tags:toast" tagToastfilter
    pee "snd:cursor:tags:toast" tagToastfilter
    pee "snd:cursor:tags:any:toast" tagToastfilter
    pee "snd:cursor:tags:any:sub:toast" tagToastfilter

    let stateDoneFilter = FilterEntryTodoState $ FilterMaybe False $ FilterSub "DONE"
    pee "state:DONE" stateDoneFilter
    pee "snd:state:DONE" stateDoneFilter
    pee "cursor:state:DONE" stateDoneFilter
    pee "snd:cursor:state:DONE" stateDoneFilter
    -- pee "snd:cursor:state:maybe:DONE" stateDoneFilter
    pee "snd:cursor:state:maybe:false:DONE" stateDoneFilter
    pee "snd:cursor:state:maybe:false:sub:DONE" stateDoneFilter
    pee "snd:cursor:state:maybe:False:sub:DONE" stateDoneFilter

    let propertyHasTimewindow = FilterEntryProperties $ FilterMapHas "timewindow"
    pee "property:timewindow" propertyHasTimewindow
    pee "properties:timewindow" propertyHasTimewindow
    pee "properties:has:timewindow" propertyHasTimewindow
    pee "snd:properties:has:timewindow" propertyHasTimewindow
    pee "cursor:properties:has:timewindow" propertyHasTimewindow
    pee "snd:cursor:properties:has:timewindow" propertyHasTimewindow

    let propertyClientNasa =
          FilterEntryProperties $
            FilterMapVal "client" $
              FilterMaybe False $
                FilterSub $
                  fromJust $
                    propertyValue "nasa"
    pee "property:client:nasa" propertyClientNasa
    pee "properties:client:nasa" propertyClientNasa
    pee "property:client:maybe:false:nasa" propertyClientNasa
    pee "properties:client:maybe:false:nasa" propertyClientNasa
    pee "properties:client:maybe:false:sub:nasa" propertyClientNasa
    pee "cursor:properties:client:maybe:false:sub:nasa" propertyClientNasa
    pee "snd:properties:client:maybe:false:sub:nasa" propertyClientNasa
    pee "snd:cursor:properties:client:maybe:false:sub:nasa" propertyClientNasa
    pee "properties:client:maybe:False:sub:nasa" propertyClientNasa

    let propertyTimeLt2h =
          FilterEntryProperties $
            FilterMapVal (fromJust $ propertyName "timewindow") $
              FilterMaybe False $
                FilterPropertyTime $
                  FilterMaybe False $
                    FilterOrd LTC $
                      Hours 2
    pee "properties:timewindow:time:lt:2h" propertyTimeLt2h
    pee "properties:timewindow:time:ord:lt:2h" propertyTimeLt2h
    pee "properties:timewindow:time:maybe:false:ord:lt:2h" propertyTimeLt2h
    pee "properties:timewindow:time:maybe:False:ord:lt:2h" propertyTimeLt2h
    pee "properties:timewindow:maybe:false:time:maybe:False:ord:lt:2h" propertyTimeLt2h
    pee "properties:timewindow:maybe:False:time:maybe:False:ord:lt:2h" propertyTimeLt2h
    pee "properties:val:timewindow:maybe:False:time:maybe:False:ord:lt:2h" propertyTimeLt2h
    pee "snd:properties:val:timewindow:maybe:False:time:maybe:False:ord:lt:2h" propertyTimeLt2h
    pee "cursor:properties:val:timewindow:maybe:False:time:maybe:False:ord:lt:2h" propertyTimeLt2h
    pee "snd:cursor:properties:val:timewindow:maybe:False:time:maybe:False:ord:lt:2h" propertyTimeLt2h

    pe
      "ancestor:tag:(home or (online or offline))"
      ( FilterSnd $
          FilterAncestor $
            FilterWithinCursor $
              FilterEntryTags $
                FilterOr
                  (FilterAny $ FilterSub (fromJust $ tag "home"))
                  ( FilterOr
                      (FilterAny $ FilterSub (fromJust $ tag "online"))
                      (FilterAny $ FilterSub (fromJust $ tag "offline"))
                  )
      )
  describe "examples" $
    forM_ entryFilterExamples $ \(description, filterFile, entryFilter) ->
      describe (T.unpack description) $ do
        it "is valid" $ shouldBeValid entryFilter

        let fileWithExtension ext = "test_resources/filter/" <> filterFile <> "." <> ext

        let astFile = fileWithExtension "ast"
        it "renders to the same ast as before" $
          pureGoldenStringFile
            astFile
            (ppShow (renderFilterAst entryFilter) <> "\n")

        it "roundtrips through an ast" $
          let rendered = renderFilterAst entryFilter
              parsed = parseEntryFilterAst rendered
           in context (show rendered) $ case parsed of
                Left err -> expectationFailure $ T.unpack $ prettyFilterTypeError err
                Right actual -> actual `shouldBe` entryFilter

        let partsFile = fileWithExtension "parts"
        it "renders to the same parts as before" $
          pureGoldenStringFile
            partsFile
            (ppShow (renderAstParts (renderFilterAst entryFilter)) <> "\n")

        it "roundtrips through parts" $
          let rendered = renderAstParts $ renderFilterAst entryFilter
              parsed = do
                ast <- left ParsingError $ parseAstParts rendered
                left TypeCheckingError $ parseEntryFilterAst ast
           in context (show rendered) $ case parsed of
                Left err -> expectationFailure $ T.unpack $ prettyFilterParseError err
                Right actual -> actual `shouldBe` entryFilter

        let textFile = fileWithExtension "txt"
        it "renders to the same text as before" $
          pureGoldenTextFile
            textFile
            (renderFilter entryFilter <> "\n")

        it "roundtrips through text" $
          let rendered = renderFilter entryFilter
              parsed = parseEntryFilter rendered
           in context (show rendered) $ case parsed of
                Left err -> expectationFailure $ T.unpack $ prettyFilterParseError err
                Right actual -> actual `shouldBe` entryFilter

tcSpec :: (Show a, Eq a) => TC a -> Ast -> a -> Spec
tcSpec tc ast a =
  it (unwords ["succesfully type-checks", show ast, "into", show a]) $
    case tc ast of
      Left err -> expectationFailure $ T.unpack $ prettyFilterTypeError err
      Right r -> r `shouldBe` a

parsesValidSpec ::
  (Show a, Validity a, Show s, Stream s Identity m, GenValid s) => Parsec s () a -> Spec
parsesValidSpec parser =
  it "produces valid values whenever parsing succeeds" $
    forAllValid $
      \input ->
        let isRight (Right _) = True
            isRight _ = False
            res = parse parser "test input" input
         in cover 10 (isRight res) "parses" $
              case res of
                Left _ -> pure ()
                Right actual -> shouldBeValid actual

parseSuccessSpec :: (Show a, Eq a, Show s, Stream s Identity m) => Parsec s () a -> s -> a -> Spec
parseSuccessSpec parser input expected =
  it (unwords ["succesfully parses", show input, "into", show expected]) $
    parseSuccess parser input expected

parseSuccess :: (Show a, Eq a, Stream s Identity m) => Parsec s () a -> s -> a -> Expectation
parseSuccess parser input expected =
  case parse parser "test input" input of
    Left pe -> expectationFailure $ show pe
    Right actual -> actual `shouldBe` expected

filterArgumentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, FilterArgument a) =>
  Spec
filterArgumentSpec =
  specify "parseArgument and renderArgument are inverses" $
    forAllValid $ \a ->
      parseArgument (renderArgument (a :: a)) `shouldBe` Right (a :: a)
