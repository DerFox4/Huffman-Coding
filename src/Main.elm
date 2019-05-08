module Main exposing (compression, decompression)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h2, input, text)
import Html.Events exposing (onInput)
import Maybe.Extra


type Tree
    = Empty
    | Node Tree Tree
    | Leaf Element


type Direction
    = Left
    | Right


type alias Element =
    ( Char, Int )


decompression : Tree -> List (List Direction) -> String
decompression tree textInCodes =
    let
        codesFromTree =
            List.map Tuple.second (codeOfLeafs tree)
    in
    List.map
        (\charInCode ->
            if List.member charInCode codesFromTree then
                searchCharByCode tree charInCode

            else
                ""
        )
        textInCodes
        |> String.concat


searchCharByCode : Tree -> List Direction -> String
searchCharByCode tree code =
    case tree of
        Empty ->
            ""

        Leaf element ->
            Tuple.first element |> String.fromChar

        Node first second ->
            if List.head code == Just Left then
                searchCharByCode first (List.tail code |> Maybe.Extra.toList |> List.concat)

            else
                searchCharByCode second (List.tail code |> Maybe.Extra.toList |> List.concat)


compression : String -> ( Tree, List (List Direction) )
compression text =
    let
        tree =
            generateTree text
    in
    ( tree
    , text
        |> String.toList
        |> List.map (getCodeByChar (codeOfLeafs tree))
        |> Maybe.Extra.values
    )


getCodeByChar : List ( Char, List Direction ) -> Char -> Maybe (List Direction)
getCodeByChar codes searchedChar =
    codes
        |> Dict.fromList
        |> Dict.get searchedChar


codeOfLeafs : Tree -> List ( Char, List Direction )
codeOfLeafs tree =
    codeOfLeafsHelp tree [] []


codeOfLeafsHelp : Tree -> List Direction -> List ( Char, List Direction ) -> List ( Char, List Direction )
codeOfLeafsHelp tree currentCode listOfCodes =
    case tree of
        Empty ->
            listOfCodes

        Leaf element ->
            ( Tuple.first element, currentCode ) :: listOfCodes

        Node first second ->
            let
                reversedlist =
                    List.reverse currentCode
            in
            [ codeOfLeafsHelp first (Left :: reversedlist |> List.reverse) listOfCodes
            , codeOfLeafsHelp second (Right :: reversedlist |> List.reverse) listOfCodes
            ]
                |> List.concat


generateTree : String -> Tree
generateTree text =
    let
        characterCounts =
            countChars text
                |> Dict.toList
    in
    List.map Leaf characterCounts
        |> mergeTrees


calculateCount : Tree -> Int
calculateCount tree =
    case tree of
        Empty ->
            0

        Leaf element ->
            Tuple.second element

        Node first second ->
            calculateCount first + calculateCount second


mergeTrees : List Tree -> Tree
mergeTrees trees =
    case trees of
        [] ->
            Empty

        first :: [] ->
            first

        _ ->
            mergeLowestCounts trees
                |> mergeTrees


mergeLowestCounts : List Tree -> List Tree
mergeLowestCounts trees =
    let
        sortedTrees =
            List.sortBy calculateCount trees
    in
    case sortedTrees of
        first :: second :: tail ->
            Node first second :: tail

        _ ->
            trees


countChars : String -> Dict Char Int
countChars text =
    countCharsHelp (String.toList text) Dict.empty


countCharsHelp : List Char -> Dict Char Int -> Dict Char Int
countCharsHelp text characterCounts =
    case text of
        currentChar :: remainingCharacters ->
            countCharsHelp remainingCharacters (incrementCounter currentChar characterCounts)

        [] ->
            characterCounts


incrementCounter : comparable -> Dict comparable Int -> Dict comparable Int
incrementCounter key =
    Dict.update key <|
        \value ->
            case value of
                Just existingCount ->
                    Just (existingCount + 1)

                Nothing ->
                    Just 1


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    String


type Msg
    = DAS String


view : Model -> Html Msg
view model =
    let
        thisTree =
            Tuple.first (compression model)

        allCodes =
            Tuple.second (compression model)
    in
    div []
        [ input [ onInput DAS ] []
        , div []
            [ h2 [] [ text "So sieht der Baum aus:" ]
            , div [] [ viewTree thisTree ]
            ]
        , div []
            [ h2 [] [ text "Dies ist der Text angegeben in den Schlüsseln der Bits / After compression:" ]
            , div [] (List.map (\code -> text (code ++ " ")) (List.map foo allCodes))
            ]
        , div []
            [ h2 [] [ text "Der Text nach der Wiederherstellung / After compression & decompression" ]
            , div [] [ text (decompression thisTree allCodes) ]
            ]
        , div []
            [ h2 [] [ text "Der original Text:" ]
            , div [] [ text model ]
            ]
        ]


foo : List Direction -> String
foo code =
    List.map
        (\direction ->
            case direction of
                Left ->
                    '0'

                Right ->
                    '1'
        )
        code
        |> String.fromList


viewTree : Tree -> Html Msg
viewTree tree =
    viewTreeHelp tree ""


viewTreeHelp : Tree -> String -> Html Msg
viewTreeHelp tree currentCode =
    div []
        [ case tree of
            Node first second ->
                div []
                    [ div [] [ viewTreeHelp first (currentCode ++ "0") ]
                    , div [] [ viewTreeHelp second (currentCode ++ "1") ]
                    ]

            Leaf element ->
                div [] [ text ("Das Element " ++ (Tuple.first element |> String.fromChar) ++ " ist " ++ (Tuple.second element |> String.fromInt) ++ "x vertreten und wird mit " ++ currentCode ++ " komprimiert") ]

            Empty ->
                div [] [ text "Ein Empty-Baum" ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        DAS string ->
            string


init : Model
init =
    ""
