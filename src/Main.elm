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


type alias Element =
    ( Char, Int )


decompression : Tree -> List String -> String
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


searchCharByCode : Tree -> String -> String
searchCharByCode tree code =
    case tree of
        Empty ->
            "Err: Empty Tree"

        Leaf element ->
            Tuple.first element |> String.fromChar

        Node first second ->
            if String.startsWith "0" code then
                searchCharByCode first (String.dropLeft 1 code)

            else if String.startsWith "1" code then
                searchCharByCode second (String.dropLeft 1 code)

            else
                "Err: It's not 0 or 1"


compression : String -> ( Tree, List String )
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


getCodeByChar : List ( Char, String ) -> Char -> Maybe String
getCodeByChar codes searchedChar =
    codes
        |> Dict.fromList
        |> Dict.get searchedChar


codeOfLeafs : Tree -> List ( Char, String )
codeOfLeafs tree =
    codeOfLeafsHelp tree "" []


codeOfLeafsHelp : Tree -> String -> List ( Char, String ) -> List ( Char, String )
codeOfLeafsHelp tree currentCode listOfCodes =
    case tree of
        Empty ->
            listOfCodes

        Leaf element ->
            ( Tuple.first element, currentCode ) :: listOfCodes

        Node first second ->
            [ codeOfLeafsHelp first (currentCode ++ "0") listOfCodes
            , codeOfLeafsHelp second (currentCode ++ "1") listOfCodes
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
            [ h2 [] [ text "Dies ist der Text angegeben in den Schlüsseln der Bytes / After compression:" ]
            , div [] (List.map (\code -> text (code ++ " ")) allCodes)
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
