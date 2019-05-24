module Main exposing (Code, Direction(..), Element, FileWithTree, Tree(..), compress, decompress)

--import Bytes.Encode as Encode exposing (Encoder)

import Browser
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, h1, h2, input, text)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra
import Task


type State
    = Compression
    | Decompression


type Tree
    = Empty
    | Node Tree Tree
    | Leaf Element


type Direction
    = Left
    | Right


type alias Code =
    List Direction


type alias Element =
    { char : Char
    , numberOf : Int
    }


type alias FileWithTree =
    { text : List Code
    , tree : Tree
    }


decompress : Tree -> List Code -> String
decompress tree textInCodes =
    textInCodes
        |> List.map (getCharInTreeByCode tree)
        |> Maybe.Extra.values
        |> String.fromList


getCharInTreeByCode : Tree -> Code -> Maybe Char
getCharInTreeByCode tree code =
    case tree of
        Empty ->
            Nothing

        Leaf element ->
            Just element.char

        Node first second ->
            if List.head code == Just Left then
                getCharInTreeByCode first (List.drop 1 code)

            else
                getCharInTreeByCode second (List.drop 1 code)


stringFromCode : Code -> String
stringFromCode code =
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


compress : String -> ( List Code, Tree )
compress text =
    let
        dictFromTree =
            createCharCodeDictFromTree (generateTree text)

        codes =
            text
                |> String.toList
                |> List.map
                    (\char ->
                        Dict.get char dictFromTree |> Maybe.withDefault []
                    )

        tree =
            generateTree text
    in
    ( codes, tree )


generateTree : String -> Tree
generateTree text =
    let
        characterCounts =
            countChars text
                |> Dict.toList
                |> List.map (\( char, int ) -> Element char int)
    in
    List.map Leaf characterCounts
        |> mergeTrees


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


calculateCount : Tree -> Int
calculateCount tree =
    case tree of
        Empty ->
            0

        Leaf element ->
            element.numberOf

        Node first second ->
            calculateCount first + calculateCount second


createCharCodeDictFromTree : Tree -> Dict Char Code
createCharCodeDictFromTree tree =
    listCodeOfCharFromTree tree [] []
        |> Dict.fromList


listCodeOfCharFromTree : Tree -> Code -> List ( Char, Code ) -> List ( Char, Code )
listCodeOfCharFromTree tree currentCode listOfCodes =
    case tree of
        Empty ->
            listOfCodes

        Leaf element ->
            ( element.char, currentCode ) :: listOfCodes

        Node first second ->
            [ listCodeOfCharFromTree first (List.singleton Left |> List.append currentCode) listOfCodes
            , listCodeOfCharFromTree second (List.singleton Right |> List.append currentCode) listOfCodes
            ]
                |> List.concat


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        tree =
            generateTree model.stringFromFile

        listOfCodes =
            Tuple.first (compress model.stringFromFile)
    in
    div []
        [ div []
            [ h1 [] [ text "Huffman-Coding" ]
            , div [] [ text "Wollen Sie eine Datei Komprimieren oder Dekomprimieren?" ]
            , div []
                [ button [ onClick (ChangeState Compression) ] [ text "Komprimieren" ]
                , button [ onClick (ChangeState Decompression) ] [ text "Dekomprimieren" ]
                ]
            ]
        , case model.state of
            Just Compression ->
                div []
                    [ h1 [] [ text "Datei Kompremierung:" ]
                    , div [] [ text "Bitte füllen Sie dieses Textfeld aus, oder drücken sie auf den Button um eine Datei zu komprimieren" ]
                    , input [ onInput NewInput ] []
                    , button [ onClick FileRequested ] [ text "Wählen sie eine Datei zur Komprimierung aus" ]
                    , if String.isEmpty model.stringFromFile then
                        div [] []

                      else
                        div []
                            [ div []
                                [ h1 [] [ text "Infos zur Komprimierung:" ]
                                , h2 [] [ text "Das beinhaltet der Baum:" ]
                                , div [] [ viewTree tree ]
                                ]
                            , div []
                                [ h2 [] [ text "Der Text/Die Datei mit den neuen Bits / After compression:" ]
                                , div [] (List.map (\code -> text (code ++ " ")) (List.map stringFromCode (Tuple.first (compress model.stringFromFile))))
                                ]
                            , div []
                                [ h2 [] [ text "Der Text/Die Datei nach der Wiederherstellung / After compression & decompression" ]
                                , div []
                                    [ let
                                        afterCompress =
                                            compress model.stringFromFile
                                      in
                                      text (decompress (Tuple.second afterCompress) (Tuple.first afterCompress))
                                    ]
                                ]
                            , div []
                                [ h2 [] [ text "Download-File" ]
                                , div []
                                    [ if String.isEmpty model.stringFromFile then
                                        text ""

                                      else
                                        button [] [ text "Hier soll der Download der komprimierten Datei gestartet werden können." ]
                                    ]
                                ]
                            , div []
                                [ h2 [] [ text "Wörter & Zeichen:" ]
                                , div [] [ model.stringFromFile |> String.words |> List.length |> String.fromInt |> (++) "Genutzte Wörter: " |> text ]
                                , div [] [ List.length listOfCodes |> String.fromInt |> (++) "Genutzte Zeichen: " |> text ]
                                , div [] [ "Der String beinhaltet: " ++ (countChars model.stringFromFile |> Dict.size |> String.fromInt) ++ " verschiedene Zeichen!" |> text ]
                                ]
                            ]
                    ]

            Just Decompression ->
                div []
                    [ h1 [] [ text "Datei Dekomprimierung: " ]
                    , div [] [ text "Falls Sie eine Datei dekomprimieren wollen, dann klicken Sie auf den Button und wählen eine Datei aus." ]
                    , div [] [ button [ onClick FileRequested ] [ text "Bitte wählen Sie eine Datei zur Dekomprimierung aus" ] ]
                    , div []
                        [ text "Hier soll die alte Datei stehen" ]
                    ]

            Nothing ->
                div [] []
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
                div [] [ text ("Das Element " ++ (element.char |> String.fromChar) ++ " ist " ++ (element.numberOf |> String.fromInt) ++ "x vertreten und wird zur Bitfolge " ++ currentCode ++ " komprimiert") ]

            Empty ->
                div [] [ text "Dieser Baum ist leer." ]
        ]


type alias Model =
    { state : Maybe State
    , stringFromFile : String
    , codeList : List Code
    , bytes : Maybe Bytes
    }


type Msg
    = NewInput String
    | FormInBytes Bytes
    | FileRequested
    | FileLoaded File
    | ChangeState State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput newInput ->
            ( { model | stringFromFile = newInput }, Cmd.none )

        ChangeState state ->
            ( { model | state = Just state }, Cmd.none )

        FileRequested ->
            ( model, Select.file [] FileLoaded )

        FileLoaded file ->
            ( model, Task.perform FormInBytes (File.toBytes file) )

        FormInBytes bytes ->
            ( let
                stringFromFile =
                    Decode.decode (Decode.string (Bytes.width bytes)) bytes |> Maybe.withDefault ""
              in
              { model
                | stringFromFile = stringFromFile
                , codeList = Tuple.first (compress model.stringFromFile)
                , bytes = Just bytes
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Nothing
      , stringFromFile = ""
      , codeList = []
      , bytes = Nothing
      }
    , Cmd.none
    )
