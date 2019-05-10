module Main exposing (compress, decompress)

import Browser
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, h2, input, text)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra
import Task


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
    ( Char, Int )


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
            Just (Tuple.first element)

        Node first second ->
            if List.head code == Just Left then
                getCharInTreeByCode first (List.drop 1 code)

            else
                getCharInTreeByCode second (List.drop 1 code)


compress : String -> ( Tree, List Code )
compress text =
    let
        tree =
            generateTree text

        codes =
            text
                |> String.toList
                |> List.map (\char -> Dict.get char (createCharCodeDictFromTree tree))
                |> Maybe.Extra.values
    in
    ( tree, codes )


generateTree : String -> Tree
generateTree text =
    let
        characterCounts =
            countChars text
                |> Dict.toList
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
            Tuple.second element

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
            ( Tuple.first element, currentCode ) :: listOfCodes

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
            Tuple.first (compress model.stringFromFile)

        listOfCodes =
            Tuple.second (compress model.stringFromFile)
    in
    div []
        [ input [ onInput NewInput ] []
        , button [ onClick ZipRequested ] [ text "Test" ]
        , div []
            [ h2 [] [ text "Das beinhaltet der Baum:" ]
            , div [] [ viewTree tree ]
            ]
        , div []
            [ h2 [] [ text "Dies ist der Text angegeben in den Schlüsseln der Bits / After compression:" ]
            , div [] (List.map (\code -> text (code ++ " ")) (List.map stringFromCode listOfCodes))
            ]
        , div []
            [ h2 [] [ text "Der Text nach der Wiederherstellung / After compression & decompression" ]
            , div [] [ text (decompress tree listOfCodes) ]
            ]
        , div []
            [ h2 [] [ text "Der original Text:" ]
            , div [] [ text model.stringFromFile ]
            ]
        , div []
            [ h2 [] [ text "Download-File" ]
            , div []
                [ if String.isEmpty model.stringFromFile then
                    text ""

                  else
                    let
                        downloadFile =
                            createDownloadFile model.stringFromFile model.stringFromCodeList
                    in
                    text (downloadFile |> Decode.decode (Decode.string (Bytes.width downloadFile)) |> Maybe.withDefault "")
                ]
            ]
        ]


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
                div [] [ text ("Das Element " ++ (Tuple.first element |> String.fromChar) ++ " ist " ++ (Tuple.second element |> String.fromInt) ++ "x vertreten und wird zur Bitfolge " ++ currentCode ++ " komprimiert") ]

            Empty ->
                div [] [ text "Dieser Baum ist leer." ]
        ]


type alias Model =
    { stringFromFile : String
    , stringFromCodeList : String
    , bytes : Maybe Bytes
    }


type Msg
    = NewInput String
    | FormInBytes Bytes
    | ZipRequested
    | ZipLoaded File


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput newInput ->
            ( { model | stringFromFile = newInput }, Cmd.none )

        ZipRequested ->
            ( model, Select.file [] ZipLoaded )

        ZipLoaded file ->
            ( model, Task.perform FormInBytes (File.toBytes file) )

        FormInBytes bytes ->
            ( let
                stringFromFile =
                    Decode.decode (Decode.string (Bytes.width bytes)) bytes |> Maybe.withDefault ""
              in
              { stringFromFile = stringFromFile
              , stringFromCodeList = Tuple.second (compress stringFromFile) |> List.map stringFromCode |> String.join "-"
              , bytes = Just bytes
              }
            , Cmd.none
            )


createDownloadFile : String -> String -> Bytes
createDownloadFile file codes =
    Encode.encode (Encode.string (file ++ "At this place starts the codes from the tree" ++ codes))


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stringFromFile = ""
      , stringFromCodeList = ""
      , bytes = Nothing
      }
    , Cmd.none
    )
