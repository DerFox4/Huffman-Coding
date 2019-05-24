module Main exposing (Code, Direction(..), Element, FileWithTree, Tree(..), compress, decodeCode, decodeElement, decodeFile, decodeText, decodeTree, decompress, encodeCode, encodeElement, encodeFile, encodeText, encodeTree)

import Bitwise
import Browser
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode exposing (Encoder)
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


createElementFromTuple : ( Char, Int ) -> Element
createElementFromTuple ( char, numberOf ) =
    { char = char
    , numberOf = numberOf
    }


generateTree : String -> Tree
generateTree text =
    let
        characterCounts =
            countChars text
                |> Dict.toList
                |> List.map createElementFromTuple
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


createOldFile : Maybe Bytes -> Maybe String
createOldFile file =
    case file of
        Just bytes ->
            bytes
                |> Decode.decode (Decode.string (Bytes.width bytes))
                |> Maybe.withDefault ""
                |> String.split "!!!At this place starts the codes from the tree!!!"
                |> List.head

        Nothing ->
            Nothing


createDownloadBytes : String -> String -> Bytes
createDownloadBytes file codes =
    Encode.encode (Encode.string (file ++ "!!!At this place starts the codes from the tree!!!" ++ codes))


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
                                        let
                                            downloadFileInBytes =
                                                createDownloadBytes model.stringFromFile (String.join "-" (List.map stringFromCode model.codeList))
                                        in
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
                        [ let
                            oldVersion =
                                createOldFile model.bytes
                          in
                          case oldVersion of
                            Just _ ->
                                div [] [ button [] [ text "Hier soll der Download der wiederhergestellten Datei möglich sein." ] ]

                            Nothing ->
                                div [] []
                        ]
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


decodeCode : Decoder Code
decodeCode =
    Decode.unsignedInt16 Bytes.BE
        |> Decode.andThen
            (\toDecodeValue ->
                let
                    conditions x =
                        (x > 4095)
                            && (x < 53248)
                            && ((x == 4096)
                                    || (x == 6144)
                                    || (x > 8191 && x < 12288 && modBy 1024 x == 0)
                                    || (x > 12287 && x < 16384 && modBy 512 x == 0)
                                    || (x > 16383 && x < 20480 && modBy 256 x == 0)
                                    || (x > 20489 && x < 24576 && modBy 128 x == 0)
                                    || (x > 24575 && x < 28672 && modBy 64 x == 0)
                                    || (x > 28671 && x < 32768 && modBy 32 x == 0)
                                    || (x > 32767 && x < 36864 && modBy 16 x == 0)
                                    || (x > 36863 && x < 40960 && modBy 8 x == 0)
                                    || (x > 40959 && x < 45056 && modBy 4 x == 0)
                                    || (x > 45055 && x < 49152 && modBy 2 x == 0)
                                    || (x > 49151 && x < 53248)
                               )
                in
                if conditions toDecodeValue then
                    toDecodeValue
                        |> calculateCodeFromContentValue
                        |> (\code ->
                                if List.isEmpty code then
                                    [ Left ]

                                else
                                    code
                           )
                        |> Decode.succeed

                else
                    Decode.fail
            )


calculateCodeFromContentValue : Int -> Code
calculateCodeFromContentValue toDecodeValue =
    let
        content =
            modBy 4096 toDecodeValue

        prefix =
            toDecodeValue // 4096
    in
    calculateCodeFromContentValueHelp content 11 []
        |> List.take prefix


calculateCodeFromContentValueHelp : Int -> Int -> Code -> Code
calculateCodeFromContentValueHelp content rep code =
    if content == 0 then
        if rep < 0 then
            List.reverse code

        else
            List.reverse code ++ List.repeat (rep + 1) Left

    else if content - (2 ^ rep) >= 0 then
        calculateCodeFromContentValueHelp (content - (2 ^ rep)) (rep - 1) (Right :: code)

    else
        calculateCodeFromContentValueHelp content (rep - 1) (Left :: code)


encodeCode : Code -> Encoder
encodeCode code =
    code
        |> calculateCodeValueContent
        |> calculateByteDescriptionInInt (List.length code)
        |> Encode.unsignedInt16 Bytes.BE


calculateByteDescriptionInInt : Int -> Int -> Int
calculateByteDescriptionInInt prefix content =
    Bitwise.shiftLeftBy 12 prefix + content


calculateCodeValueContent : Code -> Int
calculateCodeValueContent code =
    List.foldl
        (\direction acc ->
            case direction of
                Left ->
                    acc * 2

                Right ->
                    acc * 2 + 1
        )
        0
        code
        |> Bitwise.shiftLeftBy (12 - List.length code)


encodeTree : Tree -> Maybe Encoder
encodeTree tree =
    case tree of
        Empty ->
            Nothing

        Node first second ->
            Maybe.map2 (\x y -> Encode.sequence [ Encode.unsignedInt8 0, x, y ]) (encodeTree first) (encodeTree second)

        Leaf element ->
            Just (Encode.sequence [ Encode.unsignedInt8 1, encodeElement element ])


encodeElement : Element -> Encoder
encodeElement element =
    let
        charAsString =
            String.fromChar element.char
    in
    Encode.sequence
        [ Encode.unsignedInt8 (Encode.getStringWidth charAsString)
        , Encode.string charAsString
        , Encode.unsignedInt16 Bytes.BE element.numberOf
        ]


decodeElement : Decoder Element
decodeElement =
    Decode.map2 Element
        (Decode.unsignedInt8 |> Decode.andThen decodeChar)
        (Decode.unsignedInt16 Bytes.BE)


decodeChar : Int -> Decoder Char
decodeChar width =
    Decode.string width |> Decode.andThen decodeStringToChar


decodeStringToChar : String -> Decoder Char
decodeStringToChar string =
    case changeOneCharStringIntoChar string of
        Nothing ->
            Decode.fail

        Just char ->
            Decode.succeed char


changeOneCharStringIntoChar : String -> Maybe Char
changeOneCharStringIntoChar string =
    if String.length string /= 1 then
        Nothing

    else
        String.toList string |> List.head


decodeTree : Decoder Tree
decodeTree =
    Decode.unsignedInt8
        |> Decode.andThen
            (\int ->
                if int == 0 then
                    Decode.map2 Node decodeTree decodeTree

                else if int == 1 then
                    Decode.map Leaf decodeElement

                else
                    Decode.fail
            )


encodeText : Int -> List Code -> Encoder
encodeText length text =
    Encode.sequence
        (Encode.unsignedInt32 Bytes.BE length :: List.map encodeCode text)


decodeText : Decoder Code -> Decoder (List Code)
decodeText decoder =
    Decode.unsignedInt32 Bytes.BE
        |> Decode.andThen (\len -> Decode.loop ( len, [] ) (current decoder))


current : Decoder Code -> ( Int, List Code ) -> Decoder (Step ( Int, List Code ) (List Code))
current decoder ( step, list ) =
    if step <= 0 then
        Decode.succeed (Done list)

    else
        Decode.map (\code -> Loop ( step - 1, list ++ List.singleton code )) decoder


type alias FileWithTree =
    { text : List Code
    , tree : Tree
    }


encodeFile : FileWithTree -> Maybe Encoder
encodeFile file =
    case encodeTree file.tree of
        Nothing ->
            Nothing

        Just thisTree ->
            Just (Encode.sequence [ encodeText (List.length file.text) file.text, thisTree ])


decodeFile : Decoder FileWithTree
decodeFile =
    Decode.map2 FileWithTree
        (decodeText decodeCode)
        decodeTree
