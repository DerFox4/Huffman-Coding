module Main exposing (main)

import Browser
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, h1, h2, h3, input, p, pre, text)
import Html.Events exposing (onClick, onInput)
import HuffmanCoding exposing (Code, Direction(..), HuffmanFile, Tree(..))
import Task


type Task
    = Compression
    | Decompression


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
        stringFromFile =
            case model.stringFromFile of
                Err _ ->
                    "Tut uns leid, etwas ist schiefgelaufen"

                Ok string ->
                    string
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
        , case model.task of
            Just Compression ->
                div []
                    [ h2 [] [ text "Datei Kompremierung:" ]
                    , if String.isEmpty stringFromFile then
                        div []
                            [ div [] [ text "Bitte füllen Sie dieses Textfeld aus, oder drücken sie auf den Button um eine Datei zu komprimieren" ]
                            , input [ onInput NewInput ] []
                            , button [ onClick (FileRequested Compression) ] [ text "Wählen sie eine Datei zur Komprimierung aus" ]
                            ]

                      else
                        div []
                            [ h3 [] [ text "Der Inhalt ihrer aktuell ausgewählten Datei ist:" ]
                            , pre [] [ text stringFromFile ]
                            ]
                    , if String.isEmpty stringFromFile then
                        div [] []

                      else
                        div []
                            [ div []
                                [ h2 [] [ text "Download-File" ]
                                , div []
                                    [ if String.isEmpty stringFromFile then
                                        text ""

                                      else
                                        let
                                            bytes =
                                                stringFromFile
                                                    |> createFileWithTree
                                                    |> getBytesOfFileWithTree
                                        in
                                        div []
                                            [ div []
                                                [ text "Bitte geben Sie im Input-Feld einen Namen an unter dem Sie die komprimierte Datei speichern wollen:"
                                                , div []
                                                    [ input [ onInput UpdateFileName ] []
                                                    , div []
                                                        [ button [ onClick (FileRequested Compression) ] [ text "Eine andere Datei wählen" ]
                                                        ]
                                                    ]
                                                ]
                                            , if String.isEmpty model.downloadFileName then
                                                div [] []

                                              else
                                                div []
                                                    [ h3 [] [ text "Download abschließen:" ]
                                                    , button [ onClick (DownloadFileFromBytes model.downloadFileName bytes) ] [ text "Starten Sie den Download der Datei hier" ]
                                                    ]
                                            , div []
                                                [ h2 [] [ text "Wollen Sie Informationen zur Kompression erhalten?" ]
                                                , div []
                                                    [ button [ onClick ChangeFurtherInformation ]
                                                        [ if model.furtherInformation then
                                                            text "Weniger Infos"

                                                          else
                                                            text "Mehr Infos"
                                                        ]
                                                    ]
                                                ]
                                            , if model.furtherInformation then
                                                let
                                                    tree =
                                                        HuffmanCoding.generateTree stringFromFile
                                                in
                                                div []
                                                    [ div []
                                                        [ h1 [] [ text "Infos zur Komprimierung:" ]
                                                        , h2 [] [ text "Das beinhaltet der Baum:" ]
                                                        , div [] [ viewTree tree ]
                                                        ]
                                                    , div []
                                                        [ h2 [] [ text "Der Text/Die Datei mit den neuen Bits / After compression:" ]
                                                        , div [] (List.map (\code -> text (code ++ " ")) (List.map stringFromCode (Tuple.second (HuffmanCoding.compress stringFromFile))))
                                                        ]
                                                    ]

                                              else
                                                div [] []
                                            ]
                                    ]
                                ]
                            ]
                    ]

            Just Decompression ->
                div []
                    [ h1 [] [ text "Datei Dekomprimierung: " ]
                    , div []
                        [ p []
                            [ text "Falls Sie eine Datei dekomprimieren wollen, dann klicken Sie auf den Button und wählen eine Datei aus." ]
                        ]
                    , div [] [ button [ onClick (FileRequested Decompression) ] [ text "Bitte wählen Sie eine Datei zur Dekomprimierung aus" ] ]
                    , if String.isEmpty stringFromFile then
                        div [] []

                      else
                        div []
                            [ div []
                                [ h2 [] [ text "Download:" ]
                                , case model.stringFromFile of
                                    Err _ ->
                                        div [] [ text "Es tut uns leid, aber diese Datei kann nicht von uns dekomprimiert werden!" ]

                                    Ok string ->
                                        div []
                                            [ p []
                                                [ text "Bitte geben Sie den Namen für die wiederhergestellte Datei ein: "
                                                , input [ onInput UpdateFileName ] []
                                                ]
                                            , if String.isEmpty model.downloadFileName then
                                                div [] []

                                              else
                                                div [] [ button [ onClick (DownloadFileFromString model.downloadFileName stringFromFile) ] [ text "Für den Download ihrer ursprünglichen Datei klicken Sie hier" ] ]
                                            , div []
                                                [ h2 [] [ text "Wollen Sie eine Vorschau zur Datei haben?" ]
                                                , div []
                                                    [ button [ onClick ChangeFurtherInformation ]
                                                        [ if model.furtherInformation then
                                                            text "Vorschau schließen"

                                                          else
                                                            text "Klicken Sie hier"
                                                        ]
                                                    ]
                                                ]
                                            , if model.furtherInformation then
                                                div []
                                                    [ h2 [] [ text "Vorschau zur wiederhergestellten Datei" ]
                                                    , div []
                                                        [ pre [] [ text string ] ]
                                                    ]

                                              else
                                                div [] []
                                            ]
                                ]
                            ]
                    ]

            Nothing ->
                div [] []
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


createFileWithTree : String -> HuffmanFile
createFileWithTree string =
    string
        |> HuffmanCoding.compress
        |> (\( tree, codes ) -> HuffmanFile codes tree)


getBytesOfFileWithTree : HuffmanFile -> Bytes
getBytesOfFileWithTree file =
    let
        maybeEncoder =
            file
                |> HuffmanCoding.encodeFile

        encoder =
            case maybeEncoder of
                Nothing ->
                    Encode.unsignedInt8 0

                Just thisEncoder ->
                    thisEncoder
    in
    Encode.encode encoder


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
    { task : Maybe Task
    , stringFromFile : Result String String
    , codeList : List Code
    , bytes : Maybe Bytes
    , downloadFileName : String
    , furtherInformation : Bool
    }


type Msg
    = NewInput String
    | FileRequested Task
    | FileLoaded Task File
    | TransformFileIntoBytes Bytes
    | ChangeState Task
    | DownloadFileFromBytes String Bytes
    | DownloadFileFromString String String
    | TransformCodedFileIntoFile Bytes
    | UpdateFileName String
    | ChangeFurtherInformation


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewInput newInput ->
            ( { model | stringFromFile = Ok newInput }, Cmd.none )

        ChangeState newState ->
            ( { model
                | task = Just newState
                , stringFromFile = Ok ""
                , codeList = []
                , bytes = Nothing
                , downloadFileName = ""
                , furtherInformation = False
              }
            , Cmd.none
            )

        FileRequested state ->
            ( model, Select.file [] (FileLoaded state) )

        FileLoaded state file ->
            ( model
            , case state of
                Compression ->
                    Task.perform TransformFileIntoBytes (File.toBytes file)

                Decompression ->
                    Task.perform TransformCodedFileIntoFile (File.toBytes file)
            )

        TransformFileIntoBytes bytes ->
            ( let
                stringFromFile =
                    case Decode.decode (Decode.string (Bytes.width bytes)) bytes of
                        Nothing ->
                            Err ""

                        Just string ->
                            Ok string

                toCompress =
                    case model.stringFromFile of
                        Err _ ->
                            ""

                        Ok string ->
                            string
              in
              { model
                | stringFromFile = stringFromFile
                , codeList = Tuple.second (HuffmanCoding.compress toCompress)
                , bytes = Just bytes
              }
            , Cmd.none
            )

        TransformCodedFileIntoFile bytes ->
            ( { model
                | stringFromFile =
                    let
                        string =
                            case Decode.decode HuffmanCoding.decodeFile bytes of
                                Nothing ->
                                    Err "Tut uns Leid, aber diese Datei konnte nicht Dekodiert werden!"

                                Just fileWithTree ->
                                    Ok (HuffmanCoding.decompress fileWithTree.tree fileWithTree.text)
                    in
                    string
              }
            , Cmd.none
            )

        DownloadFileFromBytes name file ->
            ( model, Download.bytes (name ++ ".txt") "text/markdown" file )

        DownloadFileFromString name string ->
            ( model, Download.string (name ++ ".txt") "test/markdown" string )

        UpdateFileName newName ->
            ( { model | downloadFileName = newName }, Cmd.none )

        ChangeFurtherInformation ->
            ( { model | furtherInformation = not model.furtherInformation }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { task = Nothing
      , stringFromFile = Err ""
      , codeList = []
      , bytes = Nothing
      , downloadFileName = ""
      , furtherInformation = False
      }
    , Cmd.none
    )
