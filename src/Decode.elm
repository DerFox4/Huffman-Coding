module Decoder exposing (decodeChar, decodeCode, decodeElement, decodeFile, decodeStringToChar, decodeText, decodeTree)

import Bytes
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Main exposing (Code(..), Direction(..), Element, FileWithTree, Tree(..))


decodeFile : Decoder FileWithTree
decodeFile =
    Decode.map2 FileWithTree
        (decodeText decodeCode)
        decodeTree


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
