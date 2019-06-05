module Encoder exposing (encodeCode, encodeElement, encodeFile, encodeText, encodeTree)

import Bitwise
import Bytes
import Bytes.Encode as Encode exposing (Encoder)
import UsedTypes exposing (Code, Direction(..), Element, FileWithTree, Tree(..))


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


encodeText : Int -> List Code -> Encoder
encodeText length text =
    Encode.sequence
        (Encode.unsignedInt32 Bytes.BE length :: List.map encodeCode text)


encodeFile : FileWithTree -> Maybe Encoder
encodeFile file =
    case encodeTree file.tree of
        Nothing ->
            Nothing

        Just thisTree ->
            Just (Encode.sequence [ encodeText (List.length file.text) file.text, thisTree ])
