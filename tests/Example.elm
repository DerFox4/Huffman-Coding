module Example exposing (suite)

import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Decoder exposing (decodeCode, decodeElement, decodeFile, decodeText, decodeTree)
import Encoder exposing (encodeCode, encodeElement, encodeFile, encodeText, encodeTree)
import Expect exposing (Expectation)
import Main exposing (Code, Direction(..), Element, FileWithTree, Tree(..))
import Test exposing (..)


suite : Test
suite =
    describe "Main"
        [ describe "Code"
            [ describe "EncodingCode"
                [ test "Just Left" <|
                    \() ->
                        Encode.encode (encodeCode [ Left ])
                            |> Expect.equal (Encode.encode (Encode.unsignedInt16 Bytes.BE 4096))
                , test "Just Right" <|
                    \() ->
                        Encode.encode (encodeCode [ Right ])
                            |> Expect.equal (Encode.encode (Encode.unsignedInt16 Bytes.BE 6144))
                , test "Right Left" <|
                    \() ->
                        Encode.encode (encodeCode [ Right, Left ])
                            |> Expect.equal (Encode.encode (Encode.unsignedInt16 Bytes.BE 10240))
                , test "Left Right" <|
                    \() ->
                        Encode.encode (encodeCode [ Left, Right ])
                            |> Expect.equal (Encode.encode (Encode.unsignedInt16 Bytes.BE 9216))
                , test "Left, Left, Left, Right, Right, Left, Right, Right" <|
                    \() ->
                        Encode.encode (encodeCode [ Left, Left, Left, Right, Right, Left, Right, Right ])
                            |> Expect.equal (Encode.encode (Encode.unsignedInt16 Bytes.BE 33200))
                , test "Right, Right, Left, Right, Left, Left, Right, Left, Left, Left, Left, Left" <|
                    \() ->
                        Encode.encode (encodeCode [ Right, Right, Left, Right, Left, Left, Right, Left, Left, Left, Left, Left ])
                            |> Expect.equal (Encode.encode (Encode.unsignedInt16 Bytes.BE 52512))
                ]
            , describe "DecodingCode"
                [ test "Fill list with one Left-Element" <|
                    \() ->
                        Decode.decode decodeCode (Encode.encode (Encode.unsignedInt16 Bytes.BE 4096))
                            |> Expect.equal (Just [ Left ])
                , test "Just Right" <|
                    \() ->
                        Decode.decode decodeCode (Encode.encode (Encode.unsignedInt16 Bytes.BE 6144))
                            |> Expect.equal (Just [ Right ])
                , test "Right Left" <|
                    \() ->
                        Decode.decode decodeCode (Encode.encode (Encode.unsignedInt16 Bytes.BE 10240))
                            |> Expect.equal (Just [ Right, Left ])
                , test "Left, Right" <|
                    \() ->
                        Decode.decode decodeCode (Encode.encode (Encode.unsignedInt16 Bytes.BE 9216))
                            |> Expect.equal (Just [ Left, Right ])
                , test "Starts with 0" <|
                    \() ->
                        Decode.decode decodeCode (Encode.encode (Encode.unsignedInt16 Bytes.BE 33200))
                            |> Expect.equal (Just [ Left, Left, Left, Right, Right, Left, Right, Right ])
                , test "Fill list if content is 0" <|
                    \() ->
                        Decode.decode decodeCode (Encode.encode (Encode.unsignedInt16 Bytes.BE 52512))
                            |> Expect.equal (Just [ Right, Right, Left, Right, Left, Left, Right, Left, Left, Left, Left, Left ])
                , test "Decoder Fail" <|
                    \() ->
                        Decode.decode decodeCode (Encode.encode (Encode.unsignedInt16 Bytes.BE 42941))
                            |> Expect.equal Nothing
                ]
            ]
        , describe "Element"
            [ describe "De & Encoding"
                [ test "EmptyElement" <|
                    \() ->
                        Decode.decode decodeElement (Encode.encode (encodeElement (Element 'f' 0)))
                            |> Expect.equal (Just (Element 'f' 0))
                , test "Element 'a' 17" <|
                    \() ->
                        Decode.decode decodeElement (Encode.encode (encodeElement (Element 'a' 17)))
                            |> Expect.equal (Just (Element 'a' 17))
                , test "Element '€' 87" <|
                    \() ->
                        Decode.decode decodeElement (Encode.encode (encodeElement (Element '€' 87)))
                            |> Expect.equal (Just (Element '€' 87))
                ]
            ]
        , describe "Tree"
            [ describe "De & Encoding"
                [ test "Node Leaf Leaf" <|
                    \() ->
                        let
                            treeEncoder =
                                case encodeTree (Node (Leaf (Element 'a' 2)) (Leaf (Element 'b' 4))) of
                                    Nothing ->
                                        Encode.unsignedInt8 0

                                    Just tree ->
                                        tree
                        in
                        Decode.decode decodeTree (Encode.encode treeEncoder)
                            |> Expect.equal (Just (Node (Leaf (Element 'a' 2)) (Leaf (Element 'b' 4))))
                , test """
                                        Node
                 Node                                         Node
        Leaf            Leaf                       Node                 Node
                                            Leaf        Leaf      Leaf        Leaf
            """ <|
                    \() ->
                        let
                            treeEncoder =
                                case encodeTree (Node (Node (Leaf (Element 'a' 12)) (Leaf (Element 'b' 16))) (Node (Node (Leaf (Element 'f' 25)) (Leaf (Element 'g' 12))) (Node (Leaf (Element 'w' 13)) (Leaf (Element 'u' 8))))) of
                                    Nothing ->
                                        Encode.unsignedInt8 0

                                    Just tree ->
                                        tree
                        in
                        Decode.decode decodeTree (Encode.encode treeEncoder)
                            |> Expect.equal (Just (Node (Node (Leaf (Element 'a' 12)) (Leaf (Element 'b' 16))) (Node (Node (Leaf (Element 'f' 25)) (Leaf (Element 'g' 12))) (Node (Leaf (Element 'w' 13)) (Leaf (Element 'u' 8))))))
                ]
            ]
        , describe "De & Encode Text"
            [ test "short Codes" <|
                \() ->
                    Decode.decode (decodeText decodeCode) (Encode.encode (encodeText 4 [ [ Left ], [ Right ], [ Left, Left ], [ Right, Right ] ]))
                        |> Expect.equal (Just [ [ Left ], [ Right ], [ Left, Left ], [ Right, Right ] ])
            , test "long Codes" <|
                \() ->
                    Decode.decode (decodeText decodeCode) (Encode.encode (encodeText 3 [ [ Left, Left, Right, Right, Left, Left, Right, Left, Right ], [ Left, Right, Right, Right, Right, Left, Left, Right, Right ], [ Right, Right, Left, Right, Right, Left, Right, Right, Left ] ]))
                        |> Expect.equal (Just [ [ Left, Left, Right, Right, Left, Left, Right, Left, Right ], [ Left, Right, Right, Right, Right, Left, Left, Right, Right ], [ Right, Right, Left, Right, Right, Left, Right, Right, Left ] ])
            ]
        , describe "File"
            [ test "kurz" <|
                \() ->
                    let
                        fileEncoder =
                            case encodeFile (FileWithTree [ [ Left ], [ Right ], [ Left, Left ], [ Right, Right ] ] (Node (Leaf (Element 'a' 2)) (Leaf (Element 'b' 4)))) of
                                Nothing ->
                                    Encode.unsignedInt8 0

                                Just thisEncoder ->
                                    thisEncoder
                    in
                    Decode.decode decodeFile (Encode.encode fileEncoder)
                        |> Expect.equal (Just (FileWithTree [ [ Left ], [ Right ], [ Left, Left ], [ Right, Right ] ] (Node (Leaf (Element 'a' 2)) (Leaf (Element 'b' 4)))))
            ]
        ]
