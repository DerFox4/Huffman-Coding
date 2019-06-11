module HuffmanCoding exposing
    ( Tree(..), Element, Code, Direction(..), HuffmanFile
    , compress
    , decompress
    , encodeCode, encodeTree, encodeFile
    , decodeCode, decodeTree, decodeFile
    , createCharCodeDictFromTree, getCharFromTreeByCode, generateTree
    )

{-|


# Types

@docs Tree, Element, Code, Direction, HuffmanFile


# Compression

@docs compress


# Decompression

@docs decompress


# Encoder

@docs encodeCode, encodeTree, encodeFile


# Decoder

@docs decodeCode, decodeTree, decodeFile


# Tree Helper-Functions

@docs createCharCodeDictFromTree, getCharFromTreeByCode, generateTree

-}

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)
import Maybe.Extra


{-| A Huffman-Coding-Tree has three possible states:

1.  Empty -> This tree is empty.
2.  Node -> A Node has always two further Trees.
3.  Leaf -> A Leaf describe one of the endpoints of a Tree.

Example:

    exampleTree : Tree
    exampleTree =
        Node (Leaf Element 'h' 1 (Leaf Element 'i' 1))

-}
type Tree
    = Empty
    | Node Tree Tree
    | Leaf Element


{-| An Element contains the information which char is encoded and how often this is used in the text.
-}
type alias Element =
    { char : Char
    , numberOf : Int
    }


{-| Describe which tree in a Node has to chose.
-}
type Direction
    = Left
    | Right


{-| A list of Direction. It describe the route through a tree until a Leaf.
-}
type alias Code =
    List Direction


{-| The HuffmanFile is a combination of the text in Codes and the Huffman-Coding-Tree which was used to create the codes.
-}
type alias HuffmanFile =
    { text : List Code
    , tree : Tree
    }


{-| Transform a list of Codes by means of a Huffman-Coding-Tree into a String.

    decompress
        (Node (Node (Leaf Element 'n' 1) (Leaf Element 'i' 1)) (Node (Leaf Element 'c' 1) (Leaf Element 'e' 1)))
        [ [ Left, Left ], [ Left, Right ], [ Right, Left ], [ Right, Right ] ]
        == "nice"

-}
decompress : Tree -> List Code -> String
decompress tree textInCodes =
    textInCodes
        |> List.map (getCharFromTreeByCode tree)
        |> Maybe.Extra.values
        |> String.fromList


{-| Returns a Maybe Char by using the Direction in Codes.
It returns a `Just Char` if the route of the Directions ends in a leaf otherwise it will return `Nothing`.

    getCharFromTreeByCode
        ( Node (Node (Leaf (Element 'd' 1)) (Leaf (Element 'd' 1)) (Leaf (Element 'o' 2)) )
        [ Right ]
        == Just 'o'


    getCharFromTreeByCode
        ( Node (Node (Leaf (Element 'e' 1)) (Leaf (Element 'i' 1)) Node (Leaf (Element 'n' 1)) (Leaf (Element 's' 1)) ) )
        [Left]
        == Nothing


    getCharFromTreeByCode
        ( Node (Leaf (Element 'h' 1)) (Leaf (Element 'i' 1)) )
        [Right, Left, Right, Right]
        == Nothing

-}
getCharFromTreeByCode : Tree -> Code -> Maybe Char
getCharFromTreeByCode tree code =
    case tree of
        Empty ->
            Nothing

        Leaf element ->
            if List.isEmpty code then
                Just element.char

            else
                Nothing

        Node first second ->
            if List.isEmpty code then
                Nothing

            else if List.head code == Just Left then
                getCharFromTreeByCode first (List.drop 1 code)

            else
                getCharFromTreeByCode second (List.drop 1 code)


{-| Returns a Tuple of a Huffman-Coding-Tree and List of Codes.
Each Code describe a Char in the String. Each Code is a route description for a Char in the Tree.

    compress "hi"
        == ( Node (Leaf Element 'h' 1) (Leaf Element 'i' 1)
           , [ [ Left ], [ Right ] ]
           )

-}
compress : String -> ( Tree, List Code )
compress text =
    let
        tree =
            generateTree text

        codes =
            createListOfCodes text tree
    in
    ( tree, codes )


{-| Generates a Huffman-Coding-Tree for a String.

    generateTree "done"
        == Node
            (Node
                (Leaf Element 'd' 1)
                (Leaf Element 'o' 1)
            )
            (Node
                (Leaf Element 'n' 1)
                (Leaf Element 'e' 1)
            )

-}
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


{-| Return all Chars and their Code in a Dict for a Huffman-Coding-Tree.

    createCharCodeDictFromTree (Node (Node (Leaf Element 'b' 1) (Leaf Element 'l' 1)) (Node (Leaf Element 'u' 1) (Leaf Element 'e' 1)))
        == Dict.fromList
            [ ( 'b', [ Left, Left ] )
            , ( 'l', [ Left, Right ] )
            , ( 'u', [ Right, Left ] )
            , ( 'e', [ Right, Right ] )
            ]

-}
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



--------------------- DECODER ---------------------


{-| Decode a byte into a Node or a Leaf.

    Bytes.Decode.decode decodeTree
        |`00`| |`00`| |`00`| |`01`| |`01`| |`65`| |`00` `01`| |`01`| |`01`| |`6F`| |`00` `01`| |`01`| |`01`| |`68`| |`00` `01`| |`01`| |`01`| |`6C`| |`00` `02`|
        == (Node ((Node (Node (Leaf Element 'e' 1) (Leaf Element 'o' 1))(Leaf Element 'h' 1)) (Leaf Element 'l' 2)))

-}
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


{-| Decode two bytes into a Code.

    Bytes.Decode.decode decodeCode `5C` `80`
        == [ Right, Right, Left, Left, Right, Left ]


    Bytes.Decode.decode decodeCode `20` `00` `24` `00` `28` `00` `2C` `00`
        == [Left, Left] [Left, Right] [Right, Left] [Right, Right]

-}
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


{-| Decodes a sequence of bytes into a HuffmanFile.

    Bytes.Decode.decode
        decodeFile
        | `00` `00` `00` `04` | `20` `00` - `28` `00` -  `24` `00` - `2C` `00` | `00` `00` `01` `01` `62` `00` `01` `01` `01` `6C` `00` `01` `00` `01` `01` `75` `00` `01` `01` `01` `65` `00` `01` |
        |   Number Of Chars   |                 Codes in Bytes                 |                                                   Tree in Bytes                                                    |

    == HuffmanFile
        [ [Left, Left], [Left, Right], [Right, Left], [Right, Right] ]
        (Node (Node (Leaf Element 'b' 1) (Leaf Element 'l' 1)) (Node (Leaf Element 'u' 1) (Leaf Element 'e' 1)))

-}
decodeFile : Decoder HuffmanFile
decodeFile =
    Decode.map2 HuffmanFile
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



--------------------- ENCODER ---------------------


{-| Create a Encoder to encode a Code. Each Code will use 2 bytes after encoding.

    Bytes.Encoder.encode encodeCode [Left]
        == Hex : `10` `00`
        || Dual: `0001` `0000` `0000` `0000`


    Bytes.Encoder.encode encodeCode [Left, Right, Right, Left, Left, Right, Right]
        == Hex : `76` `60`
        || Dual: `0111` `0110` `0110` `0000`

-}
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


{-| Create a Maybe-Encoder of a tree.
If the Tree does not include any Empty-state the result will be Just Encoder Else Nothing.

    Bytes.Encode.encode encodeTree (Node Empty (Node (Leaf Element 'e' 4) (Leaf Element 'h' 4)))
        == Nothing


    Bytes.Encode.encode encodeTree (Node (Leaf Element '1' 1) (Leaf Element '€' 1))
        == Hex : |`00`| |`01`| |`01` | |`31` | |`00` `01`| |`01`| |`03` | |`E2` `82` `AC`| |`00` `01`|
                 |Node| |Leaf| |Width| |UTF-8| |Number of| |Leaf| |Width| |     UTF-8    | |Number of|


    Bytes.Encode.encode encodeTree (Node (Node (Leaf Element 'b' 1) (Leaf Element 'l' 1)) (Node (Leaf Element 'u' 1) (Leaf Element 'e' 1)))
        == Hex : |`00`| |`00`| |`01`| |`01` | |`62` | |`00` `01`| |`01`| |`01` | |`6C` | |`00` `01`| |`00`| |`01`| |`01` | |`75` | |`00` `01`| |`01`| |`01` | |`6A` | |`00` `01`|
                 |Node| |Node| |Leaf| |Width| |UTF-8| |Number of| |Leaf| |Width| |UTF-8| |Number of| |Node| |Leaf| |Width| |UTF-8| |Number of| |Leaf| |Width| |UTF-8| |Number of|

-}
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


{-| If the Tree in the HuffmanFile is able to encode correctly it returns a Just Encoder Else Nothing.

    Bytes.Encode.encode encodeFile
        HuffmanFile
            [ [ Left, Left ], [ Left, Right ], [ Right, Left ], [ Right, Right ], [ Left, Left ], [ Left, Right ], [ Right, Left ], [ Right, Right ] ]
            (Node (Node (Leaf Element 'b' 2) (Leaf Element 'l' 2)) (Node (Leaf Element 'u' 2) (Leaf Element 'e' 2)))

            == |`00` `00` `00` `08| [ look up @encodeCode ] [ look up @encodeTree ]
               |  Number of Codes | [    List of Codes    ] [         Tree        ]


    Bytes.Encode.encode encodeFile
        HuffmanFile
            [ [ Left, Right, Left ], [Right, Right], [ Left, Left ] ]
            (Node (Leaf Element 'a' 1) (Empty))

            == Nothing

-}
encodeFile : HuffmanFile -> Maybe Encoder
encodeFile huffmanFile =
    case encodeTree huffmanFile.tree of
        Nothing ->
            Nothing

        Just encodedTree ->
            Just (Encode.sequence [ encodeText (List.length huffmanFile.text) huffmanFile.text, encodedTree ])


encodeText : Int -> List Code -> Encoder
encodeText length text =
    Encode.sequence
        (Encode.unsignedInt32 Bytes.BE length :: List.map encodeCode text)


createListOfCodes : String -> Tree -> List Code
createListOfCodes text tree =
    let
        dictFromTree =
            createCharCodeDictFromTree tree
    in
    text
        |> String.toList
        |> List.map
            (\char ->
                Dict.get char dictFromTree |> Maybe.withDefault []
            )
