module UsedTypes exposing (Code, Direction(..), Element, FileWithTree, Tree(..))


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
