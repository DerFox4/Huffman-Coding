## Huffman-Coding

The Huffman-Coding is a smart way for compression and decompression.
It's used for lossless data compression.
The algorithm counts the frequency of chars to compress a String to the minimum of size.
To keep that promise, the algorithm allocates the Char with the highest frequency to the shortest possible sequence of bits to represent them.
The rarer a Char is in the String the longer is the sequence of bits.


## Example of a Huffman-Coding-Tree:

```
Codes will be created by the directions it
follows through the tree to get to the char
         |                           |
         |                           v
         |                        Right ->
         |    ==============Node==============
         |   ||    \                         ||
         |   ||     \                        ||
         |   ||    'a' will be represented   ||
         |   ||          by the Code:        ||
         |   ||     [Left, Right, Right]     ||
         v   ||    /  |                      ||
     <- Left vv   /   |                      vv
       =====Node==    |                   ==Node=====
      ||         ||   |                  ||         ||
      ||         ||   |                  ||         ||
      ||         ||   |                  ||         ||
      ||         ||   |                  ||         ||
      vv         vv   v                  vv         vv
{Leaf 'l' 2}  ===Node===             ===Node===  {Leaf 'y' 1}
             ||        ||           ||       ||
             ||        ||           ||       ||
             ||        ||           ||       ||
        ======         ||           ||        ======
       ||              ||           ||             ||
       vv              vv           vv             VV
{Leaf ('g') 1}  {Leaf 'a' 1}  {Leaf 'e' 1}  {Leaf 'r' (1)}
        ^                                              ^
        |                                              |
         ----- The information which Char is decoded   |
                                                       |
                                                       |
      The information how often this Char is used -----
```

### Possible Strings are:

```
-----------------------------   -----------------------------
| Char |        Code        |   | Char |        Code        |
|------|--------------------|   -----------------------------
|  g   | Left, Right, Left  |   |  a   | Left, Right, Right |
|  a   | Left, Right, Right |   |  l   |     Left, Left     |
|  l   |     Left, Left     |   |  l   |     Left, Left     |
|  l   |     Left, Left     |   |  e   | Right, Left, Left  |
|  e   | Right, Left, Left  |   |  r   | Right, Left, Right |
|  r   | Right, Left, Right |   |  g   | Left, Right, Left  |
|  y   |    Right, Right    |   |  y   |    Right, Right    |
-----------------------------   -----------------------------
```

This Example shows how important the list of Codes are.
This are only two possible Strings by the same Tree, but there are a lot more by the same tree.


## HuffmanFile:

A HuffmanFile contains a List Code and a Tree.
The List of Codes were created by compressing the string of a File.
The Tree will be created while compressing.

```
    createHuffmanFile : String -> HuffmanFile
    createHuffmanFile transformedFile =
        HuffmanCoding.compress transformedFile
            |> (\(tree, listCodes) ->
                    HuffmanFile
                        listCodes
                        tree
               )
```