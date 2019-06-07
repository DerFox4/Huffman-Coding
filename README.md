## Huffman-Coding

The Huffman-Coding is a smart way for compression and decompression.
It's used for lossless data compression.
For compression the algorithm counts the frequency of chars.
The higher the frequency of a char in the string the shorter is the sequence of bits which are used to represent the char.

## Example of a Huffman-Coding-Tree:
```
The Code will be created by the directions --------------
  you had to follow to get to the char          |        |
                                                v        |
                                              Right -->  |
               ---------------------Node-----------------|---
              |                              /           |   |
              |                             /            |   |
              |                            /             v   |
              v          i will get the code:        <- Left v
       -----Node-----      Right, Left, Left --->    -------Node-------
      |              |            |        \        |                  |
      |              |            |         \       |                  |
      v              v            |          \      v                  v
Leaf 's' 2    -----Node-----      |          -----Node-----      Leaf 'r' 2
             |              |     |         |              |
             |              |     |         |              |
             v              v     |         v              v
      Leaf ('u') 1    Leaf 'p' 1   -->Leaf 'i' 1     Leaf 'e' (1)
             ^                                                 ^
             |                                                 |
             |                                                 |
             |   How often the Char was used in the String ----
             |
             |
              ---- The char which is decoded
```

### One possible String is: surprise

```
                        -----------------------------
                        | Char |        Code        |
                        |------|--------------------|
                        |  s   |     Left, Left     |
                        |  u   | Left, Right, Left  |
                        |  r   |    Right, Right    |
                        |  p   | Left, Right, Right |
                        |  r   |    Right, Right    |
                        |  i   | Right, Left, Left  |
                        |  s   |     Left, Left     |
                        |  e   | Right, Left, Right |
                        -----------------------------
```

## Functions:

This package contains functions to compress and decompress a text.
Further it has some function to work with Huffman-Coding-Trees.
At least there are some function to encode and decode the types into sequences of bytes.