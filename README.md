# Huffman-Coding

## About Huffman-Coding

The Huffman-Coding is a smart way for compression and decompression.
Its used for lossless data compression.
For compression the algorithm counts the frequency of chars.
The higher the frequency of a char in the string the shorter is the sequence of bits.

## About this project

### How to start this project

If you want to use this project you first have to clone this repository.
After cloning this repository you could start the Huffman-Codding with the command `elm reactor` in your terminal.

### Description

This project is a implementation of the Huffman-Coding in Elm.
The User can write a `string` into the input field.
This project compress this string and views the used `tree` and `list code`.
Afterwards it transforms the codes back into the original `string`.

## About the functions

#### Compress
Compress a string into a list of bit sequences.
Additionally the function compress will return the tree which was used to compress the string.

#### Decompress
Decompress creates the original string from a list of codes and the associated tree.