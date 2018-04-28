# Zippy

[![Build Status](https://travis-ci.com/IITH-SBJoshi/haskell-13.svg?token=8atBSLxEouzs1ugsHMsx&branch=master)](https://travis-ci.com/IITH-SBJoshi/haskell-13)

Zippy is a small file compression utility built purely in Haskell. It provides for two different data compression techniques, namely, 
  - Huffman Encoding
  - Lempel–Ziv–Welch (LZW) Compression Algorithm

This project was built as a partial fullfilment towards CS2433-Principles of Programming Languages II (Spring'18) taught by Dr. Saurabh Joshi at IIT Hyderabad.

# Getting Started

### Running the application

Open a terminal window.

```sh
$ ./zippy <option> <input_file> <output_file>
```
### Command-line Arguments
Zippy requires three command-line arguments : <option>, <input_file> and <output_file>.

 - ```<option>```  can take the following values:
    - ```-c-huf``` for compression using Huffman encoding.
    - ```-c-lzw``` for compression using LZW compression algorithm.
    - ```-d-huf``` for decompression using Huffman encoding.
    - ```-d-lzw``` for decompression using LZW compression algorithm.

- ```<input_file>``` is the relative path of the file to be compressed.
- ```<output_file>``` is the relative path of the compressed/decompressed file.

### Building from source

Zippy requires [GHC](https://www.haskell.org/ghc) 8.0.2+ to build source.
Clone the repository.

```sh
$ cd haskell-13
$ chmod +x build.sh
$ ./build.sh
```
# Authors
- Pawar Abhishek Dwarkanath (cs16btech11027@iith.ac.in)
- Ninad Akolekar (cs16btech11024@iith.ac.in)
- Saahil Sirowa (cs16btech11030@iith.ac.in)

