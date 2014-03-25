WordNum
=======

This is my Haskell solution to a fun little problem a coworker showed me.

##### Usage

Load comb.hs and run the function `wordNum` on a word, or run `test` (no arguments) to make sure it works.

##### My TL;DR of the problem (original below)

Consider a word (ignore case). There are many possible rearrangements of the letters in that word. Put those possibilities in alphabetical order and number each one by its position in that alphabetical list. Write a program that takes a word and tells you that number. For example, the number for BAAA is 4 because we have

1. AAAB
2. AABA
3. ABAA
4. BAAA

##### My solution

Consider the n-letter word { x<sub>1</sub>, x<sub>2</sub>, ... , x<sub>n</sub> }. My solution is based on the idea that the word number will be the sum of two quantities:

1. The number of combinations starting with letters lower in the alphabet than x<sub>1</sub>, and
2. how far we are into the the arrangements that start with x<sub>1</sub>.

The trick is that the second quantity happens to be the word number of the word { x<sub>2</sub>, ... , x<sub>n</sub> }. This suggests a recursive implementation.

Getting the first quantity is a little complicated:

1. Let `uniqLowers` =  { u<sub>1</sub>, u<sub>2</sub>, ... , u<sub>m</sub> } = all the unique letters lower than x<sub>1</sub>
2. For each u<sub>j</sub>, count the number of permutations starting with u<sub>j</sub>.
3. Add all those up.

##### The original problem text

Consider a "word" as any sequence of capital letters A-Z (not limited to just "dictionary words"). For any word with at least two different letters, there are other words composed of the same letters but in a different order (for instance, stationarily/antiroyalist, which happen to both be dictionary words; for our purposes "aaiilnorstty" is also a "word" composed of the same letters as these two). We can then assign a number to every word, based on where it falls in an alphabetically sorted list of all words made up of the same set of letters. One way to do this would be to generate the entire list of words and find the desired one, but this would be slow if the word is long.

Write a program which takes a word as a command line argument and prints to standard output its number. Do not use the method above of generating the entire list. Your program should be able to accept any word 25 letters or less in length (possibly with some letters repeated), and should use no more than 1 GB of memory and take no more than 500 milliseconds to run. Any answer we check will fit in a 64-bit integer.

Sample words, with their rank:

ABAB = 2 <br>
AAAB = 1 <br>
BAAA = 4 <br>
QUESTION = 24572 <br>
BOOKKEEPER = 10743
