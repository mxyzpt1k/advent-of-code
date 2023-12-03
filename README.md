# Advent of Code 2023

A repository of emacs lisp solutions for the 2023 Advent of Code.
Although, if I see a quick solution using a Perl one-liner,
I may make a run for the leaderboard.

The initial library (aoc.el) was developed while solving the puzzles from 2015.

## Day 3: Gear Ratios

The grid class from the 2015 warm up exercises came in handy today.
At one point, I spent some time adding a method to copy grids that turned
out to be unnecessary. 
Also took advantage of dynamic scoping. (So dirty.)

## Day 2: Cube Conundrum

Aarrrg. Read the instructions thoroughly. I counted valid games in part 1 instead
of adding their IDs--and it's even in bold!

Parsing this sort of structured input happens a lot, so after the initial
submission, I went looking for a better method and found
[Parsing Expression Grammars in Emacs Lisp](https://elpa.gnu.org/packages/peg.html)
or PEG. 

## Day 1: Trebuchet?!

A missing pipe in the regular expression slowed me down in part 2.
The test cases helped find the problem, but it still took a while.
