# Advent of Code

A repository of emacs lisp solutions for the 2023 Advent of Code.
Although, if I see a quick solution using a Perl one-liner,
I may make a run for the leaderboard.

The initial library (aoc.el) was developed while solving the puzzles from 2015.

## Day 3: Gear Ratios

The grid class from the 2015 warm up exercises came in handy today;
although, I added method to copy the grid.
Also took advantage of dynamic scoping. (So dirty.)
Still slow for all that.

Turns out the grid copy wasn't needed, and it's a lot faster without it.

## Day 2: Cube Conundrum

Aarrrg. Read the instructions thoroughly. I counted valid games in part 1 instead
of adding their IDs--and it's even in bold!

## Day 1: Trebuchet?!

A missing pipe in the regular expression slowed me down in part 2.
The test cases helped find the problem, but it still took a while.
