# Advent of Code 2023

A repository of emacs lisp solutions for the 2023 Advent of Code.
Although, if I see a quick solution using a Perl one-liner,
I may make a run for the leaderboard.

The initial library (aoc.el) was developed while warming up with the
puzzles from 2015.

## Day 8: Haunted Wasteland

A little math helped solve part 2.

## Day 7: Camel Cards

Sorting card hands for an elvish (elfish?) game.
Had to knock off early to get some sleep for an early meeting.
Although, slowing down might have helped me finish faster.

## Day 6: Wait For It

Wow. What a difference. This one was simple compared to previous days.

Although I got a correct answer to part 2, it took 84 seconds to find.
You can cut that to practically nothing with a little high school algebra.

## Day 5: If You Give A Seed A Fertilizer

Part 1 wasn't too bad, but figuring out how to slice ranges in part 2
was taking too long. Eventually I decided on a modified brute force
solution. Emacs Lisp just wasn't fast enough to try every possible
input, so I skipped through the input data then backtracked the answer.

## Day 4: Scratchcards

Reading scratch off cards and counting the winners.
This was another opportunity to use the
[PEG](https://elpa.gnu.org/packages/peg.html) library.
The set operations in the Common Lisp library also came in handy.

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
