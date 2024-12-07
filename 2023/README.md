# Advent of Code 2023

A repository of emacs lisp solutions for the
[2023 Advent of Code](https://adventofcode.com/2023).
The initial library (aoc.el) was developed while warming up with the
[puzzles from 2015](https://adventofcode.com/2015).

The plan is to use [elisp](https://www.gnu.org/software/emacs/manual/elisp.html)
for the whole contest, but if I see a quick
solution using sed or awk, I may make a run for the leaderboard.

## Day 25: Snowverload

A graph problem, so I used graphviz to find the solution.
No part 2 because I don't have enough stars.

## Day 24: Never Tell Me The Odds

Finding interesecting lines of flight. Only part 1.

## Day 23: A Long Walk

Company arrived for the holiday. Haven't gotten to this one yet.

## Day 22: Sand Slabs

Sort of tetris-y, you had to disintegrate falling blocks. Only part 1.

## Day 21: Step Counter

Another grid problem. Another one-off solution.
There's a lot of refactoring in my future.

## Day 20: Pulse Propagation

Solved part one with Ruby. Part two still TBD.

## Day 19: Day 19: Aplenty

Nice problem for a lispy language. For part one, I converted the input
to S-expressions (using Perl) and evaluated the results. For part two, I wrote a
little interpreter that tracked valid values as it interpreted the rules.

## Day 18: Lavaduct Lagoon

Drew out the map and used a flood fill for part one.
Slept on part two, implemented rotations for the path, then started
eliminating the "bumps" before realizing Mathematica makes
this problem trivial.

## Day 17: Clumsy Crucible

It's got me feeling clumsy. No working solution yet.

## Day 16: The Floor Will Be Lava

Tracing beams of light through a hall of mirrors.
It's about time to refactor all the 2D functions from the last couple of days
into aoc.el.

I spent a lot of time hunting down an issues caused by a bad INCF statement
to move west

    (cl-incf (beam-col beam)) -1)

It's valid syntax, but not what I meant. I'll have to switch to DECF.

## Day 15: Lens Library

Implemening a hash table. Pretty easy compared to the last couple of days.

## Day 14: Parabolic Reflector Dish

An inefficient solution, but I went to bed and solved it in the morning
using output collected in a buffer over night. There was enough data to
find the pattern after the first hour, but I needed the sleep anyway.

## Day 13: Point of Incidence

Used Ruby to help get unstuck interpreting the problem description.

## Day 12: Hot Springs

Part 1 complete and committed, but it was slow. The expansion in part 2
will need a different solution.

## Day 11: Cosmic Expansion

Quick part 1, but I rotated the universe the wrong way the first time
through part 2. That took a while to find.

## Day 10: Pipe Maze

It took me quite a while to figure out part 2. I ended up expanding the
map into 3x3 tiles to make it easier to use a flood fill to figure out
what was inside and what was outside the path.

## Day 9: Mirage Maintenance

A couple of recursive functions and it was my best day so far.

## Day 8: Haunted Wasteland

A little math helped solve part 2.

One unusual thing that hung me up briefly, I had to update the parser
to test part 2 because the test locations for that half included
digits when the other inputs only had uppercase letters.

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
