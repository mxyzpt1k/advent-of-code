#!/usr/bin/perl
# Advent of Code
# Friday, December 13, 2024

use strict;
use warnings;

my %button = ();
my @prize = ();
my $offset = 10000000000000;

while (<>) {
  if (/Button ([AB]): X\+(\d+), Y\+(\d+)/) {
    $button{$1} = [$2,$3];
  }
  if (/Prize: X=(\d+), Y=(\d+)/) {
    @prize = ($1+$offset, $2+$offset);
    printf "(cost %s %s %s %s %s %s)\n", @{$button{A}}, @{$button{B}}, @prize;
  }
}
