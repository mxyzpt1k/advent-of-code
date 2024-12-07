#!/usr/bin/perl
# Saturday, December 07, 2024
# Advent of Code

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#   7   00:15:13   2715      0   00:59:07   6626      0

use strict;
use warnings;

my $sum = 0;
while (<>) {
  s/://;
  my ($test, @rest) = split;
  if (ispossible($test, 0, @rest)) {
    $sum += $test;
  }
  elsif (ispossible2($test, 0, @rest)) {
    $sum += $test;
  }
}
print "$sum\n";

sub ispossible2 {
  my ($target, $value, @rest) = @_;
  #print "$target $value @rest\n";
  if ($value > $target) {
    0;
  }
  elsif (not @rest) {
    $target == $value;
  }
  else {
    my ($head,@tail) = @rest;
    if (ispossible2($target, $value+$head, @tail)) {
      1;
    }
    elsif (ispossible2($target, $value*$head, @tail)) {
      1;
    }
    else {
      my $n = "$value" . "$head";
      ispossible2($target, $n*1, @tail);
    }
  }
}

sub ispossible {
  my ($target, $value, @rest) = @_;
  if ($value > $target) {
    0;
  }
  elsif (not @rest) {
    $target == $value;
  }
  else {
    my ($head,@tail) = @rest;
    if (ispossible($target, $value*$head, @tail)) {
      1;
    }
    elsif (ispossible($target, $value+$head, @tail)) {
      1;
    }
    else {
      0;
    }
  }
}

__DATA__
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
