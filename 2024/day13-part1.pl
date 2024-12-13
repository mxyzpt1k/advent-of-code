#!/usr/bin/perl
# Advent of Code
# Friday, December 13, 2024

use strict;
use warnings;

my %button = ();
my %cost = (A => 3, B => 1);
my @prize = ();
my $score = 0;

while (<>) {
  if (/Button ([AB]): X\+(\d+), Y\+(\d+)/) {
    $button{$1} = [$2,$3];
  }
  if (/Prize: X=(\d+), Y=(\d+)/) {
    @prize = ($1,$2);
    $score += best_cost();
  }
}
print "$score\n";

sub min {
  my ($m, $n) = @_;
  ($m < $n) ? $m : $n;
}

sub best_cost {
  my $cost = 0;
 OuterLoop:
  for (my $as=0; $as<=100; $as++) {
    for (my $bs=0; $bs<=100; $bs++) {
      if ($as + $bs > 200) {
	next;
      }
      if ($as*$button{A}[0] + $bs*$button{B}[0] == $prize[0]) {
	if ($as*$button{A}[1] + $bs*$button{B}[1] == $prize[1]) {
	  my $v = $as*$cost{A} + $bs*$cost{B};
	  if ($cost == 0) {
	    $cost = $v;
	  } else {
	    $cost = min($v, $cost);
	  }
	}
	next OuterLoop;
      }
      if ($as*$button{A}[1] + $bs*$button{B}[1] > $prize[1]) {
	next OuterLoop;
      }
    }
 }
  $cost;
}

