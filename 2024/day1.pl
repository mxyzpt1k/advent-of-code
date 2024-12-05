#!/usr/bin/perl
# Advent of Code 2025
# Sunday, December 01, 2024

#      -------Part 1--------   -------Part 2--------
#Day       Time  Rank  Score       Time  Rank  Score
#  1   00:04:30  1672      0   00:07:50  1896      0

use strict;
use warnings;

my @xs;
my @ys;
while (<>) {
  my ($x, $y) = split;
  push @xs, $x;
  push @ys, $y;
}

# part 1
@xs = sort {$a <=> $b} @xs;
@ys = sort {$a <=> $b} @ys;
my $diff = 0;
for (my $i=0; $i<@xs; $i++) {
  $diff += abs( $xs[$i] - $ys[$i] );
}
print "$diff\n";

# part 2
my %ys = ();
for (@ys) {
  $ys{$_} ++;
}
my $score = 0;
for (@xs) {
  $score += $_ * ($ys{$_} // 0);
}
print "$score\n";
