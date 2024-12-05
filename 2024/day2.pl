#!/usr/bin/perl
# Sunday, December 01, 2024
# Advent of Code

# Day       Time  Rank  Score       Time  Rank  Score
#   2   00:18:33  6368      0   00:36:14  5580      0

use strict;
use warnings;
use List::Util qw(all);

my $count = 0;
while (<>) {
  my @row = split;
  $count ++ if safe_part2(@row);
}
print "$count\n";

sub safe_part2 {
  my @row = @_;
  my ($i,$j);
  for ($i=0; $i<@row; $i++) {
    if (safe_part1( remove( $i, @row ))) {
      return 1;
    }
  }
  0;
}

sub remove {
  my ($i, @row) = @_;
  my @idxs = grep $_ != $i, (0 .. $#row);
  @row[@idxs];
}

sub safe_part1 {
  my @dirs = ();
  my ($i,$j);
  for ($i=0, $j=1; $j<@_; $i++, $j++) {
    my $diff = $_[$i] - $_[$j];
    push @dirs, $diff;
    $diff = abs($diff);
    if ($diff < 1 or $diff > 3) {
      return 0;
    }
  }
  same_sign( @dirs );
}

sub same_sign {
  my $sign = sign($_[0]);
  all { $sign eq sign($_) } @_;
}

sub same_sign_01 {
  if (@_ == 1) {
    1;
  }
  elsif (sign($_[0]) ne sign($_[1])) {
    0;
  }
  else {
    same_sign( @_[1 .. $#_] );
  }
}

sub sign {
  if ($_[0] < 0) {
    '-' ;
  } else {
    '+';
  }
}
