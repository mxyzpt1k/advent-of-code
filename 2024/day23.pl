#!/usr/bin/perl
# Advent of Code 2024
# Monday, December 23, 2024

# this is (most of) part 1, pipe the output to: sort -u | wc

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  23   00:16:41   1846      0   02:34:42   4870      0

use strict;
use warnings;

my %net = ();

while (<>) {
  chomp;
  my ($a,$z) = split /-/;
  $net{$a} //= {};
  $net{$a}{$z} = 1;
  $net{$z} //= {};
  $net{$z}{$a} = 1;
}

# part 1
for my $t (keys %net) {
  if ($t =~ /^t/) {
    my @bs = keys %{$net{$t}};
    for my $b (@bs) {
      next if $b eq $t;
      my @cs = keys %{$net{$b}};
      for my $c (@cs) {
	next if $c eq $b;
	if ($net{$c}{$t}) {
	  my @ary = sort ($t,$b,$c);
	  print "@ary\n";
	}
      }
    }
  }
}
 
