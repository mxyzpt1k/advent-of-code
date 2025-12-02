#!/usr/bin/perl
# Advent of Code 2025
# Tuesday, December 02, 2025

use strict;
use warnings;

my $total = 0;
my $total2 = 0;

while (<>) {
  chomp;
  my @rs = split /,/;
  for my $r (@rs) {
    my ($a,$z) = split /-/, $r;
    for (my $n=$a; $n<=$z; $n++) {
      # part 1
      if ($n =~ /^(\d+)\1$/) {
	$total += $&;
      }
      # part 2
      if ($n =~ /^(\d+)\1+$/) {
	$total2 += $&;
      }
    }
  }
}

print "total: $total\n";
print "total2: $total2\n";
