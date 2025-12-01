# Monday, December 01, 2025
# Advent of Code 2025

use strict;
use warnings;

my $pos = 50;
my $zeros = 0;			# part 1

my $z2 = 0;			# part 2
my $pos2 = 50;

while (<>) {
  chomp;
  s/R//;
  s/L/-/;

  # part 1
  $pos = ($pos + $_) % 100;
  $zeros++ unless $pos;

  # part 2
  my $sign = (abs($_) == $_) ? 1 : -1;
  for (my $i=$_; $i!=0; $i -= $sign) {
    $pos2 = ($pos2 + $sign) % 100;
    $z2 ++ unless $pos2;
  }
}

print "$zeros\n";
print "$z2\n";
