# Monday, December 01, 2025
# Advent of Code 2025

use strict;
use warnings;

my $pos = 50;
my $zeros = 0;			# part 1

my $z2 = 0;			# part 2
my $pos2 = 50;

my $z3 = 0;			# afterwards
my $pos3 = 50;

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

  # afterwards - part 2 without another loop
  $z3 += int(abs($_) / 100);
  my $rem = $_ % 100 - (($_ < 0) ? 100 : 0);
  my $sum = $pos3 + $rem;
  $z3 ++ if $sum >= 100 or ($sum <= 0 and $pos3 > 0);
  $pos3 = $sum % 100;

  #printf "%5d :: $pos2: $z2  :: $pos3: $z3\n", $_;
}

print "$zeros\n";
print "$z2\n";
print "$z3\n";
