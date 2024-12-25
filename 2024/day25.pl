# Advent of Code
# Wednesday, December 25, 2024

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  25   00:36:06   3666      0          -      -      -

use strict;
use warnings;

$/ = '';

my @keys = ();
my @locks = ();

while (<>) {
  chomp;
  my @lines = split /\n/;
  my @vs = (0, 0, 0, 0, 0);
  for (my $r=0; $r<@lines; $r++) {
    for (my $c=0; $c<5; $c++) {
      my $d = substr($lines[$r], $c, 1);
      if ($d eq '#') {
	$vs[$c] ++;
      }
    }
  }
  if ($lines[0] eq '.....') {
    push @keys, \@vs;
  }
  if ($lines[0] eq '#####') {
    push @locks, \@vs;
  }
}

my $matches = 0;
for my $lock (@locks) {
  for my $key (@keys) {
    my $tumbler = 0;
    for (my $i=0; $i<5; $i++) {
      if ($key->[$i] + $lock->[$i] < 8) {
	$tumbler ++;
      }
    }
    if ($tumbler == 5) {
      $matches ++;
    }
    #print "lock @$lock, key @$key\n";
  }
}

print "$matches\n";
