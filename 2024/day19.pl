#!/usr/bin/perl
# Advent of Code
# Thursday, December 19, 2024

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  19   00:11:29   1527      0   02:39:24   6678      0

use strict;
use warnings;
use Memoize;

memoize('part2');

my @towels = ();
my $possible = 0;		# for part 1
my $regexp = '';
my @some_towels = ();
my $combos = 0;			# for part 2

$| = 1;

while (<>) {
  chomp;
  if ($. == 1) {
    @towels = split /, /;
    my $s = join '|', @towels;
    $regexp = qr(^($s)+$);
  }
  elsif ($. > 2) {
    if (/$regexp/) {
      $possible ++;
      my $word = $_;
      @some_towels = grep { index($word, $_) >= 0 } @towels;
      $combos += part2( $_ );
      print "$word  :: $combos\n";
    }
  }
}

print "$possible\n";
print "$combos\n";

sub part2 {
  my $word = shift;
  my $n = 0;
  for (@some_towels) {
    if (index($word, $_) == 0) {
      if ($word eq $_) {
	$n ++;
      } else {
	$n += part2(substr($word, length($_))); 
      }
    }
  }
  $n;
}
