#!/usr/bin/perl
# Advent of Code 2025
# Saturday, December 06, 2025

# Part 2

use strict;
use warnings;
use List::Util qw(sum product);

my @rows = ();
while (<>) {
  chomp;
  push @rows, $_;
}
my $ops = pop @rows;

# the offset of each op is the leftmost column of a row
# a column of numbers should be above it

my $sum = 0;
my $offset = 0;
while ($ops =~ s/([*+]([^*+]*|$))//) {
  my $len = length $1;
  my $op = $1;
  my @nums = ();
  for (@rows) {
    push @nums, substr($_, $offset, $len);
  }
  my @cs = cephalopod(@nums);
  my $x = apply($op, @cs);
  #print "$op @cs = $x\n";
  $sum += $x;
  $offset += $len;
}
print "$sum\n";

sub apply {
  my $op = shift;
  if ($op =~ /\+/) {
    sum(@_);
  } else {
    product(@_);
  }
}

sub cephalopod {
  my @nums = ();
  for (my $i=0; $i<length($_[0]); $i++) {
    my $n = 0;
    for my $s (@_) {
      my $c = substr($s, $i, 1);
      if ($c ne ' ') {
	$n = $n*10 + $c;
      }
    }
    push @nums, $n if $n;
  }
  @nums;
}
