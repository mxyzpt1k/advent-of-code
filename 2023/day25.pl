#!/usr/bin/perl
# Day 25: Snowverload
# Advent of Code 2023

# convert input data into "dot" format to look at it.
# neato output made the three necessary cuts pretty obvious.

# usage: perl day25.pl input.txt | neato -Tpdf > day25.pdf

use strict;
use warnings;

my @cuts = qw(kdk nct cvx tvj fsv spx);
my %check1 = @cuts;
my %check2 = reverse(@cuts);

print "graph {\n";
while (<>) {
  my ($key, @rest) = split;
  $key =~ s/://;
  for (@rest) {
    #next if cut($key,$_);
    print "  $key -- $_\n";
  }
}
print "}\n";

sub cut {
  my ($p, $q) = @_;
  if (exists $check1{$p}) {
    $check1{$p} eq $q;
  }
  elsif (exists $check2{$p}) {
    $check2{$p} eq $q;
  }
  else { 0 }
  
}
