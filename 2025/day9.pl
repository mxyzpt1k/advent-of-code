#!/usr/bin/perl
# December 9, 2025
# Advent of Code 2025

use strict;
use warnings;
use List::Util qw(max min);

my @points = ();

while (<>) {
  chomp;
  my ($x,$y) = split /,/;
  push @points, [$x,$y];
}

# part 1
my $biggest = 0;
for my $p (@points) {
  for my $q (@points) {
    $biggest = max(area($p, $q), $biggest);
  }
}
print "part 1: $biggest\n";

# part 2
my @lines = ();
my ($width, $height) = (0,0);
my $longest = 0;
my $p = $points[$#points];
for (my $i=0; $i<@points; $i++) {
  $width = max($p->[0]+10, $width); # for svg
  $height = max($p->[0]+10, $height);
  my $q = $points[$i];
  if ($p->[0] <= $q->[0]) {
    push @lines, [$p, $q];
  } else {
    push @lines, [$q, $p];
  }
  $p = $q;
}
@lines = sort {llen($b) <=> llen($a)} @lines;

my $big2 = 0;
for my $p (@points) {
 Qloop:
  for my $q (@points) {
    if ($p->[0] <= $q->[0]) {
      if (area($p,$q) > $big2) {
	for my $line (@lines) {
	  if (intersect($p, $q, @$line)) {
	    next Qloop;
	  }
	}
	$big2 = area($p,$q)
      }
    }
  }
}
print "part 2: $big2\n";

sub intersect {
  my ($p, $q, $a, $z) = @_;
  my @ys = ($p->[1], $q->[1]);
  if ($p->[1] > $q->[1]) {
    @ys = ($q->[1], $p->[1]);
  }
  if ($a->[0] <= $p->[0] and $z->[0] <= $p->[0]) {
    return 0;
  }
  elsif ($a->[0] >= $q->[0] and $z->[0] >= $q->[0]) {
    return 0;
  }
  elsif ($a->[1] <= $ys[0] and $z->[1] <= $ys[0]) {
    return 0;
  }
  elsif ($a->[1] >= $ys[1] and $z->[1] >= $ys[1]) {
    return 0;
  }
  else {
    1;
  }
}

sub area {
  my ($p, $q) = @_;
  (abs($p->[0] - $q->[0]) + 1) * (abs($p->[1] - $q->[1]) + 1);
}

sub llen {
  my $ps = shift;
  my ($p, $q) = @$ps;
  abs($p->[0] - $q->[0] + $p->[1] - $q->[1] + 1);
}

sub svg {
  open SVG, ">day9.svg";
  select SVG;
  my $w = int $width/100;
  my $h = int $height/100;
  print qq(<?xml version="1.0" encoding="UTF-8" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg width="$w" height="$h" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"  viewBox="0.00 0.00 $w $h">

<g>\n);

  my $p = $points[$#points];
  foreach (@points) {
    printf qq(<line x1="%0.2f" y1="%0.2f" x2="%0.2f" y2="%0.2f" stroke="black" stroke-width="1"/>\n), map(($_/100.0), $p->[0], $p->[1], $_->[0], $_->[1]);
    $p = $_;
  }
  
  print qq(</g>\n);
  print qq(</svg>\n);
  close SVG;
}

svg();

