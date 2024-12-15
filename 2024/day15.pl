#!/usr/bin/perl
# Advent of Code
# Sunday, December 15, 2024

use strict;
use warnings;

my @grid = ();
my @program = ();
my $bot_x = 0;
my $bot_y = 0;

while (<>) {
  chomp;
  last unless $_;
  if (/.+\@/) {
    $bot_x = length($&) - 1;
    $bot_y = @grid ;
  }
  my @chars = split //;
  push @grid, \@chars;
}

while (<>) {
  chomp;
  my @chars = split //;
  push @program, @chars;
}

sub move {
  my ($dy,$dx) = @_;
  sub {
    my ($y, $x, $move) = @_;
    my $yy = $y+$dy;
    my $xx = $x+$dx;
    my $peek = $grid[$yy][$xx];
    if ($peek eq '#') {
      ($y,$x);
    }
    elsif ($peek eq '.') {
      $grid[$yy][$xx] = $grid[$y][$x];
      $grid[$y][$x] = '.';
      ($yy, $xx);
    }
    elsif ($peek eq 'O') {
      &$move($yy,$xx,$move);
      $peek = $grid[$yy][$xx];
      if ($peek eq '.') {
	$grid[$yy][$xx] = $grid[$y][$x];
	$grid[$y][$x] = '.';
	($yy, $xx);
      } else {
	($y, $x);
      }
    }
    else {
      die "unexpected cell contents: $peek at yy=$yy, xx=$xx for y=$y, x=$x\n";
    }
  }
}

my $north = move(-1, 0);
my $south = move( 1, 0);
my $east = move(0,  1);
my $west = move(0, -1);

my %moves = (
  '^' => $north,
  'v' => $south,
  '<' => $west,
  '>' => $east
    );

for (@program) {
  my $move = $moves{$_};
  ($bot_x,$bot_y) = &$move($bot_x, $bot_y, $move);
  #display();
}

sub display {
  foreach (@grid) {
    print join('', @$_);
    print "\n";
  }
  print "$_\n\n";
}
print score(),"\n";

sub score {
  my $score = 0;
  for (my $y=0; $y<@grid; $y++) {
    for (my $x=0; $x<@{$grid[$y]}; $x++) {
      if ($grid[$y][$x] eq 'O') {
	$score += ($y*100 + $x);
      }
    }
  }
  $score;
}

__END__
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
