# part of Advent of Code 2023, Day 19
# Tuesday, December 19, 2023

use strict;
use warnings;

print "(setf day19-total 0)\n";

while (<>) {
  print "\n;; $_";
  chomp;
  last if $_ eq '';
  # kd{x>1248:xsx,m<797:cxm,ppz}
  # pg{x<3179:A,x>3508:R,s<2653:R,A}
  if (/^(\w+)\{(.+)\}/) {
    my ($name, $rules) = ($1, $2);
    print "(define-rule '$name (quote";
    my @rules = split /,/, $rules;
    make_rule($_) for @rules;
    print ")" x @rules;
    print ")\n";
  }
}

#while (<>) {
#  my @acc = ();
#  while (s/([xmas])=(\d+)//) {
#    push @acc, "($1 $2)";
#  }
#  print "(let (@acc) (apply in))\n";
#}

sub make_rule {
  my $r = shift;
  if ($r =~ /^([ARa-z]+)$/) {
    print call_rule($r);
  }
  elsif ($r =~ /([xmas])([<>])(\d+):(\w+)/) {
    my $dst = call_rule($4);
    print " (if ($2 $1 $3) $dst";
  }
  else {
      die "unexpected pattern: $r\n";
  }
}

sub call_rule {
  my $r = shift;
  if ($r eq 'A') {
    " (accept)";
  }
  elsif ($r eq 'R') {
   " (reject)";
  }
  else {
   " (apply $r)";
  }
}

