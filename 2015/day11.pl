# day11.pl - advent of code 2015
# Monday, November 20, 2023
# warming up for 2023

my $pass = "hxbxwxba";
$pass = next_pass($pass);
print "1) $pass\n";
$pass = next_pass($pass);
print "2) $pass\n";

sub next_pass {
  my $pass = shift;
  while (1) {
    $pass++;
    $pass =~ /^[a-z]+$/ or next;
    $pass =~ /[iol]/ and next;
    $pass =~ /(.)\1.*(.)\2/ or next;
    next if $1 eq $2;
    $pass =~ /abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz/ or next;
    return $pass;
  }
}

