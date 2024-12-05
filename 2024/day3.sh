# Day       Time  Rank  Score       Time  Rank  Score
#   3   00:04:15  1117      0   00:10:40  1233      0

# solved on the command line

# part 1
perl -lne 'while (s/\bmul\((\d+),(\d+)\)//) { $t += $1*$2 } END { print $t }' day3.2024.input.txt
# 167090022

# part 2
perl -lne 'BEGIN { $do=1 }; while (s/\b(mul\((\d+),(\d+)\)|do(?:n'"'"'t)?\(\))//) {$c=$1; if ($c =~ /don/) { $do=0 } elsif ($c =~ /do/) { $do=1 } else { $t += $do*$2*$3 }} END { print $t}' day3.2024.input.txt 
# 89823704
