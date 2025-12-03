#!/usr/bin/ruby
# Wednesday, December 03, 2025
# Advent of Code 2025

# part 2

def largest(ds,needed)
  # find the largest digit with space left in line
  z = ds.length - needed
  d = ds[0 .. z].max
  [d, ds.index(d)]
end

sum = 0
$stdin.each_line do |line|
  line.chomp!
  ds = line.split('').map &:to_i
  n = 0
  pos = 0
  len = 12
  12.times do
    d,pos = largest(ds, len)
    len = len - 1
    n = n*10 + d
    ds = ds[pos+1 .. ]
  end
  sum += n
end
puts sum


__END__

# part 1
sum = 0
$stdin.each_line do |line|
  line.chomp!
  a = line.split('').sort.reverse.first
  left, right = line.split(a,2)
  if right.empty?
    b = left.split('').sort.reverse.first
    n = b + a
  else
    b = right.split('').sort.reverse.first
    n = a + b
  end
  sum += n.to_i
end
puts sum

