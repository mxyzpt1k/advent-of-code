#!/usr/bin/ruby
# Friday, December 05, 2025
# Advent of Code 2025

$ranges = []

$stdin.each_line do |line|
  line.chomp!
  break if line == ''
  a,z = line.split('-')
  $ranges << (a.to_i .. z.to_i)
end

# part 1

$fresh = 0
$stdin.each_line do |line|
  line.chomp!
  n = line.to_i
  $ranges.each do |r|
    if r.include? n
      $fresh += 1
      break
    end
  end
end
puts $fresh

# part 2

def overlap? (r1, r2)
  if r1.first.between? r2.first, r2.last
    true
  elsif r2.first.between? r1.first, r1.last
    true
  else
    false
  end
end

def reduce (ranges, sum)
  loop do
    ranges.length.times do
      r1 = ranges.shift
      remove = []
      ranges.each_with_index do |r2, i|
        if overlap? r1, r2
          r1 = [r1.first, r2.first].min .. [r1.last, r2.last].max
          remove << i
        end
      end
      remove.sort.reverse.map {|i| ranges.delete_at i}
      ranges << r1
      puts ranges.length
    end
    newsum = ranges.map(&:size).sum
    if newsum != sum
      sum = newsum
    else
      return newsum
    end
  end
end

sum = reduce $ranges, $ranges.map(&:size).sum
puts sum
# not 428637687860195
