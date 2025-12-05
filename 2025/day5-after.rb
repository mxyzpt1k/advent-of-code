#!/usr/bin/ruby
# Friday, December 05, 2025
# Advent of Code 2025

# monkey patching the Range class for this
class Range
  def overlap?(other)
    self.first.between?(other.first, other.last) or
      other.first.between?(self.first, self.last)
  end
  def concat(other)
    [self.first, other.first].min .. [self.last, other.last].max
  end
end

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

def reduce (ranges, sum)
  ranges.length.times do
    r1 = ranges.shift
    remove = []
    ranges.each_with_index do |r2, i|
      if r1.overlap? r2
        r1 = r1.concat r2
        remove << i
      end
    end
    remove.sort.reverse.map {|i| ranges.delete_at i}
    ranges << r1
    #puts ranges.length
  end
  newsum = ranges.map(&:size).sum
  if newsum != sum
    reduce ranges, newsum
  else
    newsum
  end
end

sum = reduce $ranges, $ranges.map(&:size).sum
puts sum
# not 428637687860195
