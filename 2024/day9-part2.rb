#!/usr/bin/ruby
# Advent of Code
# Monday, December 09, 2024

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#   9   02:09:52  11507      0   10:21:50  20775      0

class ElfFile
  attr_accessor :pos, :size

  def initialize(id, pos, size)
    @id = id
    @pos = pos
    @size = size
  end

  def checksum
    sum = 0
    @size.times { |i| sum += (@pos+i) * @id }
    sum
  end
end

files = []
space = []
$stdin.each_line do |line|
  pos = 0
  line.chomp.chars.each_with_index do |c, i|
    id = i/2
    size = c.ord - ?0.ord
    if i.even?
      files << ElfFile.new( id, pos, size )
    else
      space << ElfFile.new( id, pos, size )
    end
    pos += size
  end
end

# compress
files.reverse.each do |f|
  space.each do |s|
    if f.pos <  s.pos
      break
    elsif s.size >= f.size
      f.pos = s.pos
      s.size -= f.size
      s.pos += f.size
      break
    end
  end
end

puts files.reduce(0) {|sum,f| sum + f.checksum}

# 8515929533392 is too high
