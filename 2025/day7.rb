#!/usr/bin/ruby
# Sunday, December 07, 2025
# Advent of Code 2025

splits = 0                      # part 1
beams = {}                      # part 2

$stdin.each_line do |line|
  row = line.chomp.split ''
  row.each_with_index do |c, pos|
    if c == 'S'
      beams[pos] = 1
    elsif c == '^'
      if beams[pos]
        splits += 1
        beams[pos+1] = beams.fetch(pos+1, 0) + beams[pos]
        beams[pos-1] = beams.fetch(pos-1, 0) + beams[pos]
        beams.delete pos
      end
    end
  end
end

puts splits
puts beams.values.sum

