#!/usr/bin/ruby
# Advent of Cdoe 2024
# Monday, December 23, 2024

# this is part 2, part 1 is day23.pl
# switched to Ruby for sets

#      --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  23   00:16:41   1846      0   02:34:42   4870      0

require 'set'

network = {}
$stdin.each_line do |line|
  a,b = line.chomp.split '-'
  h = network.fetch(a, {})
  network[a] = h
  network[a][b] = 1
  h = network.fetch(b, {})
  network[b] = h
  network[b][a] = h
end

sets = []
network.keys.each do |h|
  s = Set.new
  s.add h
  network[h].keys.each do |k|
    s.add k
  end
  sets << s
end

best = 0
sets.each do |a|
  maybe = []
  sets.each do |b|
    c = a.intersection b
    if c.length > 3             # just guessing
      maybe << b
    end
  end
  maybe.each do |b|
    a = a.intersection b
  end
  if a.length > best
    best = a.length
    puts a.to_a.sort.join(',')
  end
  #break
end



  
