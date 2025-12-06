#!/usr/bin/ruby
# Advent of Code 2025
# Saturday, December 06, 2025

# part 1

require 'matrix'
sheet = []

$stdin.each_line do |line|
  row = line.chomp.split
  sheet << row
end

sum = 0
m = Matrix.rows sheet
m.column_vectors.each do |v|
  a = v.to_a
  op = a.pop
  if op == '*'
    n = a.reduce {|a,b| a.to_i * b.to_i}
  else
    n = a.map(&:to_i).sum
  end
  sum += n
end
puts sum
