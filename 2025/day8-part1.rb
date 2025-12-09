#!/usr/local/bin/ruby
# Advent of Code 2025
# Monday, December 08, 2025

# first time using Ruby 3.4

Point = Data.define :x, :y, :z

def distance(a,b)
  Math.sqrt((a.x - b.x)**2 + (a.y - b.y)**2 + (a.z - b.z)**2)
end

points = []
shortest = []

$stdin.each_line do |line|
  x,y,z = line.split(',').map &:to_i
  p = Point.new(x, y, z)
  points.each do |q|
    d = distance(p, q)
    shortest << [d, p, q]
  end
  points << p
end

shortest.sort! {|a,b| a[0] <=> b[0]}
shortest = shortest[0..999]
puts "done reading"
puts shortest.length

def circuit_sizes(shortest)
  circuits = []
  until shortest.empty?
    _, p0, p1 = shortest.shift
    circ = { p0 => 1, p1 => 1 }
    loop do
      rm = []
      shortest.each_with_index do |tup, i|
        _, p2, p3 = tup
        if circ.fetch(p2, false) or circ.fetch(p3, false)
          rm << i
          circ[p2] = 1
          circ[p3] = 1
        end
      end
      if rm.empty?
        circuits << circ
        break
      else
        rm.reverse.each { shortest.delete_at it }
      end
    end
  end
  circuits.map { it.keys.length }
end

cs = circuit_sizes(shortest)
puts cs.to_s
puts cs.sort.reverse.slice(0,3).to_s
puts cs.sort.reverse.slice(0,3).reduce { _1 * _2 }
