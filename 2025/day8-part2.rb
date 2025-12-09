#!/usr/local/bin/ruby
# Advent of Code 2025
# Monday, December 08, 2025

# first time using Ruby 3.4

Point = Data.define :x, :y, :z

def distance(a,b)
  Math.sqrt((a.x - b.x)**2 + (a.y - b.y)**2 + (a.z - b.z)**2)
end

$nodes = []
$edges = []

$stdin.each_line do |line|
  x,y,z = line.split(',').map &:to_i
  p = Point.new(x, y, z)
  $nodes.each do |q|
    d = distance(p, q)
    $edges << [d, q, p]
  end
  $nodes << p
end

puts "done reading"
$edges = $edges.sort {|a,b| a[0] <=> b[0]}

class Circuit
  attr_accessor :data
  def initialize(p, q)
    @data = Set.new
    @data << p
    @data << q
  end
  def add(other)
    @data += other.data
  end
  def <<(point)
    @data << point
  end
  def size
    @data.size
  end
  def eql?(other)               # for :uniq
    self === other
  end
  def member?(thing)
    @data.member? thing
  end
end

def finish(edge)
  d, p, q = edge
  puts edge.to_s
  puts p.x * q.x
  exit
end

def combine(edges)
  lookup = {}
  edges.each do |tup|
    _, p, q = tup
    pc = lookup[p]
    qc = lookup[q]
    if pc.nil? and qc.nil?
      pc = Circuit.new(p,q)
      lookup[p] = lookup[q] = pc
    elsif pc.nil?
      qc << p
      finish(tup) if qc.size == $nodes.size
      lookup[p] = qc
    elsif qc.nil?
      pc << q
      finish(tup) if pc.size == $nodes.size
      lookup[q] = pc
    elsif pc === qc
      ;# nothing to do
    else
      pc.add( qc )
      finish(tup) if pc.size == $nodes.size
      lookup[q] = pc
    end
  end
end

combine $edges

# 31765223 is too low
