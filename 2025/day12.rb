#!/usr/local/bin/ruby
# Friday, December 12, 2025
# Advent of Code 2025

class Shape
  attr_accessor :used
  def initialize(idx, cs)
    @cs = cs
    @idx = idx
    @rot = 0
    @pos = 0
    @used = cs.map {it.count('#')}.sum
  end
end

class Region
  attr_accessor :list
  def initialize(width,length,list)
    @width = width
    @length = length
    @list = list
  end
  def area
    @length * @width
  end
  def each
    @list.each do |elm|
      yield elm
    end
  end
  def each_with_index
    @list.length.times do |i|
      yield [@list[i], i]
    end
  end
end
          
def get_shape(txt)
  lines = txt.sub(/\s+$/, '').split /\n/
  idx = lines.shift
  s = Shape.new(idx, lines)
  s
end

def get_regions(txt)
  lines = txt.sub(/\s+$/, '').split /\n/
  lines.each do |line|
    if line.match /(\d+)x(\d+): (.+)/
      wid = $1.to_i
      len = $2.to_i
      lst = $3.split(' ').map &:to_i
      r = Region.new(wid, len, lst)
      yield r
    end
  end
end

shapes = []
regions = []

$/ = ''                         # read paragraphs
ARGF.each_line do |txt|
  if txt =~ /#/
    shapes << get_shape(txt)
  else
    get_regions(txt) {|r| regions << r}
  end
end

okay = 0
regions.each do |r|
  needed = 0
  r.each_with_index do |n, i|
    needed += shapes[i].used * n
  end
  if needed < r.area
    okay += 1
  end
end
puts okay

