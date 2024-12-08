# Advent of Code
# Sunday, December 08, 2024

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#   8   01:07:42   7821      0   01:34:23   7906      0

$map = []
$stdin.each_line do |line|
  $map << line.chomp.chars
end

$max_col = 0
$max_row = 0

$lookup = {}
$map.each_with_index do |row, r|
  $max_row = r
  row.length.times do |col|
    $max_col = col if r == 0
    c = row[col]
    unless (c == ?. or c == ?%)
      ps = $lookup.fetch(c, [])
      ps << [r,col]
      $lookup[c] = ps
    end
  end
end

def antinodes(p,q)              # part 1
  a1 = [0,0]
  a2 = [0,0]
  row = p[0] - q[0]
  col = p[1] - q[1]
  if p[0] + row == q[0]
    a1[0] = p[0] - row
    a2[0] = q[0] + row
  else
    a1[0] = p[0] + row
    a2[0] = q[0] - row
  end
  if p[1] + col == q[1]
    a1[1] = p[1] - col
    a2[1] = q[1] + col
  else
    a1[1] = p[1] + col
    a2[1] = q[1] - col
  end
  [a1, a2]
end

class Antinodes                 # part 2

  attr_accessor :nodes
  
  def initialize(p, q)
    @rows = p[0] - q[0]
    @cols = p[1] - q[1]
    @nodes = [p,q]
    generate(p,q)
    generate(q,p)
  end
  
  def generate(p,q)
    n = [0,0]
    if p[0] + @rows == q[0]
      n[0] = p[0] - @rows
    else
      n[0] = p[0] + @rows
    end
    if p[1] + @cols == q[1]
      n[1] = p[1] - @cols
    else
      n[1] = p[1] + @cols
    end
    if valid? n
      @nodes << n
      generate(n,p)
    end
  end
end

def valid?(pt)
  pt[0] >= 0 and pt[1] >= 0 and pt[0] <= $max_row and pt[1] <= $max_col
end

def collect(points)
  acc = []
  if points.length < 2
    acc
  else
    p = points[0]
    ps = points[1,points.length-1]
    ps.each do |q|
      # acc += antinodes(p,q)  # part 1
      acc += Antinodes.new(p,q).nodes # part 2
    end
    acc + collect(ps)
  end
end

$nodes = {}
$lookup.keys.each do |c|
  collect($lookup[c]).each do |node|
    if valid? node
      $nodes[node] = 1
    end
  end
end

puts $nodes.length

exit

$nodes.keys.each do |p|
  r,c=p
  $map[r][c] = ?# if $map[r][c] == ?.
end

$map.each do |line|
  puts line.join('')
end
