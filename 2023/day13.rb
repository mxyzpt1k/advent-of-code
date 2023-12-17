# Day 13: Point of Incidence
# Advent of Code 2023
# Wednesday, December 13, 2023

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
# 13   22:41:51  27921      0        >24h  30865      0

# got stuck in elisp, trying something different

# part 1

$goes_to_edge = false           # this is what took me a while to realize

def column_equal(m, a, b)
  m.each do |row|
    if row[a] != row[b]
      return false
    end
  end
  true
end

def vertical_size(m, a, b, limit)
  if a < 0 or b >= limit
    $goes_to_edge = true
    0
  elsif column_equal(m, a, b)
    1 + vertical_size(m, a-1, b+1, limit)
  else
    0
  end
end

def vertical_mirror(m)
  $goes_to_edge = false
  (m[0].length - 1).times do |k|
    if column_equal(m, k, k+1)
      s = vertical_size(m, k, k+1, m[0].length)
      if $goes_to_edge
        return k+1
      end
    end
  end
  0
end

def horizontal_size(m, a, b, limit)
  if a < 0 or b >= limit
    $goes_to_edge = true
    0
  elsif m[a] == m[b]
    1 + horizontal_size(m, a-1, b+1, limit)
  else
    0
  end
end

def horizontal_mirror(m)
  $goes_to_edge = false
  (m.length - 1).times do |k|
    if m[k] == m[k+1]
      s = horizontal_size(m, k, k+1, m.length)
      if $goes_to_edge
        return k+1
      end
    end
  end
  0
end

def find_mirrors(m)
  hs = horizontal_mirror(m)
  vs = vertical_mirror(m)
  vs + hs*100
end

def part1
  $/ = ''
  score = 0
  $stdin.each do |thing|
    lines = thing.split "\n"
    score += find_mirrors(lines)
  end
  puts "part1: #{score}"
end

#part1
# end of part 1

# part 2

def horizontal_mirror2(m)
  (m.length - 1).times do |k|
    if m[k] == m[k+1]
      $goes_to_edge = false
      s = horizontal_size(m, k, k+1, m.length)
      if $goes_to_edge
        yield k+1
      end
    end
  end
end

def vertical_mirror2(m)
  (m[0].length - 1).times do |k|
    if column_equal(m, k, k+1)
      $goes_to_edge = false
      s = vertical_size(m, k, k+1, m[0].length)
      if $goes_to_edge
        yield k+1
      end
    end
  end
end

def find_mirrors2(m)
  horizontal_mirror2(m) {|s| yield s*100}
  vertical_mirror2(m) {|s| yield s}
end

def find_smudge(mirror)
  score = find_mirrors(mirror)
  width = mirror[0].length
  best = 0
  mirror.length.times do |row|
    width.times do |col|
      p = mirror[row][col]
      if p == ?#
        mirror[row][col] = ?.
      else
        mirror[row][col] = ?#
      end
      find_mirrors2(mirror) do |new_score|
        if new_score != score and new_score > 0
          best = new_score
        end
      end
      mirror[row][col] = p
    end
  end
  if best == 0
    puts mirror.join("\n")
  end
  best
end

def part2
  $/ = ''
  score = 0
  $stdin.each do |chunk|
    lines = chunk.split "\n"
    score += find_smudge(lines)
  end
  puts "part2: #{score}"
end

part2
