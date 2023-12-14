# Day 13: Point of Incidence
# Advent of Code 2023
# Wednesday, December 13, 2023

# got stuck in elisp, trying something different

$goes_to_edge = false

def Max(a,b)
  if a > b then a else b end
end

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

$/ = ''
score = 0
$stdin.each do |thing|
  lines = thing.split "\n"
  score += find_mirrors(lines)
end
puts score
