#!/usr/bin/ruby
# Thursday, December 04, 2025
# Advent of Code 2025

$grid = []
$rows = 0
$cols = 0

$stdin.each_line do |line|
  row = line.chomp.split('')
  row = ['#'] + row + ['#']
  if $grid.empty?
    $grid << row.map {|c| '#'}
  end
  $grid << row
  $rows += 1
end

$cols = $grid[0].length - 2
$grid << $grid[0]

$paper = {}
$rows.times do |r|
  $cols.times do |c|
    p = [r+1,c+1]
    $paper[p] = 1 if $grid[r+1][c+1] == '@'
  end
end

def count_neighboring(r, c, wanted)
  if $grid[r][c] != '@'
    99
  else
    [$grid[r-1][c-1], $grid[r-1][c], $grid[r-1][c+1],
     $grid[r][c-1], $grid[r][c+1],
     $grid[r+1][c-1], $grid[r+1][c], $grid[r+1][c+1]
    ].count {|x| wanted == x}
  end
end

def display
  $grid.each do |row|
    puts row.to_s
  end
  puts $paper.length
  puts ''
end

# part 2
sum = 0
loop do
  changes = []
  $paper.keys.each do |pt|
    r,c = pt
    n = count_neighboring(r, c, '@')
    if n < 4
      sum += 1
      $grid[r][c] = '.'
      changes << pt
    end
  end
  if changes.empty?
    break
  else
    changes.each { |p| $paper.delete p }
  end
end

#display
puts sum
