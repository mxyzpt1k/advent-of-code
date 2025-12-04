#!/usr/bin/ruby
# Thursday, December 04, 2025
# Advent of Code 2025

$grid = []
$rows = 0
$cols = 0

$stdin.each_line do |line|
  row = line.chomp.split('')
  $cols = row.length
  row = ['#'] + row + ['#']
  if $grid.empty?
    $grid << row.map {|c| '#'}
  end
  $grid << row
  $rows += 1
end

$grid << $grid[0]

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

sum = 0
$rows.times do |r|
  $cols.times do |c|
    n = count_neighboring(r+1, c+1, '@')
    if n < 4
      sum += 1
    end
  end
end

puts sum
