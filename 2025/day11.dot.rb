#!/usr/local/bin/ruby
# Thursday, December 11, 2025

# dump AoC 2025 day 11 as a dot file

puts "graph G {"
$stdin.each_line do |line|
  ds = line.chomp.sub(':','').split
  node = ds.shift
  ds.each do |d|
    puts "#{node} -- #{d};"
  end
end
puts "}"
