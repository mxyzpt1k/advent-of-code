#!/usr/local/bin/ruby
# Thursday, December 11, 2025
# Advent of Code 2025, day 11

# thanks to https://dreampuf.github.io/GraphvizOnline/

graph = {}

$stdin.each_line do |line|
  ds = line.chomp.sub(':','').split
  node = ds.shift
  graph[node] = ds
end

def walk_tree(graph, node, finish, memo)
  if memo.has_key? node
    memo[node]
  elsif node == finish
    1
  else
    sum = 0
    graph.fetch(node,[]).each do |n|
      ps = walk_tree(graph, n, finish, memo)
      sum += ps
      memo[n] = ps
    end
    sum
  end
end

# part 1
puts walk_tree(graph, 'you', 'out', {})

# part 2
# graphviz shows that fft comes before dac
svr_fft = walk_tree(graph, 'svr', 'fft', {})
fft_dac = walk_tree(graph, 'fft', 'dac', {})
dac_out = walk_tree(graph, 'dac', 'out', {})
puts (svr_fft * fft_dac) * dac_out
