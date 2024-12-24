#!/usr/bin/ruby
# Advent of Code
# Tuesday, December 24, 2024

wires = {}
code = []
zs = []

$stdin.each_line do |line|
  if line =~ /(\w+): (\d)/
    wires[$1] = $2.to_i
  elsif line =~ /(\w+) (\w+) (\w+) -> (\w+)/
    res = $4
    code << [$2, $1, $3, res]
    if res.start_with? 'z'
      zs << res
    end
  else
    #print "?? " + line
  end
end

def apply(op, a, b)
  case op
  when 'AND'
    a & b
  when 'OR'
    a | b
  when 'XOR'
    a ^ b
  end
end

count = 0
loop do
  change = false
  count += 1
  code.each do |line|
    op, a, b, res = line
    unless wires.has_key? res
      if wires.has_key? a and wires.has_key? b
        wires[res] = apply(op, wires[a], wires[b])
        change = true
      end
    end
  end
  break unless change
end

puts count
n = 0
zs.sort.reverse.each do |b|
  #puts "#{b} => #{wires[b]}"
  n = (n<<1) + wires[b]
end
puts n

