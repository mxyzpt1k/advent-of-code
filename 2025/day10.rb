#!/usr/local/bin/ruby
# Wednesday, December 10, 2025
# Advent of Code 2025

# generated code for Mathematica to solve part 2

require 'matrix'

def to_bits(s)
  n = 0
  s.chars.each_with_index do |c,i|
    if c == ?#
      n |= 2**i
    end
  end
  n
end

def parse(line)
  line.sub!(/\[(.+?)\] */, '')
  bits = to_bits($1)
  nums2 = []
  nums = []
  while line.sub!(/\((.+?)\) */, '')
    m = $1
    nums << m.split(',').reduce(0) { |a,b| a |= 2**b.to_i }
    nums2 << m.split(',').map(&:to_i)
  end
  line.sub!(/\{(.+?)\}/, '')
  jolts = $1.split(/,/).map &:to_i
  [bits, nums, nums2, jolts]
end

def solve1(bits, nums)
  s = Set.new
  s << bits
  1000.times do |i|
    t = Set.new
    s.each do |x|
      nums.each do |y|
        z = x ^ y
        if z.zero?
          return i+1
        else
          t << z
        end
      end
    end
    s = t
  end
  -1
end

def to_matrix(nums, jolts)
  rows = []
  nums.each do |ns|
    row = jolts.map { 0 }
    ns.each { row[it] = 1 }
    rows << row
  end
  a = Matrix.columns(rows)
  b = Matrix.column_vector(jolts)
  [a.t,b]
end

#test = "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
#bits, nums = parse(test)
# printf "%d ^ %d = %b\n", bits, nums[-3], bits ^ nums[-3]
# bits ^= nums[-3]
# printf "%d ^ %d = %b\n", bits, nums[-2], bits ^ nums[-2]
# bits ^= nums[-2]
# printf "%d ^ %d = %b\n", bits, nums[-1], bits ^ nums[-1]
# bits ^= nums[-1]

def to_mathematica(a,b)
  if a.row_count != b.row_count
    puts "size mismatch: a#{[a.row_count, a.column_count]} b[#{b.row_count},1]"
    exit 1
  end
  xs = []
  a.column_count.times {|i| xs << "x#{i}"}
  puts "(* a#{[a.row_count, a.column_count]} * x[#{xs.length},1] = b[#{b.row_count},1] *)"
  print "vs = LinearOptimization["
  print xs.join("+")
  amper = ""
  print ",{"
  a.row_count.times do |r|
    cons = []
    a.column_count.times do |c|
      if a[r,c] == 1
        cons << xs[c]
      end
    end
    next if cons.empty?
    print amper
    amper = " && "
    print cons.join(" + ")
    print "==#{b[r,0]}"
  end
  xs.each do |v|
    print ",#{v}>=0"
  end
  print "},{"
  print xs.map { "#{it} \\[Element] Integers" }.join(",")
  puts "}]"
  puts "total += Plus @@ Values[vs]"
end

#puts "using LinearAlgebra"
puts "total = 0"
count1 = 0
count2 = 0
$stdin.each_line do |line|
  bits, nums, nums2, jolts = parse(line)
  count1 += solve1(bits, nums)
  
  #part 2
  a,b = to_matrix(nums2, jolts)
  to_mathematica(a.t, b)
end
puts "Print[total]"


#puts "part 1 = #{count1}"
#puts count2
