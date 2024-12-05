# Advent of Code
# Thursday, December 05, 2024

# not 4509

$rules = {}

def check(list)
  list.each_with_index do |p, j|
    if $rules.has_key? p
      list[j .. list.length-j].each do |z|
        if $rules[p].member? z
          return false
        end
      end
    end
  end
  true
end

def middle(list)
  j = list.length / 2
  list[j].to_i
end

def elf_sort(list, start)
  for i in start .. list.length-1
    a = list[i]
    if $rules.has_key? a
      for j in i+1 .. list.length-1
        if $rules[a].member? list[j]
          x = list[j]
          list[j,1] = []
          list[i,0] = x
          return elf_sort(list, i)
        end
      end
    end
  end
  list
end

# read rules
$stdin.each_line do |line|
  line.chomp!
  break if line == ''
  a,z = line.split('|')
  s = $rules.fetch(z,[])
  s << a
  $rules[z] = s
end

sum = 0
sum2 = 0
$stdin.each_line do |line|
  line.chomp!
  list = line.split(',')
  if check(list)
    sum += middle(list)
  else
    x = elf_sort list, 0
    sum2 += middle(x)
  end
end
puts sum
puts sum2
