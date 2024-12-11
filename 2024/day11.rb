# Advent of Code
# Wednesday, December 11, 2024

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  11   00:19:44   5389      0   01:19:20   5926      0


line = "510613 358 84 40702 4373582 2 0 1584".split
#line = "125 17".split

25.times do
  offset = 0
  line.length.times do |j|
    i = j + offset
    if line[i] == "0"
      line[i] = "1"
    elsif line[i].length.even?
      len = line[i].length
      a = line[i][0,len/2].sub(/^0+/, "")
      a = "0" if a == ""
      b = line[i][len/2,len].sub(/^0+/, "")
      b = "0" if b == ""
      line[i,1] = [a,b]
      offset  += 1
    else
      x = line[i].to_i * 2024
      line[i] = x.to_s
    end
  end
end

puts line.length
