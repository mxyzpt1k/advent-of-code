# November 2025
# Advent of Code 2016

class Series15
  def initialize(a,b)           # ax + b
    @a = a
    @b = b
    @x = 0
  end

  def value
    @a * @x + @b
  end

  def incr_to(n)
    while n > value
      @x += 1
    end
  end
end

series = [
  Series15.new(11,4),           # added for part 2
  Series15.new(5,2),
  Series15.new(13,4),
  Series15.new(17,4),
  Series15.new(3,0),
  Series15.new(19,5),
  Series15.new(7,1)
]

max = 1
loop do
  series.each do |s|
    s.incr_to max if s.value < max
    max = [s.value, max].max
  end
  break if series.all? {|s| s.value == max}
end  
puts max
