# Advent of Code
# Wednesday, December 11, 2024

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  11   00:19:44   5389      0   01:19:20   5926      0

class Word
  attr_accessor :word, :count
  
  def initialize(word,count=1)
    @word = word
    @count = count
  end

  def zero?
    "0" == @word
  end

  def multiply
    @word = (@word.to_i * 2024).to_s
  end

  def length
    @word.length
  end

  def split
    a = @word[0,length/2].sub(/^0+/, "")
    a = "0" if a == ""
    b = @word[length/2,length].sub(/^0+/, "")
    b = "0" if b == ""
    [Word.new(a,count),Word.new(b,count)]
  end
end

def compact(words)
  seen = {}
  words.each do |w|
    seen[w.word] = seen.fetch(w.word,0) + w.count
  end
  seen.keys.map {|w| Word.new(w, seen[w])}
end

def blink(words)
  offset = 0
  words.length.times do |j|
    i = j + offset
    if words[i].zero?
      words[i].word = "1"
    elsif words[i].length.even?
      words[i,1] = words[i].split
      offset += 1
    else
      words[i].multiply
    end
  end
  compact words
end

line = "510613 358 84 40702 4373582 2 0 1584".split
#line = "125 17".split

words = line.map { |w| Word.new(w, 1) }
75.times do
  words = blink words
end

puts words.map {|w| w.count}.sum
