# Advent of Code
# Saturday, December 14, 2024

# this is part 2

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  14   01:25:41   7153      0   10:23:14  18938      0

$bots = []

#$height = 7
#$width = 11

$height = 103
$width = 101

class Grid
  attr_accessor :count, :rows, :cols
  
  def initialize(rows, cols)
    @grid = []
    @rows = rows
    @cols = cols
    rows.times do
      @grid << [?.,] * cols
    end
    @count = 0
  end

  def place(bot)
    @grid[bot.y][bot.x] = ?@
  end

  def erase(bot)
    @grid[bot.y][bot.x] = ?.
  end
            
  def draw
    puts @count
    @grid.each do |row|
      puts row.join('')
    end
  end

  def bot?(row,col)
    ?@ == @grid[row][col]
  end

  def metric
    # how many grid points have neighbors
    count = 0
    @rows.times do |y|
      @cols.times do |x|
        n = 0
        if 0 < y and y < $height-1
          if 0 < x and x < $width-1
            n += 1 if bot?( y+1, x )
            n += 1 if bot?( y-1, x )
            n += 1 if bot?( y, x+1 )
            n += 1 if bot?( y, x-1 )
          end
        end
        count += n
      end
    end
    count
  end
end

class Robot
  attr_accessor :x, :y
  
  def initialize(x,y,vx,vy)
    @x = x
    @y = y
    @dx = vx
    @dy = vy
  end

  def step(size=1)
    @x = (@x + @dx*size) % $width
    @y = (@y + @dy*size) % $height
  end
end

$stdin.each_line do |line|
  if line =~ /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/
    x = $1.to_i
    y = $2.to_i
    vx = $3.to_i
    vy = $4.to_i
    $bots << Robot.new(x,y,vx,vy)
  end
end

grid = Grid.new($height, $width)
step_size = 1
best = -1
candidates = 0
(101*103).times do
  $bots.each { |b| grid.erase b }
  $bots.each do |bot|
    bot.step step_size
    grid.place bot
  end
  grid.count += step_size
  m = grid.metric
  if m > best
    grid.draw
    candidates += 1
    puts candidates
    best = m
  end
end
