#!/usr/bin/ruby
# Advent of Code
# Sunday, December 15, 2024

#       --------Part 1--------   --------Part 2--------
# Day       Time   Rank  Score       Time   Rank  Score
#  15   00:57:01   4733      0   11:03:03  12804      0

require 'set'

class Grid
  def initialize
    @grid = []
    @copy = []
    @y = -1
    @x = -1
    @pc = 0
  end

  def <<(chars)
    @grid << chars
    @copy << chars.clone
    chars.each_with_index do |c,i|
      if c == ?@
        @x = i
        @y = @grid.length - 1
      end
    end
  end

  def display
    @grid.each do |row|
      puts row.join('')
    end
    puts score
  end

  def write_image
    path = "animate15/step_#{10000+@pc}.ppm"
    box = '10 10 15 '
    wall = '0 0 0 '
    color = {
      '#' => wall,
      '@' => '15 0 0 ',
      '.' => '15 15 15 ',
      '[' => box,
      ']' => box
    }
    open(path,"w") do |fh|
      fh.puts "P3"
      fh.puts "#{@grid[0].length} #{@grid.length}"
      fh.puts "16"
      @grid.each do |row|
        row.each do |c|
          fh.print color[c]
        end
        fh.print "\n"
      end
    end
  end

  def box?(y,x)
    box_left?(y,x) or box_right?(y,x)
  end

  def box_left?(y,x)
    @grid[y][x] == '['
  end

  def box_right?(y,x)
    @grid[y][x] == ']'
  end

  def open?(y,x)
    @grid[y][x] == '.'
  end

  def wall?(y,x)
    @grid[y][x] == '#'
  end

  def move_x(x,dx)
    xx = x + dx
    if wall? @y, xx
      false
    else
      if box? @y, xx
        move_x xx, dx
      end
      if open? @y, xx
        @grid[@y][xx] = @grid[@y][x]
        @grid[@y][x] = '.'
        true
      else
        false
      end
    end
  end

  def move_y(y,dy,xs)
    if xs.empty?
      return true
    end
    needed = Set.new
    yy = y + dy
    xs.each do |x|
      if wall? yy, x
        return false
      elsif box_left? yy, x
        needed << x
        needed << (x + 1)
      elsif box_right? yy, x
        needed << (x - 1)
        needed << x
      elsif open? yy, x
        :ok
      else
        puts "what is that?  y=#{yy}, x=#{x} => #{@grid[yy][x]}"
        exit
      end
    end
    if move_y(yy,dy,needed)
      xs.each do |x|
        @grid[yy][x] = @grid[y][x]
        @grid[y][x] = '.'
      end
    end
  end

  def run(prog)
    prog.each_with_index do |c,i|
      #write_image
      @pc += 1
      if @grid[@y][@x] != '@'
        puts "unexpected piece at y=#{@y}, x=#{@x} :: #{@grid[@y][@x]}"
        display
        exit
      end
      case c
      when '^'
        @y -= 1 if move_y(@y, -1, [@x])
      when 'v'
        @y += 1 if move_y(@y, 1, [@x])
      when '>'
        @x += 1 if move_x(@x, 1)
      when '<'
        @x -= 1 if move_x(@x, -1)
      end
    end
  end

  def score
    n = 0
    @grid.each_with_index do |row, y|
      row.length.times do |x|
        if box_left? y, x
          n += (y * 100) + x
        end
      end
    end
    n
  end
end

$double = {
  '#' => '##',
  '.' => '..',
  '@' => '@.',
  'O' => '[]'
}

open(ARGV[0]) do |fh|
  grid = Grid.new
  map_done = false
  fh.each_line do |line|
    line.chomp!
    if map_done
      grid.run( line.chars )
    elsif line.empty?
      map_done = true
    else
      line.gsub!(/(.)/) { $double[$1] }
      grid << line.chomp.chars
    end
  end
  puts grid.score
  #grid.write_image
end
