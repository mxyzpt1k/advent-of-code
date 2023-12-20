# Day 20: Pulse Propagation
# Advent of Code 2023
# Wednesday, December 20, 2023

require 'set'

$tick = 0
$machine = {}
$upstream = {}
$counts = {:low => 0, :high => 0}
$button_press = 0

class Message
  attr_accessor :tick, :pulse, :from
  def initialize(tick, pulse, from)
    @tick = tick
    @pulse = pulse
    @from = from
  end
end

class Module
  attr_accessor :name, :queue
  def initialize(name, downstream)
    @name = name
    @downstream = downstream
    downstream.each do |down|
      $upstream[down] = Set.new unless $upstream.fetch(down, false)
      $upstream[down] << name
    end
    @queue = []
  end
  
  def send(pulse)
    @downstream.each do |dest|
      mod = $machine.fetch(dest, nil)
      if mod.nil?
        mod = Unspecified.new(dest,[])
        $machine[dest] = mod
      end
      mod.queue << Message.new($tick+1, pulse, @name)
      #puts "#{@name} -#{pulse} -> #{dest}"
      $counts[pulse] += 1
    end
  end

  def active?
    #puts "#{@name} => #{@queue}"
    if @queue.empty?
      false
    elsif @queue[0].tick == $tick
      true
    elsif @queue[0].tick < $tick
      raise "out of sync in #{@name}"
    else
      false
    end
  end

  def activate
    @queue.shift if active?
  end
end

class Broadcaster < Module
  def activate
    if active?
      @queue.shift
      send :low
    end
  end
end

class Conjunction < Module
  def init_mem
    @mem = {}
    $upstream[@name].each do |up|
      @mem[up] = :low
    end
  end
  
  def activate
    init_mem if @mem.nil?
    return unless active?
    message = @queue.shift
    @mem[message.from] = message.pulse
    if @mem.values.all? :high
      send :low
    else
      send :high
    end
  end
end

class FlipFlop < Module
  attr_accessor :state
  
  def flipflop
    if @state == :off
      @state = :on
      send :high
    else
      @state = :off
      send :low
    end
  end

  def activate
    return unless active?
    message = @queue.shift
    if message.pulse == :low
      flipflop
    end
  end
end

class Unspecified < Module
  def activate
    if @name == 'rx' and active?
      msg = @queue.shift
      if msg.pulse == :low
        raise "Done at #{$button_press}"
      end
    end
  end

  def upstream_count
    sum = 0
    $upstream[@name].each do |up|
      unless up == @name        # not sure if this happens
        sum += $machine[up].upstream_count
      end
    end
    sum
  end
end

ff = 0
cc = 0
ARGF.each do |line|
  (src, junk, *downstream) = line.delete(?,).split
  if src.start_with? '%'
    m = FlipFlop.new(src[1..], downstream)
    m.state = :off
    ff += 1
  elsif src.start_with? '&'
    m = Conjunction.new(src[1..], downstream)
    cc += 1
  elsif src == "broadcaster"
    m = Broadcaster.new(src, downstream)
  else
    raise "Unknown type #{src}"
  end
  $machine[ m.name ] = m
end

def button_push
  $button_press += 1
  $counts[:low] += 1
  b = $machine["broadcaster"]
  b.queue << Message.new($tick, :low, "button")
  while $machine.values.any? &:active?
    #puts "tick: #{$tick}, active modules: #{$machine.values.count &:active?}"
    $machine.values.each do |m|
      while m.active?
        m.activate
      end
    end
    $tick += 1
  end
end

def part1
  1000.times { button_push }
  puts $counts[:low] * $counts[:high]
end

part1

# part 2
#puts "FlipFlops = #{ff}, Conjunction = #{cc}"
# FlipFlops = 48, Conjunction = 9
# 48^9 = 1352605460594688 is too high
