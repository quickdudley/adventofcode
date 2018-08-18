
def redist banks
  fullest, index1 = banks.each_with_index.max{|a,b| a[0] <=> b[0] }
  inc1 = fullest / banks.length
  inc2 = inc1 + 1
  ex = fullest % banks.length - 1
  banks.each_with_index.map do |s,index|
    inc = if (index - index1 - 1) % banks.length <= ex
      inc2
    else
      inc1
    end
    c = if index == index1
      0
    else
      s
    end
    inc + c
  end
end

def the_cycle banks
  Enumerator.new do |y|
    cache = {}
    while true
      cache[banks] ||= 0
      case cache[banks]
      when 1
        y.yield banks
      when 2
        break
      end
      cache[banks] += 1
      banks = redist banks
    end
  end
end


initial_state = Enumerator.new do |y|
  File.open(ARGV[0],"r") do |src|
    src.each_line do |line|
      line.split(/\s/).each do |word|
        num = word.to_i
        y.yield num if num
      end
    end
  end
end.to_a

puts the_cycle(initial_state).count
