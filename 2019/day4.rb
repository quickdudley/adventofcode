def pairwise e
  Enumerator.new do |y|
    a = e.next
    while true
      b = e.next
      y.yield a,b
      a = b
    end
  rescue StopIteration
  end
end

def next_d(d,c)
  d == true || d == c || c
end

def build_candidates(d,s)
  Enumerator.new do |y|
    if s.length == 0
      y.yield [] if d == true
    else
      r = s[0]
      Enumerator.new do |y2|
        (r.begin .. r.end).each do |c|
          y2.yield(c, s[1 ... s.length].map do |m|
            (c != r.begin ? c : [c, m.begin].max) ..
            (c == r.end ? m.end : '9')
          end)
        end
      end.each do |c,sn|
        build_candidates(next_d(d,c), sn).each do |t|
          y.yield([c] + t)
        end
      end
    end
  end
end

def solve(a,b)
  build_candidates(false,a.to_s.chars.zip(b.to_s.chars).map do |l,h|
    l .. h
  end).lazy.map{|r| r.join("").to_i }
end

# puts solve(ARGV[0], ARGV[1]).count
