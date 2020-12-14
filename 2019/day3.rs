use std::env;
use std::iter::Peekable;

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
struct Point {
  x: i64,
  y: i64,
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
enum Direction {
  U,
  R,
  D,
  L
}

#[derive(Clone)]
#[derive(Copy)]
#[derive(Debug)]
struct Segment {
  direction: Direction,
  length: i64,
}

impl core::ops::Add<&Segment> for Point {
  type Output = Self;
  fn add(self, other: &Segment) -> Self {
    match other.direction {
      Direction::U => Point { x: self.x, y: self.y - other.length },
      Direction::R => Point { x: self.x + other.length, y: self.y },
      Direction::D => Point { x: self.x, y: self.y + other.length },
      Direction::L => Point { x: self.x - other.length, y: self.y },
    }
  }
}

struct Pairwise<I: Iterator> {
  last: Option<I::Item>,
  inner: I,
}

impl<I: Iterator> Pairwise<I> {
  fn new(inner: I) -> Self {
    let mut inner = inner;
    Pairwise {
      last: inner.next(),
      inner: inner,
    }
  }
}

impl<I: Iterator> Iterator for Pairwise<I>
 where
  I::Item: Copy
 {
  type Item = (I::Item,I::Item);
  fn next(&mut self) -> Option<Self::Item> {
    self.last.and_then(|last|
      self.inner.next().and_then(|fresh| {
        self.last = Some(fresh);
        Some((last,fresh))
      }).or_else(|| {
        self.last = None;
        None
      })
    )
  }
}

struct Accumulator<I, R> {
  sum: Option<R>,
  inner: I,
}

impl<I, R> Accumulator<I,R> {
  pub fn new(initial: R, inner: I) -> Self {
    Accumulator{
      sum: Some(initial),
      inner: inner,
    }
  }
}

impl<I,R> Iterator for Accumulator<I,R> where
  I: Iterator,
  R: core::ops::Add<I::Item, Output=R> + Copy,
 {
  type Item = R;
  fn next(&mut self) -> Option<R> {
    match self.sum {
      None => None,
      Some(s) => {
        self.sum = match self.inner.next() {
          Some(a) => Some(s + a),
          None => None,
        };
        Some(s)
      },
    }
  }
}
fn next_direction<I: Iterator<Item=char>>(i: &mut Peekable<I>) -> Option<Direction> {
  i.peek().and_then(|c| match c {
    'U' => Some(Direction::U),
    'R' => Some(Direction::R),
    'D' => Some(Direction::D),
    'L' => Some(Direction::L),
    _ => None,
  }).map(|d| {
    i.next();
    d
  })
}

struct SegmentIter<I: Iterator<Item=char>> { inner: Peekable<I> }

impl<I: Iterator<Item=char>> SegmentIter<I> {
  fn new(original: I) -> Self {
    SegmentIter { inner: original.peekable(), }
  }

  fn next_line(&mut self) {
    if self.inner.peek().map_or(false,|c| *c == '\n') {
      self.inner.next();
    }
  }
}

impl<I: Iterator<Item=char>> Iterator for SegmentIter<I> {
  type Item = Segment;

  fn next(&mut self) -> Option<Segment> {
    let r = next_direction(&mut self.inner)
      .and_then(|d| next_number(&mut self.inner).map(|s| Segment {
        direction: d,
        length: s,
      }));
    if r.is_some() && self.inner.peek().map_or(false, |c| *c == ',') {
      self.inner.next();
    }
    r
  }
}

fn next_number<I: Iterator<Item=char>>(i: &mut Peekable<I>) -> Option<i64> {
  i.peek()
    .and_then(|c| c.to_digit(10))
    .and_then(|d| {
      i.next();
      let mut d = d as i64;
      loop {
        match i.peek().and_then(|c| c.to_digit(10)) {
          Some(d2) => {
            d = d * 10 + d2 as i64;
            i.next();
          },
          None => {
            break Some(d);
          },
        }
      }
    })
}


fn intersection(a: (Point,Point), b: (Point,Point)) -> Option<Point> {
  let mut a = a;
  let mut b = b;
  if a.0.x > a.1.x || a.0.y > a.1.y {
    a = (a.1, a.0);
  }
  if b.0.x > b.1.x || b.0.y > b.1.y {
    b = (b.1, b.0);
  }
  if a.0.x != a.1.x {
    if b.0.x == b.1.x {
      let t = b;
      b = a;
      a = t;
    } else {
      return None;
    }
  }
  if b.0.y != b.1.y ||
     b.0.y < a.0.y ||
     b.0.y > a.1.y ||
     a.0.x < b.0.x ||
     a.0.x > b.1.x {
    None
  } else {
    Some(Point{x: a.0.x, y: b.0.y})
  }
}

fn main() {
  let args: Vec<_> = env::args().collect();
  let file = std::fs::read_to_string(&args[1])
    .expect("Could not read the file");
  let mut stream = SegmentIter::new(file.chars());
  let mut wires = [Vec::new(), Vec::new()];
  for i in 0 ..= 1 {
    while let Some(segment) = stream.next() {
      wires[i].push(segment);
    }
    stream.next_line();
  }
  let wires: [_; 2] = {
    let mut w: [Vec<(Point,Point)>; 2] = [Vec::new(), Vec::new()];
    for (dst, wl) in w.iter_mut().zip(wires.iter()) {
      dst.extend(Pairwise::new(Accumulator::new(Point{x:0,y:0},wl.into_iter())));
    }
    w
  };
  let mut nd = i64::MAX;
  let mut ai = wires[0].iter().enumerate();
  while let Some((aix,a)) = ai.next() {
    let mut bi = wires[1].iter();
    if aix == 0 { bi.next(); }
    while let Some(b) = bi.next() {
      match intersection(*a,*b) {
        None => {},
        Some(p) => {
          let d = p.x.abs() + p.y.abs();
          if d < nd {
            nd = d;
          }
        }
      }
    }
  }
  println!("{}", nd);
}
