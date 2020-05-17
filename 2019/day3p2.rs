use std::env;
use std::iter::Peekable;
use std::hash::Hash;
use std::collections::HashMap;

#[derive(Clone,Copy,Debug,Eq,Hash,PartialEq)]
struct Point {
  x: i64,
  y: i64,
}

#[derive(Clone,Copy,Debug)]
enum Direction {
  U,
  R,
  D,
  L
}

#[derive(Clone,Copy,Debug)]
struct Segment {
  direction: Direction,
  length: i64,
}

impl core::ops::Add<Direction> for Point {
  type Output = Self;
  fn add(self, other: Direction) -> Self {
    match other {
      Direction::U => Point { x: self.x, y: self.y - 1 },
      Direction::R => Point { x: self.x + 1, y: self.y },
      Direction::D => Point { x: self.x, y: self.y + 1 },
      Direction::L => Point { x: self.x - 1, y: self.y },
    }
  }
}

struct Steps<I> {
  point: Point,
  segment: Segment,
  source: I,
}

impl<I: Iterator<Item=Segment>> Steps<I> {
  pub fn new(source: I) -> Self {
    Steps {
      point: Point {x: 0, y: 0},
      segment: Segment {direction: Direction::U, length: 0},
      source: source,
    }
  }
}

impl<I: Iterator<Item=Segment>> Iterator for Steps<I> {
  type Item = Point;
  fn next(&mut self) -> Option<Point> {
    if self.segment.length == 0 {
      match self.source.next() {
        None => None,
        Some(seg) => {
          self.segment = seg;
          self.next()
        },
      }
    } else {
      self.point = self.point + self.segment.direction;
      self.segment.length -= 1;
      Some(self.point)
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

fn main() {
  let args: Vec<_> = env::args().collect();
  let file = std::fs::read_to_string(&args[1])
    .expect("Could not read the file");
  let mut stream = SegmentIter::new(file.chars());
  let wires: [Vec<Segment>; 2] = {
    let stream = &mut stream;
    let wire0 = stream.collect();
    stream.next_line();
    let wire1 = stream.collect();
    [wire0,wire1]
  };
  let mut w0s = HashMap::new();
  for (i,p) in Steps::new(wires[0].iter().map(|x|*x)).enumerate() {
    if !w0s.contains_key(&p) {
      w0s.insert(p,i as u64);
    }
  }
  let mut md = u64::MAX;
  for (i,p) in Steps::new(wires[1].iter().map(|x|*x)).enumerate() {
    if let Some(i0) = w0s.get(&p) {
      let s = i0 + i as u64 + 2;
      if s < md {
        md = s;
      }
    }
  }
  println!("{:?}", md)
}
