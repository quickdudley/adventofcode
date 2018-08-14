use std::f64;
use std::i64;
use std::env;

fn spiral_ring(c: i64) -> i64 {
  let f = c as f64;
  ((f.sqrt() + 1.0) / 2.0).ceil() as i64
}

fn ring_diagonals(n: i64) -> [i64; 4] {
  let s = (n * 2 - 1).pow(2);
  let d = n - 1;
  [s - 6 * d, s - 4 * d, s - 2 * d, s]
}

fn spiral_cab_dist(c: i64) -> i64 {
  let n = spiral_ring(c);
  let d = ring_diagonals(n);
  let md = n - 1;
  for x in d.iter() {
    if *x >= c {
      return md + (x - md - c).abs();
    }
  }
  // Compiler probably thinks this is reachable.
  0
}

fn main() {
  let args : Vec<String> = env::args().collect();
  match i64::from_str_radix(&args[1], 10) {
    Result::Ok(c) => println!("{}", spiral_cab_dist(c)),
    Result::Err(e) => println!("{}", e),
  }
}