//use std::io;
use std::f64;

fn spiral_ring(c: i64) -> i64 {
  let f = c as f64;
  ((f.sqrt() + 1.0) / 2.0).ceil() as i64
}

fn ring_open(n: i64) -> i64 {
  ring_d4(n - 1) + 1
}

fn ring_d4(n: i64) -> i64 {
  (n * 2 - 1).pow(2)
}

fn ring_diagonals(n: i64) -> (i64, i64, i64, i64) {
  let s = (n * 2 - 1).pow(2);
  let d = n - 1;
  (s - 6 * d, s - 4 * d, s - 2 * d, s)
}

fn mid_dist(n: i64) -> i64 {
  n - 1
}

fn main() {
  println!("{}", ring_open(3));
}