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

fn ring_d3(n: i64) -> i64 {
  (n * 2 - 1).pow(2) - 2 * (n - 1)
}

fn ring_d2(n: i64) -> i64 {
  (n * 2 - 1).pow(2) - 4 * (n - 1)
}

fn ring_d1(n: i64) -> i64 {
  (n * 2 - 1).pow(2) - 6 * (n - 1)
}

fn mid_dist(n: i64) -> i64 {
  n - 1
}

fn main() {
  println!("{}", ring_open(3));
}