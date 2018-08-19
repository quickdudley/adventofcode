use std::f64;
use std::i64;
use std::env;
use std::collections::HashMap;

fn spiral_ring(c: i64) -> i64 {
  let f = c as f64;
  ((f.sqrt() + 1.0) / 2.0).ceil() as i64
}

fn ring_diagonals(n: i64) -> [i64; 4] {
  let s = (n * 2 - 1).pow(2);
  let d = n - 1;
  [s - 6 * d, s - 4 * d, s - 2 * d, s]
}

fn spiral_coords(c: i64) -> (i64, i64) {
  let n = spiral_ring(c);
  let d = ring_diagonals(n);
  let md = n - 1;
  for i in [0,1,2,3].iter() {
    if d[*i] >= c {
      match *i {
        0 => return (md, c + md - d[*i]),
        1 => return (d[*i] - md - c, md),
        2 => return (-md, d[*i] - md - c),
        3 => return (md + c - d[*i], -md),
        _ => panic!("Index larger than array!")
      }
    }
  }
  panic!("Unreachable")
}

fn main() {
  let args : Vec<String> = env::args().collect();
  match i64::from_str_radix(&args[1], 10) {
    Result::Ok(c) => {
      let mut m = HashMap::new();
      m.insert((0,0),1);
      let mut i = 2;
      loop {
        let (x,y) = spiral_coords(i);
        let neighbours = [
          (x+1,y), (x+1,y+1), (x,y+1), (x-1,y+1),
          (x-1,y), (x-1,y-1), (x,y-1), (x+1,y-1)
        ];
        let mut sum = 0;
        for n in neighbours.iter() {
          sum += match m.get(n) {
            Some(ex) => *ex,
            None => 0
          }
        }
        m.insert((x,y),sum);
        if sum > c {
          println!("{}", sum);
          break;
        }
        i += 1;
      }
    },
    Result::Err(e) => println!("{}", e),
  }
}