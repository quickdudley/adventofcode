use std::convert::TryInto;
use std::env;

struct Ixs {
    ixs: Vec<usize>,
    max: usize,
}

impl Iterator for Ixs {
    type Item=Vec<usize>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut ixix = self.ixs.len() - 1;
        loop {
            let v = self.ixs[ixix] + 1;
            if v > self.max {
                if ixix == 0 {
                    return None;
                } else {
                    ixix = ixix - 1;
                }
            } else {
                for ixiy in ixix .. self.ixs.len() {
                    self.ixs[ixiy] = v;
                };
                return Some(self.ixs.clone())
            }
        }
    }
}

impl Ixs {
    fn new(count: usize, max: usize) -> Self {
        let mut r = Ixs { ixs: Vec::new(), max: max, };
        r.ixs.resize(count, 0);
        r
    }
}

fn find2020(i: &Vec<i64>, n: i64) -> i64 {
    for ixs in Ixs::new(n.try_into().unwrap(), i.len() - 1) {
        if ixs.iter().map(|ix| i[*ix] ).sum::<i64>() == 2020 {
            return ixs.iter().map(|ix| i[*ix] ).product();
        }
    }
    panic!("Not found!")
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let numbers: Vec<i64> = std::fs::read_to_string(&args[1])
        .unwrap()
        .split_whitespace()
        .map(|l| l.parse().unwrap())
        .collect();
    println!("{}", find2020(&numbers, args[2].parse().unwrap()));
}
