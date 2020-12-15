use std::env;
use std::io::BufRead;

#[derive(Debug,Copy,Clone)]
enum Tile {
    Blank,
    Tree,
}

struct Pairs<I> {
    inner: I,
}

impl<I> Pairs<I> {
    fn new(inner: I) -> Self {
        Pairs { inner: inner, }
    }
}

impl<I> Iterator for Pairs<I>
where I: Iterator {
    type Item = (<I as Iterator>::Item, <I as Iterator>::Item);
    fn next(&mut self) -> Option<Self::Item> {
        let a = self.inner.next()?;
        let b = self.inner.next()?;
        Some((a,b))
    }
}

fn tile(c: char) -> Tile {
    match c {
        '.' => Tile::Blank,
        '#' => Tile::Tree,
        _ => panic!("Unrecognized tile")
    }
}

fn trees_hit(grid: &Vec<Vec<Tile>>, dx: usize, dy: usize) -> u64 {
    let mut count = 0;
    let mut x = 0;
    for row in grid.iter().step_by(dy) {
        match row[x % row.len()] {
            Tile::Blank => {},
            Tile::Tree => {count += 1;},
        };
        x += dx;
    }
    count
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let grid: Vec<Vec<Tile>> = std::io::BufReader::new(std::fs::File::open(&args[1]).unwrap())
        .lines()
        .map(Result::unwrap)
        .map(|l| l
             .chars()
             .map(tile)
             .collect()
             )
        .collect();
    let slopes: Vec<(usize,usize)> = Pairs::new(args[2]
        .split(",")
        .map(|s| s.parse::<usize>().unwrap())
        ).collect();

    println!("{}", slopes.iter()
             .map(|(dx,dy)| trees_hit(&grid, *dx, *dy))
             .product::<u64>()
             );
}
