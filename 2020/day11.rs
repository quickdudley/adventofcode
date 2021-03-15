use std::convert::TryFrom;
use std::env;

#[derive(PartialEq,Eq)]
enum LobbyPoint {
    Floor,
    Full,
    Empty,
}

impl LobbyPoint {
    fn from_char(src: char) -> Self {
        match src {
            'L' => Self::Empty,
            '#' => Self::Full,
            _ => Self::Floor,
        }
    }
}

fn neighbours(xc: usize, yc: usize) -> impl Iterator<Item=(isize,isize)> {
    (-1 .. 2).flat_map(|y| (-1 .. 2).map(move |x| (x,y))).filter(|p| *p != (0,0))
        .map(move |(x,y)| (x + xc as isize, y + yc as isize))
}

fn time_step(grid: &Vec<Vec<LobbyPoint>>) -> Vec<Vec<LobbyPoint>> {
    grid.into_iter().enumerate().map(|(y,row)|
        row.into_iter().enumerate().map(|(x,lp)| match lp {
            LobbyPoint::Floor => LobbyPoint::Floor,
            _ => {
                let nc: usize = neighbours(x,y).map(|(uxn,uyn)| usize::try_from(uyn).ok().
                    and_then(|yn| usize::try_from(uxn).ok().and_then(move |xn|
                        if yn >= grid.len() || xn >= grid[yn].len() {
                            Some(0)
                        } else {
                            match grid[yn][xn] {
                                LobbyPoint::Full => Some(1),
                                _ => Some(0),
                            }
                        }
                    )).unwrap_or(0)
                ).sum();
                match lp {
                    LobbyPoint::Full => if nc >= 4 { LobbyPoint::Empty } else { LobbyPoint::Full },
                    LobbyPoint::Empty => if nc == 0 { LobbyPoint::Full } else { LobbyPoint::Empty },
                    _ => LobbyPoint::Floor,
                }
            }
        }).collect()
    ).collect()
}

fn parse_grid(src: &str) -> Vec<Vec<LobbyPoint>> {
    src.lines().map(|l| l.chars().map(LobbyPoint::from_char).collect()).collect()
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut grid = parse_grid(std::fs::read_to_string(&args[1]).unwrap().as_ref());
    loop {
        let grid2 = time_step(&grid);
        if grid2 == grid {
            break;
        } else {
            grid = grid2;
        }
    };
    println!("{}", grid.into_iter().flat_map(|row| row.into_iter().map(|pt| match pt {
        LobbyPoint::Full => 1,
        _ => 0,
    })).sum::<usize>())
}

