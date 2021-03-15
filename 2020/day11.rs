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

fn build_neighbourhoods_v1(grid: &Vec<Vec<LobbyPoint>>) -> Vec<Vec<Vec<(usize,usize)>>> {
    grid.into_iter().enumerate().map(|(y,row)|
        row.into_iter().enumerate().map(move |(x,lp)|
            match lp {
                LobbyPoint::Floor => vec![],
                _ => neighbours(x,y).flat_map(|(xn,yn)|
                    usize::try_from(yn).ok().and_then(|yn|
                        grid.get(yn).and_then(|row|
                            usize::try_from(xn).ok().and_then(|xn|
                                row.get(xn).and_then(|lp|
                                    match lp {
                                        LobbyPoint::Floor => None,
                                        _ => Some((xn,yn)),
                                    }
                                )
                            )
                        )
                    ).into_iter()
                ).collect(),
            }
        ).collect()
    ).collect()
}

fn seek_line(x0: usize, y0: usize, xd: isize, yd: isize, grid: &Vec<Vec<LobbyPoint>>) -> Option<(usize,usize)> {
    let x = x0.wrapping_add(xd as usize);
    let y = y0.wrapping_add(yd as usize);
    grid.get(y).and_then(|row| row.get(x)).and_then(|lp| match lp {
        LobbyPoint::Floor => seek_line(x, y, xd, yd, grid),
        _ => Some((x,y))
    })
}

fn neighbours_v2<'a>(xc: usize, yc: usize, grid: &'a Vec<Vec<LobbyPoint>>) -> impl Iterator<Item=(usize,usize)> + 'a {
    (-1 .. 2).flat_map(move |y| (-1 .. 2).map(move |x| (x,y)))
        .filter(|p| *p != (0,0))
        .flat_map(move |(x,y)| seek_line(xc,yc,x,y,grid).into_iter())
}

fn build_neighbourhoods_v2(grid: &Vec<Vec<LobbyPoint>>) -> Vec<Vec<Vec<(usize,usize)>>> {
    grid.into_iter().enumerate().map(|(y,row)|
        row.into_iter().enumerate().map(move |(x,lp)|
            match lp {
                LobbyPoint::Floor => vec![],
                _ => neighbours_v2(x,y,grid).collect(),
            }
        ).collect()
    ).collect()
}

fn time_step(grid: &Vec<Vec<LobbyPoint>>, neighbourhoods: &Vec<Vec<Vec<(usize,usize)>>>, threshold: u16) -> Vec<Vec<LobbyPoint>> {
    grid.into_iter().zip(neighbourhoods.into_iter()).map(|(gr,nr)|
        gr.into_iter().zip(nr.into_iter()).map(|(lp,n)| match lp {
            LobbyPoint::Floor => LobbyPoint::Floor,
            _ => {
                let c: u16 = n.into_iter().map(|(x,y)| match grid[*y][*x] {
                    LobbyPoint::Full => 1,
                    _ => 0,
                }).sum();
                match lp {
                    LobbyPoint::Full => if c >= threshold {
                        LobbyPoint::Empty
                    } else {
                        LobbyPoint::Full
                    },
                    LobbyPoint::Empty => if c == 0 {
                        LobbyPoint::Full
                    } else {
                        LobbyPoint::Empty
                    },
                    _ => LobbyPoint::Floor,
                }
            },
        }).collect()
    ).collect()
}

fn parse_grid(src: &str) -> Vec<Vec<LobbyPoint>> {
    src.lines().map(|l| l.chars().map(LobbyPoint::from_char).collect()).collect()
}

fn main() {
    let args: Vec<_> = env::args().collect();
    let mut grid = parse_grid(std::fs::read_to_string(&args[1]).unwrap().as_ref());
    let (neighbourhoods, threshold) = match args[2].as_ref() {
        "1" => (build_neighbourhoods_v1(&grid),4),
        "2" => (build_neighbourhoods_v2(&grid),5),
        _ => panic!("Version number (1 or 2) expected")
    };
    loop {
        let grid2 = time_step(&grid, &neighbourhoods, threshold);
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

